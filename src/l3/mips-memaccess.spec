---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- utils functions

word flip_endian_word (w::word) =
{
   c, d = QuotRem ([[w]::nat], 256);
   b, c = QuotRem (c, 256);
   a, b = QuotRem (b, 256);
   return ([d]`8 : [c]`8 : [b]`8 : [a]`8)
}

bool Aligned (vAddr::vAddr, MemType::bits(3)) = [vAddr] && MemType == 0

pAddr AdjustEndian (MemType::bits(3), pAddr::pAddr) =
  match MemType
  {
     case 0 => pAddr ?? [ReverseEndian^3]
     case 1 => pAddr ?? [ReverseEndian^2 : '0']
     case 3 => pAddr ?? [ReverseEndian : '00']
     case 7 => pAddr
     case _ => #UNPREDICTABLE ("bad access length")
  }

-----------------
-- stats utils --
-----------------

record MemAccessStats
{
    bytes_read    :: nat
    bytes_written :: nat
}

declare memAccessStats :: MemAccessStats

unit initMemAccessStats =
{
    memAccessStats.bytes_read    <- 0;
    memAccessStats.bytes_written <- 0
}

string printMemAccessStats =
    PadRight (#" ", 16, "bytes_read")    : " = " :
    PadLeft (#" ", 9, [memAccessStats.bytes_read])  : "\\n" :
    PadRight (#" ", 16, "bytes_written") : " = " :
    PadLeft (#" ", 9, [memAccessStats.bytes_written])
string csvHeaderMemAccessStats = "bytes_read,bytes_written"
string csvMemAccessStats = [memAccessStats.bytes_read] : "," : [memAccessStats.bytes_written]

-- watch paddr

declare watchPaddr :: pAddr option

unit watchForLoad (addr::pAddr, data::dword) =
  match watchPaddr
  {
     case Some (watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
         println ("watching --> load 0x" : hex64(data) : " from 0x" :
                  hex40(addr))
     case None => nothing
  }

unit watchForStore (addr::pAddr, data::dword, mask::dword) =
  match watchPaddr
  {
     case Some (watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
         println ("watching --> Store 0x" : hex64(data) : "(mask:" :
                  hex64(mask) : ") at 0x" : hex40(addr))
     case None => nothing
  }

-- Pimitive memory load (with memory-mapped devices)

dword LoadMemory
  ( MemType::bits(3), AccessLength::bits(3), needAlign::bool,
    vAddr::vAddr, link::bool ) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    mark_log (2, "Bad load, CP0.BadVAddr <-" : hex64(vAddr));
    CP0.BadVAddr <- vAddr;
    SignalException (AdEL);
    return UNKNOWN
  }
  else
  {
    var pAddr = Fst (AddressTranslation (vAddr, LOAD));
    if not exceptionSignalled then
    {
      pAddr <- AdjustEndian (MemType, pAddr);
      -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
      a = pAddr<39:3>;
      var ret;
      if a == JTAG_UART.base_address then
      {
        ret <- flip_endian_word (JTAG_UART.&data) :
               flip_endian_word (JTAG_UART.&control);
        when pAddr<2:0> == 0 do JTAG_UART_load
      }
      else
      {
        var found = false;
        for core in 0 .. totalCore - 1 do
          when a >=+ PIC_base_address([core]) and
                a <+ PIC_base_address([core]) + 1072 do
          {
            found <- true;
            ret <- PIC_load([core], a)
          };
        when not found do ret <- ReadData (a)
      };
      if link then
      {
        LLbit <- Some (true);
        CP0.LLAddr <- [pAddr]
      }
      else
        LLbit <- None;
      b = [MemType] + 0n1;
      memAccessStats.bytes_read <- memAccessStats.bytes_read + b;
      mark_log (2, "Load of " : [b] : " byte(s) from vAddr 0x" : hex64(vAddr));
      watchForLoad (pAddr, ret);
      return ret
    }
    else return UNKNOWN
  }

-- Pimitive memory store. Big-endian.

bool StoreMemory
   (MemType::bits(3), AccessLength::bits(3), needAlign::bool, MemElem::dword,
    vAddr::vAddr, cond::bool) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    mark_log (2, "Bad store, CP0.BadVAddr <-" : hex64(vAddr));
    CP0.BadVAddr <- vAddr;
    SignalException (AdES);
    return false
  }
  else
  {
    var pAddr = Fst (AddressTranslation (vAddr, STORE));
    pAddr <- AdjustEndian (MemType, pAddr);
    -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
    var sc_success = true;
    when not exceptionSignalled do
    {
      sc_success <- if cond then
        match LLbit
        {
          case None => #UNPREDICTABLE ("conditional store: LLbit not set")
          case Some (false) => false
          case Some (true) =>
            if CP0.LLAddr == [pAddr] then
              true
            else
              #UNPREDICTABLE
                ("conditional store: address doesn't match previous LL address")
        }
        else true;
      a = pAddr<39:3>;
      b = [AccessLength] + 0n1;
      l = 64 - (b + [vAddr<2:0>]) * 0n8;
      mask`64 = [2 ** (l + b * 0n8) - 2 ** l];
      if a == JTAG_UART.base_address then
        JTAG_UART_store (mask, MemElem)
      else
      {
        var found = false;
        for core in 0 .. totalCore - 1 do
           when a >=+ PIC_base_address([core]) and
                 a <+ PIC_base_address([core]) + 1072 do
           {
              found <- true;
              PIC_store([core], a, mask, MemElem)
           };
         when not found and sc_success do
         {
            for core in 0 .. totalCore - 1 do
            {   i = [core];
                st = all_state (i);
                when i <> procID and st.c_LLbit == Some (true) and
                     st.c_CP0.LLAddr<39:3> == pAddr<39:3> do
                        all_state(i).c_LLbit <- Some (false)
            };
            WriteData(a, MemElem, mask)
         }
      };
      LLbit <- None;
      memAccessStats.bytes_written <- memAccessStats.bytes_written + b;
      mark_log (2, "Store 0x" : hex64(MemElem) : ", mask 0x" : hex64(mask) :
                   " (" : [b] : " byte(s)) at vAddr 0x" : hex64(vAddr));
      watchForStore(pAddr, MemElem, mask)
    };
    sc_success
  }

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

unit Fetch =
{
   currentInst <- None;

   CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired then
                           [TLBAssocEntries - 1]
                        else
                           CP0.Random.Random - 1;

   when CP0.Compare == CP0.Count do
   {
      CP0.Cause.IP<7> <- true
      -- TI not in MIPS R4000
      -- CP0.Cause.TI <- true
   };

   when CP0.Status.IE and not (CP0.Status.EXL or CP0.Status.ERL) do
   {
      -- If any interrupts pending, raise an exception
      when CP0.Status.IM<7:2> && CP0.Cause.IP<7:2> <> 0 do
        SignalException (Int)
   };

   if exceptionSignalled then
      nothing
   else if PC<1:0> == 0 then
   {
      pc, cca = AddressTranslation (PC, LOAD);
      when not exceptionSignalled do currentInst <- Some (ReadInst (pc))
   }
   else
   {
      mark_log (2, "Bad IFetch, CP0.BadVAddr <-" : hex64(PC));
      CP0.BadVAddr <- PC;
      SignalException (AdEL)
   }
}
