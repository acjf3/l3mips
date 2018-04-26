---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- utils functions

word flip_endian_word (w::word) =
{
   'a`8 b`8 c`8 d' = w;
   return (d : c : b : a)
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
    PadLeft (#" ", 9, [memAccessStats.bytes_read])  : "\n" :
    PadRight (#" ", 16, "bytes_written") : " = " :
    PadLeft (#" ", 9, [memAccessStats.bytes_written])

string csvHeaderMemAccessStats = "bytes_read,bytes_written"

string csvMemAccessStats =
  [memAccessStats.bytes_read] : "," : [memAccessStats.bytes_written]

-- watch paddr

declare watchPaddr :: pAddr option

unit watchForLoad (addr::pAddr, data::dword) =
  match watchPaddr
  {
     case Some (watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
         mark_watcher("watching --> load " : hex (data) : " from " : hex (addr))
     case None => nothing
  }

unit watchForStore (addr::pAddr, data::dword, mask::dword) =
  match watchPaddr
  {
     case Some (watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
         mark_watcher("watching --> Store " : hex (data) : "(mask:" :
                  hex (mask) : ") at " : hex (addr))
     case None => nothing
  }

-- virtual address computation
vAddr getVirtualAddress (addr::bits(64)) = addr

-- Primitive memory load (with memory-mapped devices)

dword LoadMemory
  ( MemType::bits(3), AccessLength::bits(3), needAlign::bool,
    vAddr::vAddr, link::bool ) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    mark_log (2, "Bad load, CP0.BadVAddr <-" : hex (vAddr));
    CP0.BadVAddr <- vAddr;
    SignalException (AdEL);
    return UNKNOWN(next_unknown("mem-data"))
  }
  else
  {
    pAddr_, c = AddressTranslation (vAddr, LOAD);
    if exceptionSignalled then return UNKNOWN(next_unknown("mem-data")) else
    {
      var pAddr = AdjustEndian (MemType, pAddr_);
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
        {
          id = [core];
          base = PIC_base_address(id);
          when base <=+ a and a <+ base + 1072 do
          {
            found <- true;
            ret <- PIC_load(id, a)
          }
        };
        when not found do ret <- ReadData (a)
      };
      if link then
      {
        when c == 2 do
          #UNPREDICTABLE("load linked on uncached address");
        LLbit <- Some (true);
        CP0.LLAddr <- [pAddr]
      }
      else
        LLbit <- None;
      b = [MemType] + 0n1;
      memAccessStats.bytes_read <- memAccessStats.bytes_read + b;
      mark_log (2, "Load of " : [b] : " byte(s) from vAddr " : hex (vAddr));
      watchForLoad (pAddr, ret);
      return ret
    }
  }

-- Pimitive memory store. Big-endian.

bool StoreMemory
   (MemType::bits(3), AccessLength::bits(3), needAlign::bool, MemElem::dword,
    vAddr::vAddr, cond::bool) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    mark_log (2, "Bad store, CP0.BadVAddr <-" : hex (vAddr));
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
      sc_success <- not cond or
        match LLbit
        {
          case None => #UNPREDICTABLE ("conditional store: LLbit not set")
          case Some (false) => false
          case Some (true) =>
            CP0.LLAddr == [pAddr] or
            #UNPREDICTABLE
               ("conditional store: address doesn't match previous LL address")
        };
      a = pAddr<39:3>;
      b = [AccessLength] + 0n1;
      l = 64 - (b + [vAddr<2:0>]) * 0n8;
      mask`64 = 1 << (l + b * 0n8) - 1 << l;
      if a == JTAG_UART.base_address then
        JTAG_UART_store (mask, MemElem)
      else
      {
        var found = false;
        for core in 0 .. totalCore - 1 do
        {
           id = [core];
           base = PIC_base_address (id);
           when base <=+ a and a <+ base + 1072 do
           {
              found <- true;
              PIC_store(id, a, mask, MemElem)
           }
         };
         when not found and sc_success do
         {
            for core in 0 .. totalCore - 1 do
            {   id = [core];
                st = all_state (id);
                when id <> procID and st.c_LLbit == Some (true) and
                     st.c_CP0.LLAddr<39:3> == pAddr<39:3> do
                        all_state(id).c_LLbit <- Some (false)
            };
            WriteData(a, MemElem, mask)
         }
      };
      LLbit <- None;
      memAccessStats.bytes_written <- memAccessStats.bytes_written + b;
      mark_log (2, "Store " : hex (MemElem) : ", mask " : hex (mask) :
                   " (" : [b] : " byte(s)) at vAddr " : hex (vAddr));
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
      pc, _ = AddressTranslation (PC, LOAD);
      when not exceptionSignalled do currentInst <- Some (ReadInst (pc))
   }
   else
   {
      mark_log (2, "Bad IFetch, CP0.BadVAddr <-" : hex (PC));
      CP0.BadVAddr <- PC;
      SignalException (AdEL)
   }
}
