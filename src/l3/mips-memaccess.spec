---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- utils functions

word flip_endian_word (w::word) =
   match w { case 'a`8 b`8 c`8 d' => d : c : b : a }

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
    PadLeft (#" ", 9, [memAccessStats.bytes_written]) : "\\n"

-- watch paddr

declare watchPaddr::bits(40) option

unit watchForLoad (addr::bits(40), data::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:3> == watch_paddr<39:3> do
            println ("watching --> load 0x" : hex64(data) : " from 0x" :
                     hex40(addr))
    }
    case None => nothing
}

unit watchForStore (addr::bits(40), data::dword, mask::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
         println ("watching --> Store 0x" : hex64(data) : "(mask:" :
                  hex64(mask) : ") at 0x" : hex40(addr))
    case None => nothing
}

-- Pimitive memory load (with memory-mapped devices)

dword LoadMemory (MemType::bits(3), AccessLength::bits(3), vAddr::vAddr,
                  IorD::IorD, AccessType::AccessType, link::bool) =
{
    var pAddr;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match MemType
    {
        case 0 => (pAddr<2:0> ?? ReverseEndian^3)
        case 1 => (pAddr<2:0> ?? (ReverseEndian^2 : '0'))
        case 3 => (pAddr<2:0> ?? (ReverseEndian : '00'))
        case 7 =>  pAddr<2:0>
        case _ => #UNPREDICTABLE ("bad access length")
    };
    pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
    if not exceptionSignalled then
    {
        a = pAddr<39:3>;
        var ret;

        var found = false;
        if a == JTAG_UART.base_address then
        {
            found <- true;
            ret <- flip_endian_word (JTAG_UART.&data) :
                   flip_endian_word (JTAG_UART.&control);
            when pAddr<2:0> == 0 do JTAG_UART_load
        }
        else for core in 0 .. totalCore - 1 do
            when a >=+ PIC_base_address([core]) and
                  a <+ PIC_base_address([core]) + 1072 do
            {
                found <- true;
                ret <- PIC_load([core], a)
            };

        if link then
        {
            LLbit <- Some (true);
            CP0.LLAddr <- [pAddr]
        }
        else
            LLbit <- None;

        when not found do
            ret <- ReadData (a);

        memAccessStats.bytes_read <- memAccessStats.bytes_read + [[MemType]::nat+1];
        when 2 <= trace_level do
           mark_log (2, "Load of " : [[MemType]::nat + 1] :
                        " byte(s) from vAddr 0x" : hex64(vAddr));

        watchForLoad(pAddr, ret);
        return ret
    }
    else return UNKNOWN
}

-- Pimitive memory store. Big-endian.

bool StoreMemory (MemType::bits(3), AccessLength::bits(3), MemElem::dword,
                  vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    var pAddr;
    var sc_success = false;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match MemType
    {
        case 0 => (pAddr<2:0> ?? ReverseEndian^3)
        case 1 => (pAddr<2:0> ?? (ReverseEndian^2 : '0'))
        case 3 => (pAddr<2:0> ?? (ReverseEndian : '00'))
        case 7 =>  pAddr<2:0>
        case _ => #UNPREDICTABLE ("bad access length")
    };
    pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
    when not exceptionSignalled do
    {
        a = pAddr<39:3>;
        l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
        mask`64 = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];

        var found = false;
        if a == JTAG_UART.base_address then
        {
            found <- true;
            JTAG_UART_store (mask, MemElem)
        }
        else for core in 0 .. totalCore - 1 do
            when a >=+ PIC_base_address([core]) and
                 a <+ PIC_base_address([core]) + 1072 do
            {
                found <- true;
                PIC_store([core], a, mask, MemElem)
            };

        when cond do match LLbit
        {
            case None => #UNPREDICTABLE("conditional store: LLbit not set")
            case Some (false) => sc_success <- false
            case Some (true) =>
                if CP0.LLAddr<39:5> == pAddr<39:5> then
                    sc_success <- true
                else #UNPREDICTABLE ("conditional store: address does not match previous LL address")
        };

        LLbit <- None;

        when not found do
        {
            for core in 0 .. totalCore - 1 do
            {   i = [core];
                st = all_state (i);
                when i <> procID and
                     (not cond or sc_success) and
                     st.c_LLbit == Some (true) and
                     st.c_CP0.LLAddr<39:5> == pAddr<39:5> do
                        all_state(i).c_LLbit <- Some (false)
            };
            when not cond or sc_success do WriteData(a, MemElem, mask)
        };
        memAccessStats.bytes_written <-
           memAccessStats.bytes_written + [AccessLength] + 0n1;
        when 2 <= trace_level do
           mark_log (2, "Store 0x" : hex64(MemElem) : ", mask 0x" :
                        hex64(mask) : " (" : [[AccessLength] + 0n1] :
                        " byte(s)) at vAddr 0x" : hex64(vAddr));
        watchForStore(pAddr, MemElem, mask)
    };
    sc_success
}

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch =
{
   CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired then
                           [TLBEntries - 1]
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
      when (CP0.Status.IM<7:2> && CP0.Cause.IP<7:2>) <> 0 do
        SignalException (Int)
   };

   if exceptionSignalled then
      None
   else if PC<1:0> == 0 then
   {
      pc, cca = AddressTranslation (PC, INSTRUCTION, LOAD);
      if exceptionSignalled then None else Some (ReadInst (pc))
   }
   else
   {
      CP0.BadVAddr <- PC;
      SignalException (AdEL);
      None
   }
}

-----------------------------------
-- JALR rs (rd = 31 implied)
-- JALR rd, rs
-----------------------------------
define Branch > JALR (rs::reg, rd::reg) =
{
   temp = GPR(rs);
   GPR(rd) <- PC + 8;
   BranchTo <- Some (temp)
}
