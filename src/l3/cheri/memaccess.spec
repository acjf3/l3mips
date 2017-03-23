---------------------------------------------------------------------------
-- CHERI memory accesses
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- utils functions

word flip_endian_word (w::word) =
{
   c, d = QuotRem ([[w]::nat], 256);
   b, c = QuotRem (c, 256);
   a, b = QuotRem (b, 256);
   return ([d]`8 : [c]`8 : [b]`8 : [a]`8)
}

bool isAligned (vAddr::vAddr, MemType::bits(3)) = [vAddr] && MemType == 0

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

declare watchPaddr::bits(40) option

unit watchForLoad (addr::bits(40), data::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:3> == watch_paddr<39:3> do
            mark_watcher("watching --> load " : hex (data) : " from " : hex (addr))
    }
    case None => nothing
}

unit watchForCapLoad (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:Log2(CAPBYTEWIDTH)> == watch_paddr<39:Log2(CAPBYTEWIDTH)> do
            mark_watcher("watching --> load " : log_cap_write (cap) : " from " :
                     hex (addr))
    }
    case None => nothing
}

unit watchForStore (addr::bits(40), data::dword, mask::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
        mark_watcher("watching --> Store " : hex (data) : "(mask: " :
                 hex (mask) : ") at " : hex (addr))
    case None => nothing
}

unit watchForCapStore (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) =>
       when addr<39:Log2(CAPBYTEWIDTH)> == watch_paddr<39:Log2(CAPBYTEWIDTH)> do
        mark_watcher("watching --> Store 0x" : log_cap_write (cap) : ") at " :
                 hex (addr))
    case None => nothing
}

-- virtual address computation
vAddr getVirtualAddress (addr::bits(64)) =
  addr + getBase(CAPR(0)) + getOffset(CAPR(0))

-----------------
-- Data accesses
-----------------

dword LoadMemoryCap (MemType::bits(3), needAlign::bool, vAddr::vAddr, link::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        mark_log (2, "Bad Load, CP0.BadVAddr <-" : hex (vAddr));
        CP0.BadVAddr <- vAddr;
        SignalException (AdEL);
        UNKNOWN
    }
    else
    {
        tmp, CCA, S, L = AddressTranslation (vAddr, LOAD);
        pAddr = AdjustEndian (MemType, tmp);
        -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
        if exceptionSignalled then UNKNOWN else
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
            {
                base = PIC_base_address([core]);
                when base <=+ a and a <+ base + 1072 do
                {
                    found <- true;
                    ret <- PIC_load([core], a)
                }
            };

            if link then
            {
                when CCA == 2 do
                    #UNPREDICTABLE("load linked on uncached address");
                LLbit <- Some (true);
                CP0.LLAddr <- [pAddr]
            }
            else
                LLbit <- None;

            when not found do
                ret <- ReadData (a);

            memAccessStats.bytes_read <- memAccessStats.bytes_read + [[MemType]::nat+1];
            mark_log (2, "Load of " : [[MemType]::nat+1] :
                         " byte(s) from vAddr ":hex (vAddr));

            watchForLoad(pAddr, ret);
            ret
        }
    }
}

dword LoadMemory (MemType::bits(3), AccessLength::bits(3), needAlign::bool, vAddr::vAddr, link::bool) =
{
    capr0 = CAPR(0);
    if not getTag(capr0)
        then {SignalCapException(capExcTag,0); UNKNOWN}
    else if getSealed(capr0)
        then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if not getPerms(capr0).Permit_Load
        then {SignalCapException(capExcPermLoad, 0); UNKNOWN}
    else
    {
       base, len = getBaseAndLength(capr0);
       if vAddr <+ base
          then {SignalCapException(capExcLength,0); UNKNOWN}
       else if vAddr + ZeroExtend(AccessLength) >+ base + len
          then {SignalCapException(capExcLength,0); UNKNOWN}
       else LoadMemoryCap(MemType, needAlign, vAddr, link)
    }
}

inline nat capbottom = Log2(CAPBYTEWIDTH)-3

Capability LoadCap (vAddr::vAddr, link::bool) =
{
    pAddr, CCA, S, L = AddressTranslation (vAddr, LOAD);
    if exceptionSignalled then UNKNOWN else
    {
        a = pAddr<39:Log2(CAPBYTEWIDTH)>;

        if a == JTAG_UART.base_address<36:capbottom> then
            #UNPREDICTABLE ("Capability load attempted on UART")
        else for core in 0 .. totalCore - 1 do
        {
           base = PIC_base_address([core]);
           when base<36:capbottom> <=+ a and a <+ (base+1072)<36:capbottom> do
              #UNPREDICTABLE ("Capability load attempted on PIC")
        };

        if link then
        {
            when CCA == 2 do
                #UNPREDICTABLE("load linked on uncached address");
            LLbit <- Some (true);
            CP0.LLAddr <- [pAddr]
        }
        else
            LLbit <- None;

        var cap = ReadCap(a);

        when L do cap <- setTag(cap, false);

        memAccessStats.bytes_read <- memAccessStats.bytes_read + CAPBYTEWIDTH;
        mark_log (2, "Load cap: " : log_load_cap (pAddr, cap) :
                     " from vAddr ":hex (vAddr));

        watchForCapLoad(pAddr, cap);
        return cap
    }
}

bool StoreMemoryCap (MemType::bits(3), AccessLength::bits(3), MemElem::dword, needAlign::bool,
                   vAddr::vAddr, cond::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        mark_log (2, "Bad Store, CP0.BadVAddr <-" : hex (vAddr));
        CP0.BadVAddr <- vAddr;
        SignalException (AdES);
        return UNKNOWN
    }
    else {
        var sc_success = false;
        tmp, CCA, S, L = AddressTranslation (vAddr, STORE);
        pAddr = AdjustEndian (MemType, tmp);
        -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
        when not exceptionSignalled do
        {
            a = pAddr<39:3>;
            l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
            mask`64 = 1 << (l + ([AccessLength] + 1) * 0n8) - 1 << l;

            var found = false;
            if a == JTAG_UART.base_address then
            {
                found <- true;
                JTAG_UART_store (mask, MemElem)
            }
            else for core in 0 .. totalCore - 1 do
            {
                base = PIC_base_address([core]);
                when base <=+ a and a <+ base + 1072 do
                {
                    found <- true;
                    PIC_store([core], a, mask, MemElem)
                }
            };

            when cond do match LLbit
            {
                case None => #UNPREDICTABLE("conditional store: LLbit not set")
                case Some (false) => sc_success <- false
                case Some (true) =>
                    if CP0.LLAddr == [pAddr] then
                        sc_success <- true
                    else #UNPREDICTABLE("conditional store: address does not match previous LL address")
            };

            LLbit <- None;

            when not found do
            {
                for core in 0 .. totalCore - 1 do
                {   i = [core];
                    st = all_state(i);
                    when i <> procID and
                         (not cond or sc_success) and
                         st.c_LLbit == Some (true) and
                         st.c_CP0.LLAddr<39:3> == pAddr<39:3> do
                            all_state(i).c_LLbit <- Some (false)
                };
                when not cond or sc_success do
                    WriteData(a, MemElem, mask)
            };
            memAccessStats.bytes_written <- memAccessStats.bytes_written + [[AccessLength]::nat+1];
            mark_log (2, "Store " : hex (MemElem) : ", mask " :
                         hex (mask) : " (" : [[AccessLength]::nat+1] :
                         " byte(s)) at vAddr " : hex (vAddr));
            watchForStore(pAddr, MemElem, mask)
        };
        return sc_success
    }
}

bool StoreMemory (MemType::bits(3), AccessLength::bits(3), needAlign::bool,
                  MemElem::dword, vAddr::vAddr, cond::bool) =
{
    capr0 = CAPR(0);
    if not getTag(capr0)
        then {SignalCapException(capExcTag,0); UNKNOWN}
    else if getSealed(capr0)
        then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if not getPerms(capr0).Permit_Store
        then {SignalCapException(capExcPermStore, 0); UNKNOWN}
    else
    {
        base, len = getBaseAndLength(capr0);
        if vAddr <+ base
            then {SignalCapException(capExcLength,0); UNKNOWN}
        else if vAddr + ZeroExtend(AccessLength) >+ base + len
            then {SignalCapException(capExcLength,0); UNKNOWN}
        else StoreMemoryCap (MemType, AccessLength, MemElem, needAlign, vAddr, cond)
    }
}

bool StoreCap (vAddr::vAddr, cap::Capability, cond::bool) =
{
    var sc_success = false;
    pAddr, CCA, S, L = AddressTranslation (vAddr, STORE);
    when not exceptionSignalled do
    {
        a = pAddr<39:Log2(CAPBYTEWIDTH)>;

        if a == JTAG_UART.base_address<36:capbottom> then
            #UNPREDICTABLE ("Capability store attempted on UART")
        else for core in 0 .. totalCore - 1 do
        {
            base = PIC_base_address([core]);
            when base<36:capbottom> <=+ a and a <+ (base+1072)<36:capbottom> do
               #UNPREDICTABLE ("Capability store attempted on PIC")
        };

        when cond do match LLbit
        {
            case None => #UNPREDICTABLE("conditional store of capability: LLbit not set")
            case Some (false) => sc_success <- false
            case Some (true) =>
                if CP0.LLAddr == [pAddr] then
                    sc_success <- true
                else #UNPREDICTABLE("conditional store of capability: address does not match previous LL address")
        };

        LLbit <- None;

        if (S and getTag(cap)) then
            SignalCapException_noReg (capExcTLBNoStore)
        else
        {
            for core in 0 .. totalCore - 1 do
            {
                i = [core];
                st = all_state(i);
                when i <> procID and
                    (not cond or sc_success) and
                    st.c_LLbit == Some (true) and
                    st.c_CP0.LLAddr<39:Log2(CAPBYTEWIDTH)> == pAddr<39:Log2(CAPBYTEWIDTH)> do
                        all_state(i).c_LLbit <- Some (false)
            };

            memAccessStats.bytes_written <- memAccessStats.bytes_written + CAPBYTEWIDTH;
            mark_log (2, "Store cap: " : log_store_cap (pAddr, cap) :
                         " at vAddr ":hex (vAddr));

            watchForCapStore(pAddr, cap);
            when not cond or sc_success do
                WriteCap(a, cap)
        }
    };
    return sc_success
}

-------------------------
-- Instructions accesses
-------------------------

word option Fetch =
{
    CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired
                            then [TLBAssocEntries - 1]
                            else CP0.Random.Random - 1;
    when CP0.Compare == CP0.Count do
    {
        CP0.Cause.IP<7> <- true;
        CP0.Cause.TI <- true
    };

    when CP0.Status.IE and not (CP0.Status.EXL or CP0.Status.ERL) do
    {
        -- If any interrupts pending, raise an exception
        when (CP0.Status.IM<7:2> && CP0.Cause.IP<7:2>) <> 0 do
            SignalException (Int)
    };

    if exceptionSignalled then None
    else if PC<1:0> == 0 then
    {
        base, len = getBaseAndLength(PCC);
        vAddr = PC + base;
        if not getTag(PCC)
           then {SignalCapException_noReg(capExcTag); None}
        else if getSealed(PCC)
           then {SignalCapException_noReg(capExcSeal); None}
        else if vAddr <+ base
           then {SignalCapException_noReg(capExcLength); None}
        -- TODO need to take care of the 65 bit check (base+length overflows) everywhere else
        -- TODO and the +4 for instruction bounds check and the +access size on data bounds check
        -- TODO and whether inequalities are large or strict in all bounds checks
        else if ('0':vAddr)+4 >+ [base] + [len]
           then {SignalCapException_noReg(capExcLength); None}
        else if not getPerms(PCC).Permit_Execute
           then {SignalCapException_noReg(capExcPermExe); None}
        else
        {
            pc, cca = AddressTranslation (vAddr, LOAD);
            if exceptionSignalled then None else Some (ReadInst (pc))
        }
    }
    else
    {
        mark_log (2, "Bad IFetch, CP0.BadVAddr <-" : hex (getBase(PCC) + PC));
        CP0.BadVAddr <- getBase(PCC) + PC;
        SignalException (AdEL);
        None
    }
}
