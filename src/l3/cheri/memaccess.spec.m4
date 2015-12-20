---------------------------------------------------------------------------
-- CHERI memory accesses
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

include(`helpers.m4')dnl
include(`cap-params.m4')dnl

-- utils functions

word flip_endian_word (w::word) =
{
   c, d = QuotRem ([[w]::nat], 256);
   b, c = QuotRem (c, 256);
   a, b = QuotRem (b, 256);
   changequote(!,!)dnl
   return ([d]`8 : [c]`8 : [b]`8 : [a]`8)
   changequote(`,')dnl
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
            println ("watching --> load 0x" : hex64 (data) : " from 0x" :
                     hex40 (addr))
    }
    case None => nothing
}

unit watchForCapLoad (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:log2(CAPBYTEWIDTH)> == watch_paddr<39:log2(CAPBYTEWIDTH)> do
            println ("watching --> load " : log_cap_write (cap) : " from 0x" :
                     hex40 (addr))
    }
    case None => nothing
}

unit watchForStore (addr::bits(40), data::dword, mask::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
       when addr<39:3> == watch_paddr<39:3> do
        println ("watching --> Store 0x" : hex64 (data) : "(mask: 0x" :
                 hex64 (mask) : ") at 0x" : hex40 (addr))
    case None => nothing
}

unit watchForCapStore (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) =>
       when addr<39:log2(CAPBYTEWIDTH)> == watch_paddr<39:log2(CAPBYTEWIDTH)> do
        println ("watching --> Store 0x" : log_cap_write (cap) : ") at 0x" :
                 hex40 (addr))
    case None => nothing
}

-----------------
-- Data accesses
-----------------

dword LoadMemoryCap (MemType::bits(3), needAlign::bool, vAddr::vAddr, IorD::IorD,
                     AccessType::AccessType, link::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        when 2 <= trace_level do
            mark_log
                (2, "Bad Load, CP0.BadVAddr <-" : hex64(vAddr));
        CP0.BadVAddr <- vAddr;
        SignalException (AdEL);
        UNKNOWN
    }
    else
    {
        tmp, CCA, S, L = AddressTranslation (vAddr, DATA, LOAD);
        pAddr = AdjustEndian (MemType, tmp);
        -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
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
               mark_log (2, "Load of " : [[MemType]::nat+1] :
                            " byte(s) from vAddr 0x":hex64(vAddr));

            watchForLoad(pAddr, ret);
            ret
        }
        else UNKNOWN
    }
}

dword LoadMemory (MemType::bits(3), AccessLength::bits(3), needAlign::bool, vAddr::vAddr,
                  IorD::IorD, AccessType::AccessType, link::bool) =
{
    final_vAddr = vAddr + getBase(CAPR(0)) + getOffset(CAPR(0));
    if not getTag(CAPR(0))
        then {SignalCapException(capExcTag,0); UNKNOWN}
    else if getSealed(CAPR(0))
        then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if (final_vAddr <+ getBase(CAPR(0)))
        then {SignalCapException(capExcLength,0); UNKNOWN}
    else if (final_vAddr + ZeroExtend(AccessLength) >+ getBase(CAPR(0)) + getLength(CAPR(0)))
        then {SignalCapException(capExcLength,0); UNKNOWN}
    else if not getPerms(CAPR(0)).Permit_Load
        then {SignalCapException(capExcPermLoad, 0); UNKNOWN}
    else LoadMemoryCap(MemType, needAlign, final_vAddr, IorD, AccessType, link)
}

Capability LoadCap (vAddr::vAddr, link::bool) =
{
    pAddr, CCA, S, L = AddressTranslation (vAddr, DATA, LOAD);
    if not exceptionSignalled then
    {
        a = pAddr<39:log2(CAPBYTEWIDTH)>;

        define(BOTTOM, eval(log2(CAPBYTEWIDTH)-3))dnl
        if a == JTAG_UART.base_address<36:BOTTOM> then
            #UNPREDICTABLE ("Capability load attempted on UART")
        else
            for core in 0 .. (totalCore - 1) do
                when a >=+ PIC_base_address([core])<36:BOTTOM>
                     and a <+ (PIC_base_address([core])+1072)<36:BOTTOM> do
                    #UNPREDICTABLE ("Capability load attempted on PIC");
        undefine(BOTTOM)dnl

        if link then
        {
            LLbit <- Some (true);
            CP0.LLAddr <- [pAddr]
        }
        else
            LLbit <- None;

        var cap = ReadCap(a);

        when L do cap <- setTag(cap, false);

        memAccessStats.bytes_read <- memAccessStats.bytes_read + CAPBYTEWIDTH;
        when 2 <= trace_level do
           mark_log (2, "Load cap: " : log_load_cap (pAddr, cap) :
                        " from vAddr 0x":hex64(vAddr));

        watchForCapLoad(pAddr, cap);
        return cap
    }
    else return UNKNOWN
}

bool StoreMemoryCap (MemType::bits(3), AccessLength::bits(3), MemElem::dword, needAlign::bool,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        when 2 <= trace_level do
            mark_log
                (2, "Bad Store, CP0.BadVAddr <-" : hex64(vAddr));
        CP0.BadVAddr <- vAddr;
        SignalException (AdES);
        return UNKNOWN
    }
    else {
        var sc_success = false;
        tmp, CCA, S, L = AddressTranslation (vAddr, DATA, STORE);
        pAddr = AdjustEndian (MemType, tmp);
        -- pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
        when not exceptionSignalled do
        {
            a = pAddr<39:3>;
            l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
            mask::bits(64) = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];

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
            when 2 <= trace_level do
               mark_log (2, "Store 0x" : hex64(MemElem) : ", mask 0x" :
                            hex64(mask) : " (" : [[AccessLength]::nat+1] :
                            " byte(s)) at vAddr 0x" : hex64(vAddr));
            watchForStore(pAddr, MemElem, mask)
        };
        return sc_success
    }
}

bool StoreMemory (MemType::bits(3), AccessLength::bits(3), needAlign::bool, MemElem::dword,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    final_vAddr = vAddr + getBase(CAPR(0)) + getOffset(CAPR(0));
    if not getTag(CAPR(0))
        then {SignalCapException(capExcTag,0); UNKNOWN}
    else if getSealed(CAPR(0))
        then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if (final_vAddr <+ getBase(CAPR(0)))
        then {SignalCapException(capExcLength,0); UNKNOWN}
    else if (final_vAddr + ZeroExtend(AccessLength) >+ getBase(CAPR(0)) + getLength(CAPR(0)))
        then {SignalCapException(capExcLength,0); UNKNOWN}
    else if not getPerms(CAPR(0)).Permit_Store
        then {SignalCapException(capExcPermStore, 0); UNKNOWN}
    else StoreMemoryCap (MemType, AccessLength, MemElem, needAlign, final_vAddr, IorD,
                         AccessType, cond)
}

unit StoreMem
   (MemType::bits(3), AccessLength::bits(3), needAlign::bool, MemElem::dword,
    vAddr::vAddr, IorD::IorD, AccessType::AccessType) =
{
   _ = StoreMemory (MemType,AccessLength,needAlign,MemElem,vAddr,IorD,
                    AccessType,false);
   nothing
}

bool StoreCap (vAddr::vAddr, cap::Capability, cond::bool) =
{
    var sc_success = false;
    pAddr, CCA, S, L = AddressTranslation (vAddr, DATA, STORE);
    when not exceptionSignalled do
    {
        a = pAddr<39:log2(CAPBYTEWIDTH)>;

        define(BOTTOM, eval(log2(CAPBYTEWIDTH)-3))dnl
        if a == JTAG_UART.base_address<36:BOTTOM> then
            #UNPREDICTABLE ("Capability store attempted on UART")
        else
            for core in 0 .. (totalCore - 1) do
                when a >=+ PIC_base_address([core])<36:BOTTOM>
                     and a <+ (PIC_base_address([core])+1072)<36:BOTTOM> do
                    #UNPREDICTABLE ("Capability store attempted on PIC");
        undefine(BOTTOM)dnl

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
                    st.c_CP0.LLAddr<39:log2(CAPBYTEWIDTH)> == pAddr<39:log2(CAPBYTEWIDTH)> do
                        all_state(i).c_LLbit <- Some (false)
            };

            memAccessStats.bytes_written <- memAccessStats.bytes_written + CAPBYTEWIDTH;
            when 2 <= trace_level do
               mark_log (2, "Store cap: " : log_store_cap (pAddr, cap) :
                            " at vAddr 0x":hex64(vAddr));

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
                            then [TLBEntries - 1]
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
        vAddr = PC + getBase(PCC);
        if not getTag(PCC) then {SignalCapException_noReg(capExcTag); None}
        else if getSealed(PCC) then {SignalCapException_noReg(capExcSeal); None}
        else if (vAddr <+ getBase(PCC)) then {SignalCapException_noReg(capExcLength); None}
        -- TODO need to take care of the 65 bit check (base+length overflows) everywhere else
        -- TODO and the +4 for instruction bounds check and the +access size on data bounds check
        -- TODO and whether inequalities are large or strict in all bounds checks
        else if (('0':vAddr)+4 >+ [getBase(PCC)] + [getLength(PCC)]) then {SignalCapException_noReg(capExcLength); None}
        else if not getPerms(PCC).Permit_Execute then {SignalCapException_noReg(capExcPermExe); None}
        else {
            pc, cca = AddressTranslation (vAddr, INSTRUCTION, LOAD);
            if exceptionSignalled then None else Some (ReadInst (pc))
        }
    }
    else
    {
        when 2 <= trace_level do
            mark_log
                (2, "Bad IFetch, CP0.BadVAddr <-" : hex64(getBase(PCC) + PC));
        CP0.BadVAddr <- getBase(PCC) + PC;
        SignalException (AdEL);
        None
    }
}
