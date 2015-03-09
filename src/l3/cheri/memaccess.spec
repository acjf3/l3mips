---------------------------------------------------------------------------
-- CHERI memory accesses
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- utils functions

word flip_endian_word (w::word) =
   match w { case 'a`8 b`8 c`8 d' => d : c : b : a }

-- watch paddr

declare watchPaddr::bits(40) option

unit watchForLoad (addr::bits(40), data::dword) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:3> == watch_paddr<39:3> do
            println ("watching --> load 0x" : PadLeft (#"0", 16, [data]) : " from 0x" : PadLeft (#"0", 10, [addr]))
    }
    case None => nothing
}

unit watchForCapLoad (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) =>
    {
        when addr<39:5> == watch_paddr<39:5> do
            println ("watching --> load " : log_cap_write (cap) : " from 0x" : PadLeft (#"0", 10, [addr]))
    }
    case None => nothing
}

unit watchForStore (addr::bits(40), data::dword, mask::dword) = match watchPaddr
{
    case Some(watch_paddr) => when addr<39:3> == watch_paddr<39:3> do
        println ("watching --> Store 0x" : PadLeft (#"0", 16, [data]) : "(mask:" : PadLeft (#"0", 16, [mask]) : ") at 0x" : PadLeft (#"0", 10, [addr]))
    case None => nothing
}

unit watchForCapStore (addr::bits(40), cap::Capability) = match watchPaddr
{
    case Some(watch_paddr) => when addr<39:5> == watch_paddr<39:5> do
        println ("watching --> Store 0x" :log_cap_write (cap) : ") at 0x" : PadLeft (#"0", 10, [addr]))
    case None => nothing
}

-----------------
-- Data accesses
-----------------

dword LoadMemoryCap (MemType::bits(3), vAddr::vAddr, IorD::IorD,
                     AccessType::AccessType, link::bool) =
{
    var pAddr;
    tmp, CCA, S, L = AddressTranslation (vAddr, DATA, LOAD);
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

        when found == false do
            ret <- ReadData (a);

        mark_log (2, "Load of ":[[MemType]::nat+1]:" byte(s)");

        watchForLoad(pAddr, ret);
        return ret
    }
    else return UNKNOWN
}

dword LoadMemory (MemType::bits(3), AccessLength::bits(3), vAddr::vAddr,
                            IorD::IorD, AccessType::AccessType, link::bool) =
{
    final_vAddr = vAddr + CAPR(0).base + CAPR(0).offset;
    if not CAPR(0).tag then {SignalCapException(capExcTag,0); UNKNOWN}
    else if CAPR(0).sealed then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if (final_vAddr <+ CAPR(0).base) then {SignalCapException(capExcLength,0); UNKNOWN}
    else if (final_vAddr >+ CAPR(0).base + CAPR(0).length) then {SignalCapException(capExcLength,0); UNKNOWN}
    else if not Perms(CAPR(0).perms).Permit_Load then {SignalCapException(capExcPermLoad, 0); UNKNOWN}
    else LoadMemoryCap(MemType, final_vAddr, IorD, AccessType, link)
}

Capability LoadCap (vAddr::vAddr) =
{
    pAddr, CCA, S, L = AddressTranslation (vAddr, DATA, CLOAD);
    if not exceptionSignalled then
    {
        a = pAddr<39:5>;

        if a == JTAG_UART.base_address<36:2> then
            #UNPREDICTABLE ("Capability load attempted on UART")
        else
            for core in 0 .. (totalCore - 1) do
                when a >=+ PIC_base_address([core])<36:2>
                     and a <+ (PIC_base_address([core])+1072)<36:2> do
                    #UNPREDICTABLE ("Capability load attempted on PIC");

        var Capability = ReadCap(a);

        when L do Capability.tag <- false;

        LLbit <- None;

        mark_log (2, "Load cap: " : log_load_cap (pAddr, Capability));

        watchForCapLoad(pAddr, Capability);
        return Capability
    }
    else return UNKNOWN
}

bool StoreMemoryCap (MemType::bits(3), AccessLength::bits(3), MemElem::dword,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    var pAddr;
    var sc_success = false;
    tmp, CCA, S, L = AddressTranslation (vAddr, DATA, STORE);
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
                else #UNPREDICTABLE("conditional store: address does not match previous LL address")
        };

        LLbit <- None;

        when not found do
        {
            for core in 0 .. totalCore - 1 do
                when core <> [procID] and
                     (not cond or sc_success) and
                     c_LLbit([core]) == Some (true) and
                     c_CP0([core]).LLAddr<39:5> == pAddr<39:5> do
                        c_LLbit([core]) <- Some (false);
            when not cond or sc_success do
                WriteData(a, MemElem, mask)
        };
        mark_log (2, "Store of ":[[AccessLength]::nat+1]:" byte(s)");
        watchForStore(pAddr, MemElem, mask)
    };
    return sc_success
}

bool StoreMemory (MemType::bits(3), AccessLength::bits(3), MemElem::dword,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    final_vAddr = vAddr + CAPR(0).base + CAPR(0).offset;
    if not CAPR(0).tag then {SignalCapException(capExcTag,0); UNKNOWN}
    else if CAPR(0).sealed then {SignalCapException(capExcSeal,0); UNKNOWN}
    else if (final_vAddr <+ CAPR(0).base) then {SignalCapException(capExcLength,0); UNKNOWN}
    else if (final_vAddr >+ CAPR(0).base + CAPR(0).length) then {SignalCapException(capExcLength,0); UNKNOWN}
    else if not Perms(CAPR(0).perms).Permit_Store then {SignalCapException(capExcPermStore, 0); UNKNOWN}
    else StoreMemoryCap(MemType, AccessLength, MemElem, final_vAddr, IorD, AccessType, cond)
}

unit StoreCap (vAddr::vAddr, Capability::Capability) =
{
    pAddr, CCA, S, L = AddressTranslation (vAddr, DATA, CSTORE);
    when not exceptionSignalled do
    {
        a = pAddr<39:5>;

        if a == JTAG_UART.base_address<36:2> then
            #UNPREDICTABLE ("Capability store attempted on UART")
        else
            for core in 0 .. (totalCore - 1) do
                when a >=+ PIC_base_address([core])<36:2>
                     and a <+ (PIC_base_address([core])+1072)<36:2> do
                    #UNPREDICTABLE ("Capability store attempted on PIC");

        LLbit <- None;

        if (S and Capability.tag) then
            SignalCapException_noReg (capExcTLBNoStore)
        else
        {
            for core in 0 .. (totalCore - 1) do
                when core <> [procID] and
                    c_LLbit([core]) == Some (true) and
                    c_CP0([core]).LLAddr<39:5> == pAddr<39:5> do
                        c_LLbit([core]) <- Some (false);

            mark_log (2, "Store cap: " : log_store_cap (pAddr, Capability));

            watchForCapStore(pAddr, Capability);
            WriteCap(a, Capability)
        }
    }
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
        vAddr = PC + PCC.base;
        if not PCC.tag then {SignalCapException_noReg(capExcTag); None}
        else if PCC.sealed then {SignalCapException_noReg(capExcSeal); None}
        else if (vAddr >+ PCC.base + PCC.length) then {SignalCapException_noReg(capExcLength); None}
        else if (vAddr <+ PCC.base) then {SignalCapException_noReg(capExcLength); None}
        else if not Perms(PCC.perms).Permit_Execute then {SignalCapException_noReg(capExcPermExe); None}
        else {
            pc, cca = AddressTranslation (vAddr, INSTRUCTION, LOAD);
            if exceptionSignalled then None else Some (ReadInst (pc))
        }
    }
    else
    {
        CP0.BadVAddr <- PCC.base + PC;
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
