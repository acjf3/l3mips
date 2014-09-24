---------------------------------------------------------------------------
-- CHERI memory accesses
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------
-- Data accesses
-----------------

dword * pAddr LoadMemory (AccessLength::bits(3), vAddr::vAddr,
                            IorD::IorD, AccessType::AccessType) =
{
    var pAddr;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match AccessLength
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
            ret <- flip_endian_word (JTAG_UART.&data)
                : flip_endian_word (JTAG_UART.&control);
            when pAddr<2:0> == 0 do JTAG_UART_load
        }
        else for core in 0 .. (totalCore - 1) do
            when a >= PIC_base_address([core]) and a < (PIC_base_address([core])+1072) do
                {found <- true; ret <- PIC_load([core], a)};

        when found == false do
            ret <- MEM (a);

        return (ret, pAddr)
    }
    else return UNKNOWN
}

word loadWord32 (a::pAddr) =
{
    d = MEM (a<39:3>);
    if a<2> then d<31:0> else d<63:32>
}

pAddr StoreMemory (AccessLength::bits(3), MemElem::dword,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType) =
{
    var pAddr;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match AccessLength
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
        l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
        mask`64 = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];
        mark (w_mem (pAddr, mask, AccessLength, MemElem));

        var found = false;
        if a == JTAG_UART.base_address then
            {found <- true; JTAG_UART_store (mask, MemElem)}
        else for core in 0 .. (totalCore - 1) do
        when a >= PIC_base_address([core]) and a < (PIC_base_address([core])+1072) do
            {found <- true; PIC_store([core], a, mask, MemElem)};

        when found == false do
        {
            for core in 0 .. (totalCore - 1) do
                when core <> [procID] and
                    c_LLbit([core]) == Some (true) and
                    c_CP0([core]).LLAddr<39:3> == pAddr<39:3> do
                        c_LLbit([core]) <- Some (false);
            MEM(a) <- MEM(a) && ~mask || MemElem && mask
        };
        return pAddr
    }
    else return UNKNOWN
}

-------------------------
-- Instructions accesses
-------------------------

word option Fetch =
{
    log <- Nil;
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
        var perms::Perms;
        &perms <- PCC.perms;

        if (vAddr >+ PCC.base + PCC.length) then {SignalCapException_noReg(capExcLength); None}
        else if not perms.Permit_Execute then {SignalCapException_noReg(capExcPermExe); None}
        else {
            pc, cca = AddressTranslation (vAddr, INSTRUCTION, LOAD);
            if exceptionSignalled then None else Some (loadWord32 (pc))
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
