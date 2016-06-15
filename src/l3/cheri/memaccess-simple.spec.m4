---------------------------------------------------------------------------
-- CHERI memory accesses
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- utils functions
---------------------------------------------------------------------------

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

-- dummy stubs
---------------------------------------------------------------------------

unit initMemAccessStats = nothing
unit initMemStats = nothing

string printMemAccessStats = "No MemAccessStats implemented"
string csvHeaderMemAccessStats = "No csvHeaderAccessStats implemented"
string csvMemAccessStats = "No csvMemAccessStats implemented"
string printMemStats = "No MemStats implemented"
string csvHeaderMemStats = "No csvHeaderMemStats implemented"
string csvMemStats = "No csvMemStats implemented"
unit clearDynamicMemStats () = nothing

declare watchPaddr::bits(40) option

-- memory types and declarations
---------------------------------------------------------------------------

construct DataType {Cap :: Capability, Raw :: CAPRAWBITS}

declare mem :: CAPADDR -> DataType

unit InitMEM = mem <- InitMap (Raw(UNKNOWN))

dword ReadData (dwordAddr::bits(37)) = match mem(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>)
{
    case Cap (cap) => readDwordFromRaw (dwordAddr, capToBits(cap))
    case Raw (raw) => readDwordFromRaw (dwordAddr, raw)
}

unit WriteData (dwordAddr::bits(37), data::dword, mask::dword) =
{
    old_blob = match mem(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>)
    {
        case Cap (cap) => capToBits(cap)
        case Raw (raw) => raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, mask, old_blob);
    mem(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>) <- Raw(new_blob)
}

word ReadInst (a::pAddr) =
{
    inst_pair = match mem(a<39:Log2(CAPBYTEWIDTH)>)
    {
        case Cap (cap) => readDwordFromRaw (a<39:3>, capToBits(cap))
        case Raw (raw) => readDwordFromRaw (a<39:3>, raw)
    };
    if a<2> then inst_pair<31:0> else inst_pair<63:32>
}

Capability ReadCap (capAddr::CAPADDR) = match mem(capAddr)
{
    case Cap (cap) => cap
    case Raw (raw) => bitsToCap(raw)
}

unit WriteCap (capAddr::CAPADDR, cap::Capability) = mem(capAddr) <- Cap (cap)

-- sml stubs
---------------------------------------------------------------------------

ifelse(dnl
regexp(CAP,p64),0,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
    mem(dwordAddr) <- Raw(data)

unit Write256 (addr::bits(35), data::bits(256)) =
{
    mem(addr:'11') <- Raw (data<255:192>);
    mem(addr:'10') <- Raw (data<191:128>);
    mem(addr:'01') <- Raw (data<127:64>);
    mem(addr:'00') <- Raw (data<63:0>)
}
,dnl
regexp(CAP,c128c\|c128c3\|p128),0,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
{
    var fill = 0;
    match mem(dwordAddr<36:1>)
    {
        case Raw (raw) => fill <- if dwordAddr<0> then raw<63:0> else raw<127:64>
        case _ => nothing
    };
    new_data = if dwordAddr<0> then Raw (data:fill) else Raw (fill:data);
    mem(dwordAddr<36:1>) <- new_data
}

unit Write256 (addr::bits(35), data::bits(256)) =
{
    mem(addr:'1') <- Raw (data<255:128>);
    mem(addr:'0') <- Raw (data<127:0>)
}
,dnl
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
{
    old_blob = match mem(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>)
    {
        case Cap (cap) => [&cap]
        case Raw (raw) => raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, ~0, old_blob);
    mem(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>) <- Raw(new_blob)
}

unit Write256 (addr::bits(35), data::bits(256)) =
{
    mem(addr) <- Raw (data)
}
)dnl

-- memory API
---------------------------------------------------------------------------

-- memory loads

dword LoadMemoryCap (MemType::bits(3), needAlign::bool, vAddr::vAddr, IorD::IorD,
                     AccessType::AccessType, link::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        CP0.BadVAddr <- vAddr;
        SignalException (AdEL);
        UNKNOWN
    }
    else
    {
        pAddr = AdjustEndian (MemType, [vAddr]);
        if link then
        {
            LLbit <- Some (true);
            CP0.LLAddr <- [pAddr]
        }
        else
            LLbit <- None;
        ReadData (pAddr<39:3>)
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
    L = false; -- XXX usually comes from the MMU
    if link then
    {
        LLbit <- Some (true);
        CP0.LLAddr <- vAddr
    }
    else
        LLbit <- None;
    var cap = ReadCap(vAddr<39:Log2(CAPBYTEWIDTH)>);
    when L do cap <- setTag(cap, false);
    return cap
}

-- memory stores

bool StoreMemoryCap (MemType::bits(3), AccessLength::bits(3), MemElem::dword, needAlign::bool,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    if needAlign and not isAligned (vAddr, MemType)
    then {
        CP0.BadVAddr <- vAddr;
        SignalException (AdES);
        return UNKNOWN
    }
    else {
        var sc_success = false;
        pAddr = AdjustEndian (MemType, [vAddr]);
        l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
        mask::bits(64) = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];

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
            WriteData(pAddr<39:3>, MemElem, mask);
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
    S = false; -- XXX usually comes from the MMU

    when cond do match LLbit
    {
        case None => #UNPREDICTABLE("conditional store of capability: LLbit not set")
        case Some (false) => sc_success <- false
        case Some (true) =>
            if CP0.LLAddr == vAddr then
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
                st.c_CP0.LLAddr<39:Log2(CAPBYTEWIDTH)> == vAddr<39:Log2(CAPBYTEWIDTH)> do
                    all_state(i).c_LLbit <- Some (false)
        };
        when not cond or sc_success do
            WriteCap(vAddr<39:Log2(CAPBYTEWIDTH)>, cap)
    };
    return sc_success
}

-- instructions accesses

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
        vAddr = PC + getBase(PCC);
        if not getTag(PCC) then {SignalCapException_noReg(capExcTag); None}
        else if getSealed(PCC) then {SignalCapException_noReg(capExcSeal); None}
        else if (vAddr <+ getBase(PCC)) then {SignalCapException_noReg(capExcLength); None}
        -- TODO need to take care of the 65 bit check (base+length overflows) everywhere else
        -- TODO and the +4 for instruction bounds check and the +access size on data bounds check
        -- TODO and whether inequalities are large or strict in all bounds checks
        else if (('0':vAddr)+4 >+ [getBase(PCC)] + [getLength(PCC)]) then {SignalCapException_noReg(capExcLength); None}
        else if not getPerms(PCC).Permit_Execute then {SignalCapException_noReg(capExcPermExe); None}
        else if exceptionSignalled then None else Some (ReadInst ([vAddr]))
    }
    else
    {
        CP0.BadVAddr <- getBase(PCC) + PC;
        SignalException (AdEL);
        None
    }
}
