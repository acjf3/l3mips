---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

include(`cap-params.m4')dnl
-------------------
-- Helper functions
-------------------
bool register_inaccessible(cb::reg) = allow_system_reg_access(getPerms(PCC), cb)

bool register_inaccessible_write_attempt(mask::bits(16)) =
{
    var ret = true;
    if mask<15> and register_inaccessible(31) then
        SignalCapException_v(31)
    else if mask<14> and register_inaccessible(30) then
        SignalCapException_v(30)
    else if mask<13> and register_inaccessible(29) then
        SignalCapException_v(29)
    else if mask<11> and register_inaccessible(27) then
        SignalCapException_v(27)
    else if mask<12> and register_inaccessible(28) then
        SignalCapException_v(28)
    else ret <- false;
    ret
}

-- only works for non empty lists
bool list SignExtendBitString(w::nat, x::bool list) = PadLeft (Head(x), w, x)
bool list ZeroExtendBitString(w::nat, x::bool list) = PadLeft (false, w, x)

-----------------------------------
-- dump capability registers
-----------------------------------
define COP2 > CHERICOP2 > DumpCapReg = dumpCRegs()

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetBase (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- getBase(CAPR(cb))

-----------------------------------
-- CGetOffset rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetOffset (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- getOffset(CAPR(cb))

-----------------------------------
-- CGetLen rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetLen (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- getLength(CAPR(cb))

-----------------------------------
-- CGetTag rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetTag (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- ZeroExtend([getTag(CAPR(cb))])

-----------------------------------
-- CGetSealed rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetSealed (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<0> <- getSealed(CAPR(cb));
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetPerm rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPerm (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- ZeroExtend(&getPerms(CAPR(cb)))

-----------------------------------
-- CGetType rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetType (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- ZeroExtend(getType(CAPR(cb)))

-----------------------------------
-- CGetPCC cd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCC (cd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else
        CAPR(cd) <- setOffset(PCC, PC)

-----------------------------------
-- CGetPCCSetOffset cd, rs
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCCSetOffset (cd::reg, rs::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if not isCapRepresentable (getSealed(PCC),
                                    getBase(PCC),
                                    getLength(PCC),
                                    GPR(rs)) then
        CAPR(cd) <- setOffset(nullCap, getBase(PCC) + GPR(rs))
    else
        CAPR(cd) <- setOffset(PCC, GPR(rs))

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible({-EPCC-}31) then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        GPR(rd)<7:0> <- capcause.RegNum;
        GPR(rd)<15:8> <- capcause.ExcCode;
        GPR(rd)<63:16> <- 0
    }

-----------------------------------
-- CSetCause rt
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetCause (rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible({-EPCC-}31) then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        capcause.ExcCode <- GPR(rt)<15:8>;
        capcause.RegNum <- GPR(rt)<7:0>
    }

-----------------------------------
-- CIncOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if getTag(CAPR(cb)) and getSealed(CAPR(cb)) and GPR(rt) <> 0 then
        SignalCapException(capExcSeal,cb)
    else if not isCapRepresentable (getSealed(CAPR(cb)),
                                    getBase(CAPR(cb)),
                                    getLength(CAPR(cb)),
                                    getOffset(CAPR(cb)) + GPR(rt)) then
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt))
    else
        CAPR(cd) <- setOffset(CAPR(cb), getOffset(CAPR(cb)) + GPR(rt))

-----------------------------------
-- CSetBounds
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetBounds (cd::reg, cb::reg, rt::reg) =
{
    base::bits(65)   = ZeroExtend(getBase(CAPR(cb)));
    offset::bits(65) = ZeroExtend(getOffset(CAPR(cb)));
    length::bits(65) = ZeroExtend(getLength(CAPR(cb)));
    cursor::bits(65) = base + offset;
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if cursor <+ base then
        SignalCapException(capExcLength,cb)
    else if cursor + ZeroExtend(GPR(rt)) >+ base + length then
        SignalCapException(capExcLength,cb)
    else
        CAPR(cd) <- setBounds(CAPR(cb), GPR(rt))
}

-----------------------------------
-- CClearRegs
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearRegs (regset::bits(5), mask::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else match regset
    {
        case 0 => for i in  0 .. 15 do when mask<i> do GPR([i])     <- 0
        case 1 => for i in 16 .. 31 do when mask<i-16> do GPR([i])  <- 0
        case 2 => for i in  0 .. 15 do when mask<i> do CAPR([i])    <- nullCap
        case 3 => when not register_inaccessible_write_attempt(mask) do
                       for i in 16 .. 31 do when mask<i-16> do CAPR([i]) <- nullCap
        case _ => SignalException (ResI)
    }

-----------------------------------
-- CClearTag
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearTag (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        CAPR(cd) <- setTag(CAPR(cb), false)

-----------------------------------
-- CAndPerm
-----------------------------------
define COP2 > CHERICOP2 > CSet > CAndPerm (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else
        CAPR(cd) <- setPerms(CAPR(cb), Perms(&getPerms(CAPR(cb)) && GPR(rt)<eval(NBPERMS-1):0>))

-----------------------------------
-- CSetOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if getTag(CAPR(cb)) and getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not isCapRepresentable (getSealed(CAPR(cb)),
                                    getBase(CAPR(cb)),
                                    getLength(CAPR(cb)),
                                    GPR(rt)) then
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + GPR(rt))
    else
        CAPR(cd) <- setOffset(CAPR(cb), GPR(rt))

-----------------------------------
-- CSub
-----------------------------------
define COP2 > CHERICOP2 > CSub (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else
        GPR(rd) <- getBase(CAPR(cb)) + getOffset(CAPR(cb)) - getBase(CAPR(ct)) - getOffset(CAPR(ct))

-----------------------------------
-- CCheckPerm
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckPerm (cs::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if &getPerms(CAPR(cs)) && GPR(rt)<eval(NBPERMS-1):0> <> GPR(rt)<eval(NBPERMS-1):0> then
        SignalCapException(capExcUser,cs)
    else
        nothing

-----------------------------------
-- CChecType
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckType (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if not getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if getType(CAPR(cs)) <> getType(CAPR(cb)) then
        SignalCapException(capExcType,cs)
    else
        nothing

-----------------------------------
-- CFromPtr
-----------------------------------
define COP2 > CHERICOP2 > CSet > CFromPtr (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if GPR(rt) == 0 then
        CAPR(cd) <- nullCap
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not isCapRepresentable(getSealed(CAPR(cb)), getBase(CAPR(cb)), getLength(CAPR(cb)), GPR(rt)) then
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + GPR(rt))
    else
        CAPR(cd) <- setOffset(CAPR(cb), GPR(rt))

-----------------------------------
-- CToPtr
-----------------------------------
define COP2 > CHERICOP2 > CGet > CToPtr (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if not getTag(CAPR(cb)) then
        GPR(rd) <- 0
    else
        GPR(rd) <- getBase(CAPR(cb)) + getOffset(CAPR(cb)) - getBase(CAPR(ct))

-----------------------------------
-- CPtrCmp
-----------------------------------
define COP2 > CHERICOP2 > CPtrCmp (rd::reg, cb::reg, ct::reg, t::bits(3)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        var equal = false;
        var less;
        var greater;
        var lessu;
        var greateru;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if register_inaccessible(ct) then
            SignalCapException_v(ct)
        else if getTag(CAPR(cb)) <> getTag(CAPR(ct)) then
            if getTag(CAPR(cb)) then
            {
                less <- false;
                lessu <- false;
                greater <- true;
                greateru <- true
            }
            else
            {
                less <- true;
                lessu <- true;
                greater <- false;
                greateru <- false
            }
        else
        {
            cursor1 = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64
            cursor2 = getBase(CAPR(ct)) + getOffset(CAPR(ct)); -- mod 2^64
            equal <- cursor1 == cursor2;
            less <- cursor1 < cursor2;
            greater <- cursor1 > cursor2;
            lessu <- cursor1 <+ cursor2;
            greateru <- cursor1 >+ cursor2
        };
        when not exceptionSignalled do match t
        {
           case 0 => GPR(rd) <- [equal]
           case 1 => GPR(rd) <- [not equal]
           case 2 => GPR(rd) <- [less]
           case 3 => GPR(rd) <- [less or equal]
           case 4 => GPR(rd) <- [lessu]
           case 5 => GPR(rd) <- [lessu or equal]
           case 6 => GPR(rd) <- if CAPR(cb) == CAPR(ct) then 1 else 0
           case _ => SignalException (ResI)
        }
    }

-----------------------------------
-- CBTU
-----------------------------------
define COP2 > CHERICOP2 > CBTU (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not getTag(CAPR(cb)) then
            BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CBTS
-----------------------------------
define COP2 > CHERICOP2 > CBTS (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if getTag(CAPR(cb)) then
            BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Capability then
        SignalCapException(capExcPermStoreCap,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Local_Capability
            and getTag(CAPR(cs)) and not getPerms(CAPR(cs)).Global then
        SignalCapException(capExcPermStoreLocalCap,cb)
    else
    {
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64
        extOff = offset:'0000';
        addr::bits(66) = ZeroExtend(cursor) + ZeroExtend(GPR(rt)) + SignExtend(extOff);
        if addr + CAPBYTEWIDTH > ZeroExtend(getBase(CAPR(cb)) + getLength(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if addr < ZeroExtend(getBase(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned([addr]) then
        {
            CP0.BadVAddr <- [addr];
            SignalException(AdES)
        }
        else
        {
            _ = StoreCap([addr], CAPR(cs), false);
            LLbit <- None
        }
    }

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load_Capability then
        SignalCapException(capExcPermLoadCap,cb)
    else
    {
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64
        extOff = offset:'0000';
        addr::bits(66) = ZeroExtend(cursor) + ZeroExtend(GPR(rt)) + SignExtend(extOff);
        if addr + CAPBYTEWIDTH > ZeroExtend(getBase(CAPR(cb)) + getLength(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if addr < ZeroExtend(getBase(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned([addr]) then
        {
            CP0.BadVAddr <- [addr];
            SignalException(AdEL)
        }
        else
        {
            tmp = LoadCap([addr], false);
            when not exceptionSignalled do CAPR(cd) <- tmp;
            LLbit <- None
        }
    }

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else
    {
        var access;
        var size;
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb));
        extOff = (([offset<7>]::bits(1))^3:offset) << [t];
        addr::bits(66) = ZeroExtend(cursor) + ZeroExtend(GPR(rt)) + SignExtend(extOff);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2 : '0')
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU : '00')
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD
            }
        };
        if addr + size > ZeroExtend(getBase(CAPR(cb)) + getLength(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if addr < ZeroExtend(getBase(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else
        {
            data = LoadMemoryCap(access, true, [addr], DATA, LOAD, false);
            when not exceptionSignalled do
            {
                data_list = [data]::bool list;
                bottom = ([bytesel]::nat)*8;
                top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
                final_data = data_list<top:bottom>;
                if s == 0 then GPR(rd) <- [ZeroExtendBitString(64, final_data)]
                else GPR(rd) <- [SignExtendBitString(64, final_data)]
            }
        }
    }
{-
-----------------------------------
-- CLLD
-----------------------------------
define LWC2 > CHERILWC2 > CLLD (rd::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    -- XXX bug in spec
    else if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + 8 >+ getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    -- XXX bug in spec
    else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr<2:0> <> 0 then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdEL)
    }
    else
        GPR(rd) <- LoadMemoryCap(DOUBLEWORD, addr, DATA, LOAD, true)
}
-}
-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else
    {
        var access;
        var size;
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
        extOff = (([offset<7>]::bits(1))^3:offset) << [t];
        tmp::bits(66) = SignExtend(extOff);
        addr::bits(66) = ZeroExtend(cursor) + ZeroExtend(GPR(rt)) + SignExtend(extOff);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2:'0')
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^1:'00')
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD
            }
        };
        if addr + size > ZeroExtend(getBase(CAPR(cb)) + getLength(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else if addr < ZeroExtend(getBase(CAPR(cb))) then
            SignalCapException(capExcLength,cb)
        else
        {
            _ = StoreMemoryCap(access, access, GPR(rs) << (0n8 * [bytesel]), true, [addr], DATA, STORE, false);
            nothing
        }
    }
{-
-----------------------------------
-- CSCD
-----------------------------------
define SWC2 > CHERISWC2 > CSCD (rs::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    -- XXX bug in spec
    else if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + 8 >+ getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    -- XXX bug in spec
    else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr<2:0> <> 0 then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdES)
    }
    else
        GPR(rs) <- if StoreMemoryCap(DOUBLEWORD, DOUBLEWORD, GPR(rs), addr, DATA, LOAD, true) then 1 else 0
}
-}

-----------------------------------
-- CLLC
-----------------------------------
define COP2 > CHERICOP2 > CLLC (cd::reg, cb::reg) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load_Capability then
        SignalCapException(capExcPermLoadCap,cb)
    else if addr + CAPBYTEWIDTH >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if not isCapAligned(addr) then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdEL)
    }
    else
    {
        ret_cap = LoadCap(addr, true);
        when not exceptionSignalled do CAPR(cd) <- ret_cap
    }
}

-----------------------------------
-- CLLx
-----------------------------------
define COP2 > CHERICOP2 > CLLx (rd::reg, cb::reg, stt::bits(3)) =
{
    t = stt<1:0>;
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    size = ZeroExtend(1::bits(64) << [t]);
    access_length = if t == '00' then '000' else '1'^[t];
    bytesel = match t
        {
            case '00' => addr<2:0> ?? BigEndianCPU^3
            case '01' => addr<2:0> ?? (BigEndianCPU^2:'0')
            case '10' => addr<2:0> ?? (BigEndianCPU^1:'00')
            case '11' => '000'
        };
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else if addr + size >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        data = LoadMemoryCap(access_length, true, addr, DATA, LOAD, true);
        when not exceptionSignalled do
        {
            data_list = [data]::bool list;
            bottom = ([bytesel]::nat)*8;
            top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
            final_data = data_list<[top]:[bottom]>;
            if not stt<2> then GPR(rd) <- [ZeroExtendBitString(64, [final_data])]
            else GPR(rd) <- [SignExtendBitString(64, [final_data])]
        }
    }
}

-----------------------------------
-- CSCC
-----------------------------------
define COP2 > CHERICOP2 > CSCC (cs::reg, cb::reg, rd::reg) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Capability then
        SignalCapException(capExcPermStoreCap,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Local_Capability
            and getTag(CAPR(cs)) and not getPerms(CAPR(cs)).Global then
        SignalCapException(capExcPermStoreLocalCap,cb)
    else if addr + CAPBYTEWIDTH >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if not isCapAligned(addr) then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdES)
    }
    else
    {
        ret = if StoreCap(addr, CAPR(cs), true) then 1 else 0;
        when not exceptionSignalled do GPR(rd) <- ret
    }
}

-----------------------------------
-- CSCx
-----------------------------------
define COP2 > CHERICOP2 > CSCx (rs::reg, cb::reg, rd::reg, t::bits(2)) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    size = ZeroExtend(1::bits(64) << [t]);
    access_length = if t == '00' then '000' else '1'^[t];
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else if addr + size >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        ret = if StoreMemoryCap(access_length, access_length,
                                     GPR(rs), true, addr, DATA, STORE, true)
                                     then 1 else 0;
        when not exceptionSignalled do GPR(rd) <- ret
    }
}

-----------------------------------
-- CJR
-----------------------------------
define COP2 > CHERICOP2 > CJR (cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not getTag(CAPR(cb)) then
            SignalCapException(capExcTag,cb)
        else if getSealed(CAPR(cb)) then
            SignalCapException(capExcSeal,cb)
        else if not getPerms(CAPR(cb)).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if not getPerms(CAPR(cb)).Global then
            SignalCapException(capExcGlobal,cb)
        else if getOffset(CAPR(cb)) + 4 >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if (getBase(CAPR(cb)) + getOffset(CAPR(cb)))<1:0> <> '00' then
        {
            CP0.BadVAddr <- (getBase(CAPR(cb)) + getOffset(CAPR(cb)));
            SignalException(AdEL)
        }
        else
        {
            BranchToPCC <- Some (getOffset(CAPR(cb)), CAPR(cb))
        }
    }

-----------------------------------
-- CJALR
-----------------------------------
define COP2 > CHERICOP2 > CJALR (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cd) then
            SignalCapException_v(cd)
        else if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not getTag(CAPR(cb)) then
            SignalCapException(capExcTag,cb)
        else if getSealed(CAPR(cb)) then
            SignalCapException(capExcSeal,cb)
        else if not getPerms(CAPR(cb)).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if not getPerms(CAPR(cb)).Global then
            SignalCapException(capExcGlobal,cb)
        else if getOffset(CAPR(cb)) + 4 >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if (getBase(CAPR(cb)) + getOffset(CAPR(cb)))<1:0> <> '00' then
        {
            CP0.BadVAddr <- (getBase(CAPR(cb)) + getOffset(CAPR(cb)));
            SignalException(AdEL)
        }
        else
        {
            CAPR(cd) <- setOffset(PCC, PC + 8);
            BranchToPCC <- Some (getOffset(CAPR(cb)), CAPR(cb))
        }
    }

-----------------------------------
-- CSeal
-----------------------------------
define COP2 > CHERICOP2 > CSeal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if getSealed(CAPR(ct)) then
        SignalCapException(capExcSeal,ct)
    else if not getPerms(CAPR(ct)).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if getOffset(CAPR(ct)) >=+ getLength(CAPR(ct)) then
        SignalCapException(capExcLength,ct)
    else if (getBase(CAPR(ct)) + getOffset(CAPR(ct))) >=+ eval(2**OTYPEWIDTH) then
        SignalCapException(capExcLength,ct)
    else if not isCapRepresentable (true,
                                    getBase(CAPR(cs)),
                                    getLength(CAPR(cs)),
                                    getOffset(CAPR(cs))) then
        SignalCapException(capExcInexact,cs)
    else
    {
        var new_cap = CAPR(cs);
        new_cap  <- setSealed(new_cap, true);
        new_cap  <- setType(new_cap, (getBase(CAPR(ct)) + getOffset(CAPR(ct)))<eval(OTYPEWIDTH-1):0>);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CUnseal
-----------------------------------
define COP2 > CHERICOP2 > CUnseal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if getSealed(CAPR(ct)) then
        SignalCapException(capExcSeal,ct)
    else if (getBase(CAPR(ct)) + getOffset(CAPR(ct)))<eval(OTYPEWIDTH-1):0> <> getType(CAPR(cs)) then
        SignalCapException(capExcType,ct)
    else if not getPerms(CAPR(ct)).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if getOffset(CAPR(ct)) >=+ getLength(CAPR(ct)) then
        SignalCapException(capExcLength,ct)
    else if (getBase(CAPR(ct)) + getOffset(CAPR(ct))) >=+ eval(2**OTYPEWIDTH) then -- XXX probably redundant with the inequality test up there
        SignalCapException(capExcLength,ct)
    else
    {
        var new_cap = CAPR(cs);
        new_cap <- setSealed(new_cap, false);
        new_cap <- setType(new_cap, 0);
        var p::Perms = getPerms(new_cap);
        p.Global <- getPerms(CAPR(cs)).Global and getPerms(CAPR(ct)).Global;
        new_cap  <- setPerms(new_cap, p);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CCall
-----------------------------------
define COP2 > CHERICOP2 > CCall (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalCapException(capExcCall,cs)

-----------------------------------
-- CReturn
-----------------------------------
define COP2 > CHERICOP2 > CReturn =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalCapException_noReg(capExcRet)

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERICOP2 > UnknownCapInstruction =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalException (ResI)
