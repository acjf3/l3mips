---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

include(`cap-params.m4')dnl
-------------------
-- Helper functions
-------------------

bool register_inaccessible(cb::reg) =
{
    perms = getPerms(PCC);
    return (cb == 31 and not perms.Access_EPCC
         or cb == 30 and not perms.Access_KDC
         or cb == 29 and not perms.Access_KCC
         or cb == 27 and not perms.Access_KR1C
         or cb == 28 and not perms.Access_KR2C)
}

-- only works for non empty lists
bool list SignExtendBitString(w::nat, x::bool list) = PadLeft (Head(x), w, x)

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
    {
        GPR(rd)<0> <- getTag(CAPR(cb));
        GPR(rd)<63:1> <- 0
    }

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
    {
        var new_cap = PCC;
        new_cap <- setOffset(new_cap, PC);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if not getPerms(PCC).Access_EPCC then
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
    else if not getPerms(PCC).Access_EPCC then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        capcause.ExcCode <- GPR(rt)<15:8>;
        capcause.RegNum <- GPR(rt)<7:0>
    }

-----------------------------------
-- CIncBase
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncBase (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not getTag(CAPR(cb)) and GPR(rt) <> 0 then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) and GPR(rt) <> 0 then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        var new_cap     = CAPR(cb);
        new_cap <- setBase(new_cap, getBase(CAPR(cb)) + GPR(rt));
        new_cap <- setLength(new_cap, getLength(CAPR(cb)) - GPR(rt));
        CAPR(cd) <- new_cap
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
    else
    {
        var new_cap     = CAPR(cb);
        new_cap <- setOffset(new_cap, getOffset(CAPR(cb)) + GPR(rt));
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CSetLen
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetLen (cd::reg, cb::reg, rt::reg) =
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
    else if GPR(rt) >+ getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap <- setLength(new_cap, GPR(rt));
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CSetBounds
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetBounds (cd::reg, cb::reg, rt::reg) =
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
{- XXX that is in the current spec
    else if (getBase(CAPR(cb)) + getOffset(CAPR(cb))) <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if GPR(rt) >+ (getLength(CAPR(cb)) - getOffset(CAPR(cb))) then
        SignalCapException(capExcLength,cb)
-}
-- XXX that is not
    else if (getBase(CAPR(cb))+getOffset(CAPR(cb)) <+ getBase(CAPR(cb))) then
        SignalCapException(capExcLength,cb)
    else if getBase(CAPR(cb))+getOffset(CAPR(cb)) >+ (getBase(CAPR(cb))+getLength(CAPR(cb))) then
        SignalCapException(capExcLength,cb)
    else if ((getBase(CAPR(cb))+getOffset(CAPR(cb))+GPR(rt)) <+ getBase(CAPR(cb))) then
        SignalCapException(capExcLength,cb)
    else if ((getBase(CAPR(cb))+getOffset(CAPR(cb))+GPR(rt)) >+ (getBase(CAPR(cb))+getLength(CAPR(cb)))) then
        SignalCapException(capExcLength,cb)
    else if (getBase(CAPR(cb))+getOffset(CAPR(cb)) >+ getBase(CAPR(cb))+getOffset(CAPR(cb))+GPR(rt)) then
        SignalCapException(capExcLength,cb)
    else
    {
        {- XXX implementation 1
        var new_cap = CAPR(cb);
        new_cap <- setBase(new_cap, getBase(CAPR(cb))+getOffset(CAPR(cb)));
        new_cap <- setLength(new_cap, GPR(rt));
        new_cap <- setOffset(new_cap, 0);
        CAPR(cd) <- new_cap
        -}
        -- XXX implemetation 2
        CAPR(cd) <- setBounds(CAPR(cb), GPR(rt))
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
    {
        var new_cap = CAPR(cb);
        new_cap <- setTag(new_cap, false);
        CAPR(cd) <- new_cap
    }

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
    {
        var new_cap = CAPR(cb);
        new_cap <- setPerms(new_cap, Perms(&getPerms(CAPR(cb)) && GPR(rt)<eval(NBPERMS-1):0>));
        CAPR(cd) <- new_cap
    }

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
    else
    {
        var new_cap = CAPR(cb);
        new_cap <- setOffset(new_cap, GPR(rt));
        CAPR(cd) <- new_cap
    }

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
    {
        var new_cap::Capability;
        new_cap <- setTag(new_cap, false);
        new_cap <- setSealed(new_cap, false);
        new_cap <- setPerms(new_cap, Perms(0));
        new_cap <- setBase(new_cap, 0);
        new_cap <- setLength(new_cap, 0);
        new_cap <- setOffset(new_cap, 0);
        new_cap <- setType(new_cap, 0);
        --new_cap <- setReserved(new_cap, 0); -- XXX reserved
        CAPR(cd) <- new_cap
    }
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
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
        match t
        {
           case 0 => GPR(rd) <- [equal]
           case 1 => GPR(rd) <- [not equal]
           case 2 => GPR(rd) <- [less]
           case 3 => GPR(rd) <- [less or equal]
           case 4 => GPR(rd) <- [lessu]
           case 5 => GPR(rd) <- [lessu or equal]
           case _ => nothing
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
            if PC + SignExtend(offset) + 4 >+ getLength(PCC) then
                SignalCapException_noReg(capExcLength)
            else
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
            if PC + SignExtend(offset) + 4 >+ getLength(PCC) then
                SignalCapException_noReg(capExcLength)
            else
                BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    if register_inaccessible(cs) then
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
        addr = cursor + GPR(rt) + SignExtend(offset);
        -- XXX bug in spec
        if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + CAPBYTEWIDTH >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        -- XXX bug in spec
        else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned(addr) then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdES)
        }
        else
        {
            StoreCap(addr,CAPR(cs));
            LLbit <- None
        }
    }

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    if register_inaccessible(cd) then
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
        addr = cursor + GPR(rt) + SignExtend(offset);
        -- XXX bug in spec
        if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + CAPBYTEWIDTH >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        -- XXX bug in spec
        else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned(addr) then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdEL)
        }
        else
        {
            tmp = LoadCap(addr);
            when not exceptionSignalled do CAPR(cd) <- tmp;
            LLbit <- None
        }
    }

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    if register_inaccessible(cb) then
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
        var aligned;
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
        var addr = cursor + GPR(rt) + SignExtend(offset);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3;
                aligned <- true
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2 : '0');
                aligned <- addr<0> == false
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU : '00');
                aligned <- addr<1:0> == 0
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD;
                aligned <- addr<2:0> == 0
            }
        };
        -- XXX bug in spec
        if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + size >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        -- XXX bug in spec
        else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not aligned then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdEL)
        }
        else
        {
            data = LoadMemoryCap(access, addr, DATA, LOAD, false);
            when not exceptionSignalled do
            {
                data_list = [data]::bool list;
                bottom = ([bytesel]::nat)*8;
                top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
                final_data = data_list<top:bottom>;
                if s == 0 then GPR(rd) <- [final_data]
                else GPR(rd) <- [SignExtendBitString(64, final_data)]
            }
        }
    }

-----------------------------------
-- CLLD
-----------------------------------
define LWC2 > CHERILWC2 > CLLD (rd::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if register_inaccessible(cb) then
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

-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    if register_inaccessible(cb) then
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
        var aligned;
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
        var addr = cursor + GPR(rt) + SignExtend(offset);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3;
                aligned <- true
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2:'0');
                aligned <- addr<0> == false
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^1:'00');
                aligned <- addr<1:0> == 0
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD;
                aligned <- addr<2:0> == 0
            }
        };
        -- XXX bug in spec
        if getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) + size >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        -- XXX bug in spec
        else if getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt) + SignExtend(offset) <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not aligned then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdES)
        }
        else
        {
            _ = StoreMemoryCap(access, access, GPR(rs) << (0n8 * [bytesel]), addr, DATA, STORE, false);
            nothing
        }
    }

-----------------------------------
-- CSCD
-----------------------------------
define SWC2 > CHERISWC2 > CSCD (rs::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb)); -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if register_inaccessible(cb) then
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
            var new_cap = PCC;
            new_cap <- setOffset(new_cap, PC + 8);
            CAPR(cd) <- new_cap;
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
    else
    {
        var new_cap = CAPR(cs);
        new_cap <- setSealed(new_cap, true);
        new_cap <- setType(new_cap, (getBase(CAPR(ct)) + getOffset(CAPR(ct)))<eval(OTYPEWIDTH-1):0>);
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
        new_cap <- setPerms(new_cap, p);
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
