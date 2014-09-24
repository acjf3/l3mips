---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool register_inaccessible(cb::reg) =
{
    perms = Perms(PCC.perms);
    return (cb == 31 and not perms.Access_EPCC
         or cb == 30 and not perms.Access_KDC
         or cb == 29 and not perms.Access_KCC
         or cb == 28 and not perms.Access_KR2C
         or cb == 27 and not perms.Access_KR1C)
}

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetBase (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).base

-----------------------------------
-- CGetOffset rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetOffset (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).offset

-----------------------------------
-- CGetLen rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetLen (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).length

-----------------------------------
-- CGetTag rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetTag (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<0> <- CAPR(cb).tag;
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetSealed rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetSealed (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<0> <- CAPR(cb).sealed;
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetPerm rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPerm (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<14:0> <- CAPR(cb).perms<14:0>;
        GPR(rd)<63:15> <- 0
    }

-----------------------------------
-- CGetType rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetType (rd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<23:0> <- CAPR(cb).otype;
        GPR(rd)<63:24> <- 0
    }

-----------------------------------
-- CGetPCC cd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCC (cd::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else
    {
        CAPR(cd) <- PCC;
        CAPR(cd).offset <- PC
    }

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
{
    perms = Perms(PCC.perms);
    if not perms.Access_EPCC then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        GPR(rd)<7:0> <- capcause.RegNum;
        GPR(rd)<15:8> <- capcause.ExcCode;
        GPR(rd)<63:16> <- 0
    }
}

-----------------------------------
-- CSetCause rt
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetCause (rt::reg) =
{
    perms = Perms(PCC.perms);
    if not perms.Access_EPCC then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        capcause.ExcCode <- GPR(rt)<15:8>;
        capcause.RegNum <- GPR(rt)<7:0>
    }
}

-----------------------------------
-- CIncBase
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncBase (cd::reg, cb::reg, rt::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag and GPR(rt) <> 0 then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed and GPR(rt) <> 0 then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).base <- CAPR(cb).base + GPR(rt);
        CAPR(cd).length <- CAPR(cb).length - GPR(rt)
    }

-----------------------------------
-- CSetLen
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetLen (cd::reg, cb::reg, rt::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).length <- GPR(rt)
    }

-----------------------------------
-- CClearTag
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearTag (cd::reg, cb::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).tag <- false
    }

-----------------------------------
-- CAndPerm
-----------------------------------
define COP2 > CHERICOP2 > CSet > CAndPerm (cd::reg, cb::reg, rt::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).perms <- CAPR(cd).perms && GPR(rt)<30:0>
    }

-----------------------------------
-- CSetOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetOffset (cd::reg, cb::reg, rt::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).offset <- GPR(rt)
    }

-----------------------------------
-- CSetType
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetType (cd::reg, cb::reg, rt::reg) =
    nothing

-----------------------------------
-- CCheckPerm
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckPerm (cs::reg, rt::reg) =
    if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if CAPR(cs).perms && GPR(rt)<30:0> <> GPR(rt)<30:0> then
        SignalCapException(capExcUser,cs)
    else
        nothing

-----------------------------------
-- CChecType
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckType (cs::reg, cb::reg) =
    if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if not CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if not CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if CAPR(cs).otype <> CAPR(cb).otype then
        SignalCapException(capExcType,cs)
    else
        nothing

-----------------------------------
-- CFromPtr
-----------------------------------
define COP2 > CHERICOP2 > CSet > CFromPtr (cd::reg, cb::reg, rt::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if GPR(rt) == 0 then
    {
        CAPR(cd).tag <- false;
        CAPR(cd).sealed <- false;
        CAPR(cd).perms <- 0;
        CAPR(cd).base <- 0;
        CAPR(cd).length <- 0;
        CAPR(cd).offset <- 0;
        CAPR(cd).otype <- 0;
        CAPR(cd).reserved <- 0
    }
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if not CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) > CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        CAPR(cd) <- CAPR(cb);
        CAPR(cd).base <- CAPR(cb).base + GPR(rt);
        CAPR(cd).length <- CAPR(cb).length - GPR(rt)
    }

-----------------------------------
-- CToPtr
-----------------------------------
define COP2 > CHERICOP2 > CGet > CToPtr (rd::reg, cb::reg, ct::reg) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if not CAPR(cb).tag then
        GPR(rd) <- 0
    else
        GPR(rd) <- CAPR(cb).base + CAPR(cb).offset - CAPR(ct).base

-----------------------------------
-- CPtrCmp
-----------------------------------
define COP2 > CHERICOP2 > CPtrCmp (rd::reg, cb::reg, ct::reg, t::bits(3)) =
{
    var equal;
    var less;
    var greater;
    var lessu;
    var greateru;
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if CAPR(cb).tag <> CAPR(ct).tag then
    {
        equal = false;
        if CAPR(cb).tag then
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
    }
    else
    {
        cursor1 = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64
        cursor2 = CAPR(ct).base + CAPR(ct).offset; -- mod 2^64
        equal <- cursor1 == cursor2;
        less <- cursor1 < cursor2;
        greater <- cursor1 > cursor2;
        lessu <- cursor1 <+ cursor2;
        greateru <- cursor1 >+ cursor2
    };
    if t == 0 then
        GPR(rd) <- [equal]
    else if t == 1 then
        GPR(rd) <- [not equal]
    else if t == 2 then
        GPR(rd) <- [less]
    else if t == 3 then
        GPR(rd) <- [less or equal]
    else if t == 4 then
        GPR(rd) <- [lessu]
    else if t == 5 then
        GPR(rd) <- [lessu or equal]
    else
        nothing
}

-----------------------------------
-- CBTU
-----------------------------------
define COP2 > CHERICOP2 > CBTU (cb::reg, offset::bits(16)) =
{
    CheckBranch;
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        if PC + SignExtend(offset) + 4 >+ PCC.length then
            SignalCapException_noReg(capExcLength)
        else
            BranchTo <- Some (PC + SignExtend(offset) << 2)
    else
        nothing
}

-----------------------------------
-- CBTS
-----------------------------------
define COP2 > CHERICOP2 > CBTS (cb::reg, offset::bits(16)) =
{
    CheckBranch;
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if CAPR(cb).tag then
        if PC + SignExtend(offset) + 4 >+ PCC.length then
            SignalCapException_noReg(capExcLength)
        else
            BranchTo <- Some (PC + SignExtend(offset) << 2)
    else
        nothing
}

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    nothing

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    nothing

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    nothing

-----------------------------------
-- CLLD
-----------------------------------
define LWC2 > CHERILWC2 > CLLD (rd::reg, cb::reg, rt::reg, offset::bits(8)) =
    nothing

-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    nothing

-----------------------------------
-- CSCD
-----------------------------------
define SWC2 > CHERISWC2 > CSCD (rs::reg, cb::reg, rt::reg, offset::bits(8)) =
    nothing

-----------------------------------
-- CJR
-----------------------------------
define COP2 > CHERICOP2 > CJR (cb::reg) =
{
    CheckBranch;
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if Perms(CAPR(cb).perms).Permit_Execute then
        SignalCapException(capExcPermExe,cb)
    else if Perms(CAPR(cb).perms).Global then
        SignalCapException(capExcGlobal,cb)
    else if CAPR(cb).offset + 4 >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else if (CAPR(cb).base + CAPR(cb).offset)<1:0> <> '00' then
        SignalException(AdEL)
    else
    {
        PCC <- CAPR(cb);
        BranchTo <- Some (CAPR(cb).offset)
    }
}

-----------------------------------
-- CJALR
-----------------------------------
define COP2 > CHERICOP2 > CJALR (cd::reg, cb::reg) =
{
    CheckBranch;
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if Perms(CAPR(cb).perms).Permit_Execute then
        SignalCapException(capExcPermExe,cb)
    else if Perms(CAPR(cb).perms).Global then
        SignalCapException(capExcGlobal,cb)
    else if CAPR(cb).offset + 4 >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else if (CAPR(cb).base + CAPR(cb).offset)<1:0> <> '00' then
        SignalException(AdEL)
    else
    {
        CAPR(cd) <- PCC;
        CAPR(cd).offset <- PC;
        PCC <- CAPR(cb);
        BranchTo <- Some (CAPR(cb).offset)
    }
}

-----------------------------------
-- CSeal
-----------------------------------
define COP2 > CHERICOP2 > CSeal (cd::reg, cs::reg, ct::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if CAPR(ct).sealed then
        SignalCapException(capExcSeal,ct)
    else if CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if not Perms(CAPR(ct).perms).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if CAPR(ct).offset >=+ CAPR(ct).length then
        SignalCapException(capExcLength,ct)
{- FIXME
    else if (CAPR(ct).base + CAPR(ct).offset) >+= 16777216 then
        SignalCapException(capExcLength,ct)
-}
    else
    {
        CAPR(cd) <- CAPR(cs);
        CAPR(cd).sealed <- true;
        CAPR(cd).otype <- (CAPR(ct).base + CAPR(ct).offset)<23:0>
    }

-----------------------------------
-- CUnseal
-----------------------------------
define COP2 > CHERICOP2 > CUnseal (cd::reg, cs::reg, ct::reg) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if CAPR(ct).sealed then
        SignalCapException(capExcSeal,ct)
    else if CAPR(ct).offset >=+ CAPR(ct).length then
        SignalCapException(capExcLength,ct)
    else if (CAPR(ct).base + CAPR(ct).offset)<23:0> <> CAPR(cs).otype then
        SignalCapException(capExcType,ct)
    else if not Perms(CAPR(ct).perms).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else
    {
        CAPR(cd) <- CAPR(cs);
        CAPR(cd).sealed <- false;
        CAPR(cd).otype <- 0;
        Perms(CAPR(cd).perms).Global <- Perms(CAPR(cs).perms).Global and Perms(CAPR(ct).perms).Global 
    }

-----------------------------------
-- CCall
-----------------------------------
define COP2 > CHERICOP2 > CCall (cs::reg, cb::reg) =
    nothing

-----------------------------------
-- CReturn
-----------------------------------
define COP2 > CHERICOP2 > CReturn =
    nothing

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERICOP2 > UnknownCapInstruction =
   SignalException (ResI)
