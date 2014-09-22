---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool register_inaccessible(cb::reg) =
{
    var perms::Perms;
    &perms <- PCC.perms;
    return (((cb == 31) and not perms.Access_EPCC)
     or ((cb == 30) and not perms.Access_KDC)
     or ((cb == 29) and not perms.Access_KCC)
     or ((cb == 28) and not perms.Access_KR2C)
     or ((cb == 27) and not perms.Access_KR1C))
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
    var perms::Perms;
    &perms <- PCC.perms;
    if not perms.Access_EPCC then
        SignalCapException(capExcAccEPCC,0xff)
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
    ()

-----------------------------------
-- CIncBase
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncBase (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CSetLen
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetLen (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CClearTag
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearTag (cd::reg, cb::reg) =
    ()

-----------------------------------
-- CAndPerm
-----------------------------------
define COP2 > CHERICOP2 > CSet > CAndPerm (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CSetOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetOffset (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CSetType
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetType (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CCheckPerm
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckPerm (cs::reg, rt::reg) =
    ()

-----------------------------------
-- CChecType
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckType (cs::reg, cb::reg) =
    ()

-----------------------------------
-- CFromPtr
-----------------------------------
define COP2 > CHERICOP2 > CSet > CFromPtr (cd::reg, cb::reg, rt::reg) =
    ()

-----------------------------------
-- CToPtr
-----------------------------------
define COP2 > CHERICOP2 > CGet > CToPtr (rd::reg, cb::reg, ct::reg) =
    ()

-----------------------------------
-- CPtrCmp
-----------------------------------
define COP2 > CHERICOP2 > CPtrCmp (rd::reg, cb::reg, ct::reg, t::bits(3)) =
    ()

-----------------------------------
-- CBTU
-----------------------------------
define COP2 > CHERICOP2 > CBTU (cb::reg, offset::bits(16)) =
    ()

-----------------------------------
-- CBTS
-----------------------------------
define COP2 > CHERICOP2 > CBTS (cb::reg, offset::bits(16)) =
    ()

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    ()

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    ()

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    ()

-----------------------------------
-- CLLD
-----------------------------------
define LWC2 > CHERILWC2 > CLLD (rd::reg, cb::reg, rt::reg, offset::bits(8)) =
    ()

-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    ()

-----------------------------------
-- CSCD
-----------------------------------
define SWC2 > CHERISWC2 > CSCD (rs::reg, cb::reg, rt::reg, offset::bits(8)) =
    ()

-----------------------------------
-- CJR
-----------------------------------
define COP2 > CHERICOP2 > CJR (cb::reg) =
    ()

-----------------------------------
-- CJALR
-----------------------------------
define COP2 > CHERICOP2 > CJALR (cd::reg, cb::reg) =
    ()

-----------------------------------
-- CSealCode
-----------------------------------
define COP2 > CHERICOP2 > CSealCode (cd::reg, cs::reg) =
    ()

-----------------------------------
-- CSealData
-----------------------------------
define COP2 > CHERICOP2 > CSealData (cd::reg, cs::reg, ct::reg) =
    ()

-----------------------------------
-- CUnseal
-----------------------------------
define COP2 > CHERICOP2 > CUnseal (cd::reg, cs::reg, ct::reg) =
    ()

-----------------------------------
-- CCall
-----------------------------------
define COP2 > CHERICOP2 > CCall (cs::reg, cb::reg) =
    ()

-----------------------------------
-- CReturn
-----------------------------------
define COP2 > CHERICOP2 > CReturn =
    ()

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERICOP2 > UnknownCapInstruction =
   SignalException (ResI)
