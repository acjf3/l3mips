---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetBase (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetOffset rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetOffset (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetLen rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetLen (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetTag rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetTag (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetSealed rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetSealed (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetPerm rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPerm (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetType rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetType (rd::reg, cb::reg) =
    ()

-----------------------------------
-- CGetPCC cd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCC (cd::reg) =
    ()

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
    ()

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
