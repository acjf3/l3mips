---------------------------------------------------------------------------
-- The standard MIPS instructions implementation
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------------------
-- MTC0 rt, rd
-- MTC0 rt, rd, sel
-----------------------------------
define CP > MTC0 (rt::reg, rd::reg, sel::bits(3)) =
  -- Will need adapting for EntryLo1 and EntryLo0
  if CP0.Status.CU0 or KernelMode then
    mtc (0, rd, sel, rt)
  else
    SignalException (CpU)

-----------------------------------
-- DMTC0 rt, rd
-- DMTC0 rt, rd, sel
-----------------------------------
define CP > DMTC0 (rt::reg, rd::reg, sel::bits(3)) =
  if CP0.Status.CU0 or KernelMode then
    dmtc (0, rd, sel, rt)
  else
    SignalException (CpU)

-----------------------------------
-- MFC0 rt, rd
-- MFC0 rt, rd, sel
-----------------------------------
define CP > MFC0 (rt::reg, rd::reg, sel::bits(3)) =
  -- Will need adapting for EntryLo1 and EntryLo0; see manual entry for MFC0
  if CP0.Status.CU0 or KernelMode then
    mfc (0, rd, sel, rt)
  else
    SignalException (CpU)

-----------------------------------
-- DMFC0 rt, rd
-- DMFC0 rt, rd, sel
-----------------------------------
define CP > DMFC0 (rt::reg, rd::reg, sel::bits(3)) =
  if CP0.Status.CU0 or KernelMode then
    dmfc (0, rd, sel, rt)
  else
    SignalException (CpU)
