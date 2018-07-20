---------------------------------------------------------------------------
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Coprocessor register access
--------------------------------------------------

unit mtc (n::nat, reg::bits(5), sel::bits(3), src::bits(5)) =
  when n == 0 do
    if getPerms(PCC).Access_System_Registers then CP0R(reg, sel) <- GPR(src)
    else SignalCapException_noReg(capExcAccessSysReg)

unit dmtc (n::nat, reg::bits(5), sel::bits(3), src::bits(5)) =
  mtc(n, reg, sel, src)

unit mfc (n::nat, reg::bits(5), sel::bits(3), dest::bits(5)) =
  if n == 0 then
    if getPerms(PCC).Access_System_Registers then GPR(dest) <- SignExtend(CP0R(reg, sel)<31:0>)
    else SignalCapException_noReg(capExcAccessSysReg)
  else GPR(dest) <- UNKNOWN(next_unknown("cop-reg"))

unit dmfc (n::nat, reg::bits(5), sel::bits(3), dest::bits(5)) =
  if n == 0 then
    if getPerms(PCC).Access_System_Registers then GPR(dest) <- CP0R(reg, sel)
    else SignalCapException_noReg(capExcAccessSysReg)
  else GPR(dest) <- UNKNOWN(next_unknown("cop-reg"))
