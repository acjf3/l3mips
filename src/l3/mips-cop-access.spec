---------------------------------------------------------------------------
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Coprocessor register access
--------------------------------------------------

unit mtc (n::nat, reg::bits(5), sel::bits(3), src::bits(5)) =
  when n == 0 do CP0R(reg, sel) <- GPR(src)

unit dmtc (n::nat, reg::bits(5), sel::bits(3), src::bits(5)) =
  mtc(n, reg, sel, src)

unit mfc (n::nat, reg::bits(5), sel::bits(3), dest::bits(5)) =
  GPR(dest) <-
    if n == 0 then SignExtend(CP0R(reg, sel)<31:0>)
    else UNKNOWN(next_unknown("cop-reg"))

unit dmfc (n::nat, reg::bits(5), sel::bits(3), dest::bits(5)) =
  GPR(dest) <-
    if n == 0 then CP0R(reg, sel)
    else UNKNOWN(next_unknown("cop-reg"))
