---------------------------------------------------------------------------
-- Place holder decoders for coprocessor 2
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

instruction COP2Decode (v::bits(26)) = COP2()
instruction LWC2Decode (rd::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
  LWC2()
instruction SWC2Decode (rs::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
  SWC2()
instruction LDC2Decode (a::reg * reg * reg * bits(11)) = LDC2()
instruction SDC2Decode (a::reg * reg * reg * bits(11)) = SDC2()
