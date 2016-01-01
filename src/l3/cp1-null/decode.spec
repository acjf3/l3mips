---------------------------------------------------------------------------
-- Stub for a decoder for the floating point instructions.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

instruction COP1Decode (v::bits(26)) = COP1()

instruction COP3Decode (v::bits(26)) = COP1()

instruction LDC1Decode (base::reg, offset::bits(16), ft::reg) = COP1()

instruction LWC1Decode (base::reg, offset::bits(16), ft::reg) = COP1()

instruction SDC1Decode (base::reg, offset::bits(16), ft::reg) = COP1()

instruction SWC1Decode (base::reg, offset::bits(16), ft::reg) = COP1()

instruction MOVCIDecode (rs:: bits(5), rt::bits(5), rd::bits(5)) = COP1()
