---------------------------------------------------------------------------
-- Stub for a disassembler for the floating point instructions.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

string COP1InstructionToString (i::instruction) = "default COP1 instruction"

word COP1Encode (i::instruction) = '010001' : 0
