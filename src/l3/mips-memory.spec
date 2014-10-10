---------------------------------------------------------------------------
-- MIPS default memory
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

declare MEM :: mAddr -> dword -- physical memory (37 bits), doubleword access

unit InitMEM = MEM <- InitMap (0x0) 
dword ReadData (pAddr::mAddr) = MEM(pAddr)
unit WriteData (pAddr::mAddr, data::dword, mask::dword) = MEM(pAddr) <- MEM(pAddr) && ~mask || data && mask
word ReadInst (a::pAddr) = if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>
