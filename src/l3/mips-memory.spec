---------------------------------------------------------------------------
-- MIPS default memory
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

declare MEM :: mAddr -> dword -- physical memory (37 bits), doubleword access

unit InitMEM = MEM <- InitMap (0x0) 

dword ReadData (pAddr::mAddr) =
{
    data = MEM(pAddr);
    mark_log (2, log_r_mem (pAddr, data));
    data
}

unit WriteData (pAddr::mAddr, data::dword, mask::dword) =
{
    MEM(pAddr) <- MEM(pAddr) && ~mask || data && mask;
    mark_log (2, log_w_mem (pAddr, mask, data))
}

word ReadInst (a::pAddr) = if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>
