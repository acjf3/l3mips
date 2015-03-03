--------------------------------------------------------------------------------
-- CHERI default memory
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------


-----------------
-- stats utils --
-----------------

record MemStats
{
    data_reads  :: nat
    data_writes :: nat
    inst_reads  :: nat
    cap_reads   :: nat
    cap_writes  :: nat
}

declare memStats :: MemStats

unit initMemStats =
{
    memStats.data_reads  <- 0;
    memStats.data_writes <- 0;
    memStats.inst_reads  <- 0;
    memStats.cap_reads   <- 0;
    memStats.cap_writes  <- 0
}

string printMemStats =
    PadRight (#" ", 16, "data_reads")  : " = " : PadLeft (#" ", 9, [memStats.data_reads::nat])  : "\\n" :
    PadRight (#" ", 16, "data_writes") : " = " : PadLeft (#" ", 9, [memStats.data_writes::nat]) : "\\n" :
    PadRight (#" ", 16, "inst_reads")  : " = " : PadLeft (#" ", 9, [memStats.inst_reads::nat])  : "\\n" :
    PadRight (#" ", 16, "cap_reads")   : " = " : PadLeft (#" ", 9, [memStats.cap_reads::nat])   : "\\n" :
    PadRight (#" ", 16, "cap_writes")  : " = " : PadLeft (#" ", 9, [memStats.cap_writes::nat])

---------------------------
-- memory implementation --
---------------------------

type dwordAddr = bits(37)
type capAddr   = bits(35)

declare MEM :: capAddr -> bits(257) -- physical memory (35 bits), capability access

unit InitMEM =
{
    initMemStats;
    MEM <- InitMap (UNKNOWN)
}

dword ReadData (pAddr::dwordAddr) =
{
    memStats.data_reads <- memStats.data_reads + 1;
    data_blob = MEM(pAddr<36:2>);
    data = match pAddr<1:0>
    {
        case '00' => data_blob<63:0>
        case '01' => data_blob<127:64>
        case '10' => data_blob<191:128>
        case '11' => data_blob<255:192>
    };
    data
}

unit WriteData (pAddr::dwordAddr, data::dword, mask::dword) =
{
    memStats.data_writes <- memStats.data_writes + 1;
    data_blob = MEM(pAddr<36:2>);
    match pAddr<1:0>
    {
        case '00' => MEM(pAddr<36:2>) <- '0' : data_blob<255:64>  : (data_blob<63:0>    && ~mask || data && mask)
        case '01' => MEM(pAddr<36:2>) <- '0' : data_blob<255:128> : (data_blob<127:64>  && ~mask || data && mask) : data_blob<63:0>
        case '10' => MEM(pAddr<36:2>) <- '0' : data_blob<255:192> : (data_blob<191:128> && ~mask || data && mask) : data_blob<127:0>
        case '11' => MEM(pAddr<36:2>) <- '0' : (data_blob<255:192> && ~mask || data && mask) : data_blob<191:0>
    }
}

word ReadInst (a::pAddr) =
{
    memStats.inst_reads <- memStats.inst_reads + 1;
    data_blob = MEM(a<39:5>);
    match a<4:2>
    {
        case '000' => data_blob<63:32>
        case '001' => data_blob<31:0>
        case '010' => data_blob<127:96>
        case '011' => data_blob<95:64>
        case '100' => data_blob<191:160>
        case '101' => data_blob<159:128>
        case '110' => data_blob<255:224>
        case '111' => data_blob<223:192>
    }
}

Capability ReadCap (capAddr::capAddr) =
{
    memStats.cap_reads <- memStats.cap_reads + 1;
    raw = MEM(capAddr);
    Capability((if raw<256> then '1' else '0') : raw<63:0> : raw<127:64> : raw<191:128> : raw<255:192>)
}

unit WriteCap (capAddr::capAddr, cap::Capability) =
{
    memStats.cap_writes <- memStats.cap_writes + 1;
    raw = &cap;
    MEM(capAddr) <- ((if raw<256> then '1' else '0') : raw<63:0> : raw<127:64> : raw<191:128> : raw<255:192>)
}

-- sml helper function
unit WriteDWORD (pAddr::dwordAddr, data::dword) =
{
    data_blob = MEM(pAddr<36:2>);
    match pAddr<1:0>
    {
        case '00' => MEM(pAddr<36:2>) <- '0' : data_blob<255:64>  : data
        case '01' => MEM(pAddr<36:2>) <- '0' : data_blob<255:128> : data : data_blob<63:0>
        case '10' => MEM(pAddr<36:2>) <- '0' : data_blob<255:192> : data : data_blob<127:0>
        case '11' => MEM(pAddr<36:2>) <- '0' : data : data_blob<191:0>
    }
}

-- sml helper function
unit Write256 (addr::bits(35), data::bits(256)) = MEM(addr) <- '0' : data
