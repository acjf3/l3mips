---------------------------------------------------------------------------
-- MIPS default memory
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------
-- stats utils --
-----------------

record MemStats
{
    data_reads  :: nat
    data_writes :: nat
    inst_reads  :: nat
}

declare memStats :: MemStats

unit initMemStats =
{
    memStats.data_reads  <- 0;
    memStats.data_writes <- 0;
    memStats.inst_reads  <- 0
}

string printMemStats =
    PadRight (#" ", 16, "data_reads")  : " = " :
    PadLeft (#" ", 9, [memStats.data_reads])  : "\n" :
    PadRight (#" ", 16, "data_writes") : " = " :
    PadLeft (#" ", 9, [memStats.data_writes]) : "\n" :
    PadRight (#" ", 16, "inst_reads")  : " = " :
    PadLeft (#" ", 9, [memStats.inst_reads])
string csvHeaderMemStats = "data_reads,data_writes,inst_reads"
string csvMemStats = [memStats.data_reads] : "," : [memStats.data_writes] : "," : [memStats.inst_reads]

unit clearDynamicMemStats () = nothing

---------------------------
-- memory implementation --
---------------------------

type dwordAddr = bits(37)

declare MEM :: dwordAddr -> dword -- physical memory (37 bits), doubleword access

-- mem log utils --

string MemAddr_str (addr::dwordAddr) = hex (addr:'000')

string log_mem_write (addr::dwordAddr, data::dword) =
    "write MEM[" : MemAddr_str (addr) : "] <- " : hex (data)

string log_mem_read (addr::dwordAddr, data::dword) =
    "read MEM[" : MemAddr_str (addr) : "]: " : hex (data)

-- mem API --

unit InitMEM =
{
    initMemStats;
    MEM <- InitMap (UNKNOWN(next_unknown("mem-data")))
}

dword ReadData (pAddr::dwordAddr) =
{
    memStats.data_reads <- memStats.data_reads + 1;
    data = MEM(pAddr);
    mark_log (4, log_mem_read (pAddr, data));
    data
}

unit WriteData (pAddr::dwordAddr, data::dword, mask::dword) =
{
    memStats.data_writes <- memStats.data_writes + 1;
    d = MEM(pAddr) && ~mask || data && mask;
    MEM(pAddr) <- d;
    mark_log (4, log_mem_write (pAddr, d))
}

word ReadInst (a::pAddr) =
{
    memStats.inst_reads <- memStats.inst_reads + 1;
    if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>
}

-- sml helper function
unit WriteDWORD (pAddr::dwordAddr, data::dword) = MEM(pAddr) <- data

-- sml helper function
unit Write256 (pAddr::bits(35), data::bits(256)) =
{
    MEM(pAddr:'00') <- data<63:0>;
    MEM(pAddr:'01') <- data<127:64>;
    MEM(pAddr:'10') <- data<191:128>;
    MEM(pAddr:'11') <- data<255:192>
}

-- stub for configuring L2 cache
unit setL2 (replace_policy::nat, prefetch_depth::nat, l2_pefetcher::nat) = ()
