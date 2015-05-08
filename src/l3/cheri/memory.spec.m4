--------------------------------------------------------------------------------
-- CHERI default memory
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

include(`helpers.m4')dnl
include(`cap-params.m4')dnl
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

----------------------------
-- types and declarations --
----------------------------

construct DataType {DataCap :: Capability, DataRaw :: CAPRAWBITS}

declare MEM :: CAPADDR -> DataType -- physical memory (35 bits), capability access

---------------------------
-- memory implementation --
---------------------------

unit InitMEM =
{
    initMemStats;
    MEM <- InitMap (DataRaw(UNKNOWN))
}

dword ReadData (dwordAddr::bits(37)) =
{
    memStats.data_reads <- memStats.data_reads + 1;
    var data;
    match MEM(dwordAddr<36:eval(log2(CAPBYTEWIDTH)-3)>)
    {
        case DataCap (cap) =>
        {
            mark_log(5, "!!! normal read in cap !!!"); -- shouldn't be done
            data <- readDwordFromRaw (dwordAddr, capToBits(cap))
        }
        case DataRaw (raw) => data <- readDwordFromRaw (dwordAddr, raw)
    };
    mark_log(3, "read data 0x":strToLower([data]):" from dwordAddr 0x":strToLower([dwordAddr]));
    data
}

unit WriteData (dwordAddr::bits(37), data::dword, mask::dword) =
{
    memStats.data_writes <- memStats.data_writes + 1;
    var old_blob;
    match MEM(dwordAddr<36:eval(log2(CAPBYTEWIDTH)-3)>)
    {
        case DataCap (cap) =>
        {
            mark_log(5, "!!! normal write in cap !!!");
            old_blob <- capToBits(cap)
        }
        case DataRaw (raw) => old_blob <- raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, mask, old_blob);
    mark_log(3, "write data 0x":strToLower([data]):" @ dwordAddr 0x":strToLower([dwordAddr]));
    MEM(dwordAddr<36:eval(log2(CAPBYTEWIDTH)-3)>) <- DataRaw(new_blob)
}

word ReadInst (a::pAddr) =
{
    memStats.inst_reads <- memStats.inst_reads + 1;
    var inst_pair;
    match MEM(a<39:log2(CAPBYTEWIDTH)>)
    {
        case DataCap (cap) =>
        {
            mark_log(5, "!!! instruction read in cap !!!"); -- shouldn't be done
            inst_pair <- readDwordFromRaw (a<39:3>, capToBits(cap))
        }
        case DataRaw (raw) => inst_pair <- readDwordFromRaw (a<39:3>, raw)
    };
    inst = if a<2> then inst_pair<31:0> else inst_pair<63:32>;
    mark_log(3, "read instruction 0x":strToLower([inst]):" @0x":strToLower([a<39:2>]));
    inst
}

Capability ReadCap (capAddr::CAPADDR) =
{
    memStats.cap_reads <- memStats.cap_reads + 1;
    data = match MEM(capAddr)
    {
        case DataCap (cap) => cap
        case DataRaw (raw) => bitsToCap(raw)
    };
    mark_log(4, "read ":(if getTag(data) then "valid" else "invalid"):" cap from capAddr 0x":strToLower([capAddr]));
    data
}

unit WriteCap (capAddr::CAPADDR, cap::Capability) =
{
    memStats.cap_writes <- memStats.cap_writes + 1;
    MEM(capAddr) <- DataCap (cap);
    mark_log(4, "write ":(if getTag(cap) then "valid" else "invalid"):" cap @ capAddr 0x":strToLower([capAddr]))
}
