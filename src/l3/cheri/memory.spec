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
    valid_cap_reads     :: nat
    invalid_cap_reads   :: nat
    valid_cap_writes    :: nat
    invalid_cap_writes  :: nat
    working_set         :: nat
    tags_set            :: nat
    tags_working_set    :: nat
}

declare staticMemStats  :: MemStats
declare dynamicMemStats :: MemStats

MemStats nullMemStats =
{
    var stats;
    stats.data_reads  <- 0;
    stats.data_writes <- 0;
    stats.inst_reads  <- 0;
    stats.valid_cap_reads    <- 0;
    stats.invalid_cap_reads  <- 0;
    stats.valid_cap_writes   <- 0;
    stats.invalid_cap_writes <- 0;
    stats.working_set        <- 0;
    stats.tags_set           <- 0;
    stats
}

unit initMemStats = 
{
    staticMemStats   <- nullMemStats;
    dynamicMemStats  <- nullMemStats
}

unit clearDynamicMemStats () = dynamicMemStats <- nullMemStats

unit stats_data_reads_updt (inc::int) =
{
    staticMemStats.data_reads  <- [[staticMemStats.data_reads] + inc];
    dynamicMemStats.data_reads <- [[dynamicMemStats.data_reads] + inc]
}
unit stats_data_writes_updt (inc::int) =
{
    staticMemStats.data_writes  <- [[staticMemStats.data_writes] + inc];
    dynamicMemStats.data_writes <- [[dynamicMemStats.data_writes] + inc]
}
unit stats_inst_reads_updt (inc::int) =
{
    staticMemStats.inst_reads  <- [[staticMemStats.inst_reads] + inc];
    dynamicMemStats.inst_reads <- [[dynamicMemStats.inst_reads] + inc]
}
unit stats_valid_cap_reads_updt (inc::int) =
{
    staticMemStats.valid_cap_reads  <- [[staticMemStats.valid_cap_reads] + inc];
    dynamicMemStats.valid_cap_reads <- [[dynamicMemStats.valid_cap_reads] + inc]
}
unit stats_valid_cap_writes_updt (inc::int) =
{
    staticMemStats.valid_cap_writes  <- [[staticMemStats.valid_cap_writes] + inc];
    dynamicMemStats.valid_cap_writes <- [[dynamicMemStats.valid_cap_writes] + inc]
}
unit stats_invalid_cap_reads_updt (inc::int) =
{
    staticMemStats.invalid_cap_reads  <- [[staticMemStats.invalid_cap_reads] + inc];
    dynamicMemStats.invalid_cap_reads <- [[dynamicMemStats.invalid_cap_reads] + inc]
}
unit stats_invalid_cap_writes_updt (inc::int) =
{
    staticMemStats.invalid_cap_writes  <- [[staticMemStats.invalid_cap_writes] + inc];
    dynamicMemStats.invalid_cap_writes <- [[dynamicMemStats.invalid_cap_writes] + inc]
}
unit stats_working_set_updt (inc::int) =
{
    staticMemStats.working_set  <- [[staticMemStats.working_set] + inc];
    dynamicMemStats.working_set <- [[dynamicMemStats.working_set] + inc]
}
unit stats_tags_set_updt (inc::int) =
{
    staticMemStats.tags_set  <- [[staticMemStats.tags_set] + inc];
    dynamicMemStats.tags_set <- [[dynamicMemStats.tags_set] + inc]
}

string strMemStats (memStats::MemStats) =
    PadRight (#" ", 19, "data_reads")  : " = " :
    PadLeft (#" ", 9, [memStats.data_reads])  : "\\n" :
    PadRight (#" ", 19, "data_writes") : " = " :
    PadLeft (#" ", 9, [memStats.data_writes]) : "\\n" :
    PadRight (#" ", 19, "inst_reads")  : " = " :
    PadLeft (#" ", 9, [memStats.inst_reads])  : "\\n" :
    PadRight (#" ", 19, "valid_cap_reads")   : " = " :
    PadLeft (#" ", 9, [memStats.valid_cap_reads])   : "\\n" :
    PadRight (#" ", 19, "invalid_cap_reads")   : " = " :
    PadLeft (#" ", 9, [memStats.invalid_cap_reads])   : "\\n" :
    PadRight (#" ", 19, "valid_cap_writes")   : " = " :
    PadLeft (#" ", 9, [memStats.valid_cap_writes])   : "\\n" :
    PadRight (#" ", 19, "invalid_cap_writes")  : " = " :
    PadLeft (#" ", 9, [memStats.invalid_cap_writes]) : "\\n" :
    PadRight (#" ", 19, "working_set")  : " = " :
    PadLeft (#" ", 9, [memStats.working_set]) : " (" : [memStats.working_set * CAPBYTEWIDTH] : " bytes)\\n" :
    PadRight (#" ", 19, "tags_set")  : " = " :
    PadLeft (#" ", 9, [memStats.tags_set]) : " (one tag per " : [CAPBYTEWIDTH] : " bytes)\\n"

string printMemStats =
    "static memory count:\\n" : strMemStats(staticMemStats) :
    "dynamic mem stats (for last sampled quantum)\\n" : strMemStats(dynamicMemStats)

----------------------------
-- types and declarations --
----------------------------

construct DataType {Cap :: Capability, Raw :: CAPRAWBITS}

declare the_MEM :: CAPADDR -> DataType -- physical memory (35 bits), capability access
construct ShadowMem {memUntouched memR memW memRW}
declare the_shadow_MEM   :: CAPADDR -> ShadowMem
construct ShadowTags {tagUntouched tagR::bool tagW::bool tagRW::bool}
declare the_shadow_TAGS  :: CAPADDR -> ShadowTags

component MEM (addr::CAPADDR) :: DataType
{
    -- READING --
    value = {
        data = the_MEM(addr);
        tagged = match data {case Cap(c) => getTag(c) case Raw(r) => false};
        match the_shadow_MEM(addr)
        {
            case memUntouched =>
            {
                the_shadow_MEM(addr) <- memR;
                stats_working_set_updt(1)
            }
            case memR =>
            {
                ()
            }
            case memW =>
            {
                the_shadow_MEM(addr) <- memRW
            }
            case memRW =>
            {
                ()
            }
        };
        match the_shadow_TAGS(addr)
        {
            case tagUntouched =>
            {
                the_shadow_TAGS(addr) <- tagR(tagged)
            }
            case tagR(t) =>
            {
                ()
            }
            case tagW(t) =>
            {
                the_shadow_TAGS(addr) <- tagRW(t)
            }
            case tagRW(t) =>
            {
                ()
            }
        };
        data
    }

    -- WRITING --
    assign value =
    {
        tagged = match value {case Cap(c) => getTag(c) case Raw(r) => false};
        match the_shadow_MEM(addr)
        {
            case memUntouched =>
            {
                the_shadow_MEM(addr) <- memW;
                stats_working_set_updt(1)
            }
            case memR =>
            {
                the_shadow_MEM(addr) <- memRW
            }
            case memW =>
            {
                ()
            }
            case memRW =>
            {
                ()
            }
        };
        match the_shadow_TAGS(addr)
        {
            case tagUntouched =>
            {
                the_shadow_TAGS(addr) <- tagW(tagged);
                when tagged do stats_tags_set_updt(1)
            }
            case tagR(t) =>
            {
                the_shadow_TAGS(addr) <- tagRW(tagged);
                when tagged and t == false do
                    stats_tags_set_updt(1);
                when not tagged and t == true do
                    stats_tags_set_updt(-1)
            }
            case tagW(t) =>
            {
                the_shadow_TAGS(addr) <- tagW(tagged);
                when tagged and t == false do
                    stats_tags_set_updt(1);
                when not tagged and t == true do
                    stats_tags_set_updt(-1)
            }
            case tagRW(t) =>
            {
                the_shadow_TAGS(addr) <- tagRW(tagged);
                when tagged and t == false do
                    stats_tags_set_updt(1);
                when not tagged and t == true do
                    stats_tags_set_updt(-1)
            }
        };
        the_MEM(addr) <- value
    }
}

---------------------------
-- memory implementation --
---------------------------

unit InitMEM =
{
    the_shadow_MEM   <- InitMap (memUntouched);
    the_shadow_TAGS  <- InitMap (tagUntouched);
    the_MEM <- InitMap (Raw(UNKNOWN))
}

dword ReadData (dwordAddr::bits(37)) =
{
    stats_data_reads_updt(1);
    var data;
    match MEM(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>)
    {
        case Cap (cap) =>
        {
            when getTag(cap) do
               -- shouldn't be done
               mark_log(5, "!!! normal read in valid cap !!!");
            data <- readDwordFromRaw (dwordAddr, capToBits(cap))
        }
        case Raw (raw) => data <- readDwordFromRaw (dwordAddr, raw)
    };
    mark_log(3, "read data 0x":hex64(data):" from 0x":hex40(dwordAddr:0));
    data
}

unit WriteData (dwordAddr::bits(37), data::dword, mask::dword) =
{
    stats_data_writes_updt(1);
    var old_blob;
    match the_MEM(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>)
    {
        case Cap (cap) =>
        {
            when getTag(cap) do
               mark_log(5, "!!! normal write in valid cap !!!");
            old_blob <- capToBits(cap)
        }
        case Raw (raw) => old_blob <- raw
    };
    new_blob = updateDwordInRaw (dwordAddr, data, mask, old_blob);
    mark_log(3, "write data 0x":hex64(data):" @ 0x":hex40(dwordAddr:0));
    MEM(dwordAddr<36:(Log2(CAPBYTEWIDTH)-3)>) <- Raw(new_blob)
}

word ReadInst (a::pAddr) =
{
    stats_inst_reads_updt(1);
    var inst_pair;
    match MEM(a<39:Log2(CAPBYTEWIDTH)>)
    {
        case Cap (cap) =>
        {
            when getTag(cap) do
               -- shouldn't be done
               mark_log(5, "!!! instruction read in valid cap !!!");
            inst_pair <- readDwordFromRaw (a<39:3>, capToBits(cap))
        }
        case Raw (raw) => inst_pair <- readDwordFromRaw (a<39:3>, raw)
    };
    inst = if a<2> then inst_pair<31:0> else inst_pair<63:32>;
    mark_log(3, "read instruction 0x":hex32(inst):" @0x":hex40(a));
    inst
}

Capability ReadCap (capAddr::CAPADDR) =
{
    data = match MEM(capAddr)
    {
        case Cap (cap) => cap
        case Raw (raw) => bitsToCap(raw)
    };
    if getTag(data) then
        stats_valid_cap_reads_updt(1)
    else
        stats_invalid_cap_reads_updt(1);
    mark_log(4, "read " : (if getTag(data) then "valid" else "invalid") :
                " cap from 0x" : hex40(capAddr:0));
    data
}

unit WriteCap (capAddr::CAPADDR, cap::Capability) =
{
    MEM(capAddr) <- Cap (cap);
    if getTag(cap) then
        stats_valid_cap_writes_updt(1)
    else
        stats_invalid_cap_writes_updt(1);
    mark_log(4, "write " : (if getTag(cap) then "valid" else "invalid") :
                " cap @ 0x" : hex40(capAddr:0))
}
