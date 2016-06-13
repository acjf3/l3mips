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

declare memStats :: MemStats

unit initMemStats =
{
    memStats.data_reads  <- 0;
    memStats.data_writes <- 0;
    memStats.inst_reads  <- 0;
    memStats.valid_cap_reads    <- 0;
    memStats.invalid_cap_reads  <- 0;
    memStats.valid_cap_writes   <- 0;
    memStats.invalid_cap_writes <- 0;
    memStats.working_set        <- 0;
    memStats.tags_set           <- 0
}

string printMemStats =
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
                memStats.working_set <- memStats.working_set + 1
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
                memStats.working_set <- memStats.working_set + 1
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
                when tagged do memStats.tags_set <- memStats.tags_set + 1
            }
            case tagR(t) =>
            {
                the_shadow_TAGS(addr) <- tagRW(tagged);
                when tagged and t == false do
                    memStats.tags_set <- memStats.tags_set + 1;
                when not tagged and t == true do
                    memStats.tags_set <- memStats.tags_set - 1
            }
            case tagW(t) =>
            {
                the_shadow_TAGS(addr) <- tagW(tagged);
                when tagged and t == false do
                    memStats.tags_set <- memStats.tags_set + 1;
                when not tagged and t == true do
                    memStats.tags_set <- memStats.tags_set - 1
            }
            case tagRW(t) =>
            {
                the_shadow_TAGS(addr) <- tagRW(tagged);
                when tagged and t == false do
                    memStats.tags_set <- memStats.tags_set + 1;
                when not tagged and t == true do
                    memStats.tags_set <- memStats.tags_set - 1
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
    initMemStats;
    the_shadow_MEM   <- InitMap (memUntouched);
    the_shadow_TAGS  <- InitMap (tagUntouched);
    the_MEM <- InitMap (Raw(UNKNOWN))
}

dword ReadData (dwordAddr::bits(37)) =
{
    memStats.data_reads <- memStats.data_reads + 1;
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
    memStats.data_writes <- memStats.data_writes + 1;
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
    memStats.inst_reads <- memStats.inst_reads + 1;
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
        memStats.valid_cap_reads <- memStats.valid_cap_reads + 1
    else
        memStats.invalid_cap_reads <- memStats.invalid_cap_reads + 1;
    mark_log(4, "read " : (if getTag(data) then "valid" else "invalid") :
                " cap from 0x" : hex40(capAddr:0));
    data
}

unit WriteCap (capAddr::CAPADDR, cap::Capability) =
{
    MEM(capAddr) <- Cap (cap);
    if getTag(cap) then
        memStats.valid_cap_writes <- memStats.valid_cap_writes + 1
    else
        memStats.invalid_cap_writes <- memStats.invalid_cap_writes + 1;
    mark_log(4, "write " : (if getTag(cap) then "valid" else "invalid") :
                " cap @ 0x" : hex40(capAddr:0))
}
