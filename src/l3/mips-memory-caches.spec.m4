---------------------------------------------------------------------------
-- BERI caches
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

dnl -- math utils
define(`log2', `ifelse($1, 1, 0, `eval(1 + log2(eval($1 / 2)))')')dnl -- compute log2
dnl -- generic utils
define(`Replace', `Take($1,$3):Cons($2,Drop($1+1, $3))')dnl -- Replace(i,val,list) replaces ith element of a list

-----------------
-- basic types --
-----------------

dnl -- L1 compile time values (direct mapped L1)
define(`L1SIZE', ifdef(`L1SIZE', L1SIZE, 16384))dnl -- L1 cache size in bytes (default 16KB)
define(`L1LINESIZE', ifdef(`L1LINESIZE', L1LINESIZE, 32))dnl -- L1 line size in bytes (default 32B)
define(`L1ADDRWIDTH', 40)dnl -- address size in bits (40 bits)
define(`L1OFFSETWIDTH', log2(L1LINESIZE))dnl -- size of offset feild in bits
define(`L1INDEXWIDTH', eval(log2(eval(L1SIZE/L1LINESIZE))))dnl -- size of index feild in bits
define(`L1TAGWIDTH', eval(L1ADDRWIDTH-L1INDEXWIDTH-L1OFFSETWIDTH))dnl -- size of tag feild in bits
define(`L1LINENUMBERWIDTH', eval(L1ADDRWIDTH-L1OFFSETWIDTH))dnl -- size of linenumber feild in bits

type L1Offset = bits(L1OFFSETWIDTH)
type L1Index = bits(L1INDEXWIDTH)
type L1Tag = bits(L1TAGWIDTH)
type L1LineNumber = bits(L1LINENUMBERWIDTH)
type L1Data = bits(eval(L1LINESIZE*8))
record L1Entry {valid::bool tag::L1Tag data::L1Data}
type DirectMappedL1 = L1Index -> L1Entry
type L1Cache = DirectMappedL1

type L1Id = nat

construct L1Type {Data, Inst}

string l1type_str (cacheType::L1Type) = match cacheType
{
    case Data => "DCache"
    case Inst => "ICache"
}

dnl -- L2 compile time values
define(`L2SIZE', ifdef(`L2SIZE', L2SIZE, 65536))dnl -- L2 cache size in bytes (default 64KB)
define(`L2WAYS', ifdef(`L2WAYS', L2WAYS, 1))dnl -- L2 associativity (default direct mapped)
define(`L2LINESIZE', ifdef(`L2LINESIZE', L2LINESIZE, 32))dnl -- L2 line size in bytes (default 32B)
define(`L2ADDRWIDTH', 40)dnl -- address size in bits (40 bits)
define(`L2OFFSETWIDTH', log2(L2LINESIZE))dnl -- size of offset feild in bits
define(`L2INDEXWIDTH', eval(log2(eval(L2SIZE/(L2WAYS*L2LINESIZE)))))dnl -- size of index feild in bits
define(`L2TAGWIDTH', eval(L2ADDRWIDTH-L2INDEXWIDTH-L2OFFSETWIDTH))dnl -- size of tag feild in bits
define(`L2LINENUMBERWIDTH', eval(L2ADDRWIDTH-L2OFFSETWIDTH))dnl -- size of linenumber feild in bits

type L2Offset = bits(L2OFFSETWIDTH)
type L2Index = bits(L2INDEXWIDTH)
type L2Tag = bits(L2TAGWIDTH)
type L2LineNumber = bits(L2LINENUMBERWIDTH)
type L2Data = bits(eval(L2LINESIZE*8))
record L2Entry {valid::bool tag::L2Tag sharers::L1Id list data::L2Data}
type DirectMappedL2 = L2Index -> L2Entry
type L2Cache = nat -> DirectMappedL2

-- mem types --

type MemAddr = L2LineNumber
type MemData = L2Data
type dwordAddr = bits(37)

-- instanciate l1 caches --

declare
{
    current_l1_type :: L1Type
    c_l1_cache      :: (L1Type * id) -> L1Cache
}

component L1Cache (idx::L1Index) :: L1Entry
{
    value =
    {
        cache = c_l1_cache(current_l1_type, procID);
        cache (idx)
    }
    assign value =
    {
        var new_cache = c_l1_cache(current_l1_type, procID);
        new_cache (idx) <- value;
        c_l1_cache(current_l1_type, procID) <- new_cache
    }
}

L1Id L1ID = 
match current_l1_type
{
    case Inst => (2 * [procID])
    case Data => (2 * [procID]) + 1
}

-- instanciate l2 cache --

declare l2ReplacePolicy :: nat
-- naive replace policy
declare l2LastVictimWay::nat
-- LRU replace policy
declare l2LRUBits::L2Index -> nat list

declare c_l2 :: nat -> DirectMappedL2
component L2Cache (way::nat, idx::L2Index) :: L2Entry
{
    value = { m = c_l2(way); m(idx) }
    assign value = { var m = c_l2(way)
                    ; m(idx) <- value
                    ; c_l2(way) <- m }
}

-- instanciate memory --

declare MEM :: MemAddr -> MemData -- physical memory

-- general l1 utils --

L1Index l1_hash_default(addr::L1LineNumber) = addr<eval(L1LINENUMBERWIDTH-1-L1TAGWIDTH):eval(L1LINENUMBERWIDTH-L1TAGWIDTH-L1INDEXWIDTH)>
L1Tag l1_tag_default(addr::L1LineNumber) = addr<L1LINENUMBERWIDTH-1:eval(L1LINENUMBERWIDTH-L1TAGWIDTH)>

L1Index L1Idx(addr::L1LineNumber) = l1_hash_default(addr)
L1Tag L1Tag(addr::L1LineNumber) = l1_tag_default(addr)

L1LineNumber L1LineNumberFromDwordAddr (addr::dwordAddr) = addr<36:eval(37-L1LINENUMBERWIDTH)>
nat L1LineDwordIdx (addr::dwordAddr) = [addr<eval(log2(L1LINESIZE/8)-1):0>]

dword list L1DataToDwordList (data::L1Data) =
{
    var dword_list = Nil;
    for i in eval(L1LINESIZE/8) .. 1 do
        dword_list <- Cons (data<(i*64)-1:(i-1)*64>, dword_list);
    dword_list
}

L1Data DwordListToL1Data (dword_list::dword list) =
{
    var data;
    var tmp_list = dword_list;
    for i in 1 .. eval(L1LINESIZE/8) do
    {
        data<(i*64)-1:(i-1)*64> <- Head(tmp_list);
        tmp_list <- Drop(1, tmp_list)
    };
    data
}

L1Data L1MergeData (old::L1Data, new::L1Data, mask::L1Data) = old && ~mask || new && mask

L1Entry mkL1CacheEntry(valid::bool, tag::L1Tag, data::L1Data) =
{
    var line::L1Entry;
    line.valid <- valid;
    line.tag   <- tag;
    line.data  <- data;
    line
}

L1Cache DirectMappedL1Init () = InitMap (mkL1CacheEntry(false, UNKNOWN, UNKNOWN))

-- l1 log utils --

string addr_l1line_str (addr::L1LineNumber) =
    "0x" : PadLeft (`#'"0", eval((L1LINENUMBERWIDTH+3)/4), [addr])
string addr_l1tag_str (addr::L1LineNumber) =
    "0x" : PadLeft (`#'"0", eval((L1TAGWIDTH+3)/4), [L1Tag(addr)])
string addr_l1idx_str (addr::L1LineNumber) =
    PadLeft (`#'"0", eval((L1INDEXWIDTH+3)/4), [[L1Idx(addr)]::nat])
string l1tag_str (tag::L1Tag) =
    "0x" : PadLeft (`#'"0", eval((L1TAGWIDTH+3)/4), [tag])

string l1data_str (data::L1Data) =
{
    var str = "{";
    var i::nat = 0;
    dword_list = L1DataToDwordList (data);
    foreach elem in dword_list do
    {
        when i > 0 do
            str <- str : ",";
        str <- str : [i::nat] :":0x" : hex64(elem);
        i <- i + 1
    };
    return str : "}"
}

-- general l2 utils --

L2Index l2_hash_default(addr::L2LineNumber) = addr<eval(L2LINENUMBERWIDTH-1-L2TAGWIDTH):eval(L2LINENUMBERWIDTH-L2TAGWIDTH-L2INDEXWIDTH)>
L2Tag l2_tag_default(addr::L2LineNumber) = addr<L2LINENUMBERWIDTH-1:eval(L2LINENUMBERWIDTH-L2TAGWIDTH)>

L2Index L2Idx(addr::L2LineNumber) = l2_hash_default(addr)
L2Tag L2Tag(addr::L2LineNumber) = l2_tag_default(addr)

L2LineNumber L2LineNumberFromL1LineNumber (addr::L1LineNumber) = addr<L1LINENUMBERWIDTH-1:eval(L1LINENUMBERWIDTH-L2LINENUMBERWIDTH)>

{-
L1Data list L2DataToL1DataList (data::L2Data) =
{
    var l1data_list = Nil;
    for i in eval(L2LINESIZE/L1LINESIZE) .. 1 do
        l1data_list <- Cons (data<(i*eval(8*L1LINESIZE))-1:(i-1)*eval(8*L1LINESIZE)>, l1data_list);
    l1data_list
}

L2Data L1DataListToL2Data (l1data_list::L1Data list) =
{
    var data;
    var tmp_list = l1data_list;
    for i in 1 .. eval(L2LINESIZE/L1LINESIZE) do
    {
        data<(i*eval(8*L1LINESIZE))-1:(i-1)*(8*L1LINESIZE)> <- Head(tmp_list);
        tmp_list <- Drop(1, tmp_list)
    };
    data
}
-}

L2Data L2MergeData (old::L2Data, new::L2Data, mask::L2Data) = old && ~mask || new && mask

L2Entry mkL2CacheEntry(valid::bool, tag::L2Tag, sharers::L1Id list, data::L2Data) =
{
    var line::L2Entry;
    line.valid   <- valid;
    line.tag     <- tag;
    line.sharers <- sharers;
    line.data    <- data;
    line
}

DirectMappedL2 DirectMappedL2Init () = InitMap (mkL2CacheEntry(false, UNKNOWN, UNKNOWN, UNKNOWN))

-- l2 log utils --

string addr_l2line_str (addr::L2LineNumber) =
    "0x" : PadLeft (`#'"0", eval((L2LINENUMBERWIDTH+3)/4), [addr])
string addr_l2tag_str (addr::L2LineNumber) =
    "0x" : PadLeft (`#'"0", eval((L2TAGWIDTH+3)/4), [L2Tag(addr)])
string addr_l2idx_str (addr::L2LineNumber) =
    PadLeft (`#'"0", eval((L2INDEXWIDTH+3)/4), [[L2Idx(addr)]::nat])
string l2tag_str (tag::L2Tag) =
    "0x" : PadLeft (`#'"0", eval((L2TAGWIDTH+3)/4), [tag])

string sharers_str (sharers::L1Id list) =
{
    var str = "{";
    var i::nat = 0;
    foreach sharer in sharers do
    {
        when i > 0 do
            str <- str : ",";
        str <- str : [sharer::nat];
        i <- i + 1
    };
    return str : "}"
}

string l2data_str (data::L2Data) = -- FIXME
    "0x" : ToLower([data])

string l2entry_str (entry::L2Entry) =
    "[" : [entry.valid] :
    "|" : l2tag_str(entry.tag) :
    "|sharers" : sharers_str(entry.sharers) :
    "|" : l2data_str(entry.data) : "]"

string l2addr_str (addr::L2LineNumber) =
    addr_l2line_str(addr) :
    "(tag:" : addr_l2tag_str(addr) :
    ",idx:" : addr_l2idx_str(addr) : ")"

-- mem log utils --

string MemAddr_str (addr::MemAddr) =
    "0x" : hex40(ZeroExtend(addr)<<log2(L2LINESIZE))

string MemData_str (data::MemData) = "0x" : ToLower([data])

string log_mem_write (addr::MemAddr, data::MemData) =
    "write MEM[" : MemAddr_str (addr) :
    "] <- " : MemData_str (data)

string log_mem_read (addr::MemAddr, data::MemData) =
    "read MEM[" : MemAddr_str (addr) :
    "]: " : MemData_str (data)

--------------
-- l2 cache --
--------------

-- l2 logs --

string l2prefix_str (addr::L2LineNumber) =
    "L2(core" : [[procID]::nat] : "," : l1type_str(current_l1_type) : ")" :
    "@" : l2addr_str(addr)

string log_l2_read (addr::L2LineNumber) =
    l2prefix_str(addr) : " - read"

string log_l2_read_hit (addr::L2LineNumber, way::nat, entry::L2Entry) =
    l2prefix_str(addr) : " - read hit - way " : [way::nat] : " - " :
    l2entry_str(entry)

string log_l2_read_miss (addr::L2LineNumber) =
    l2prefix_str(addr) : " - read miss - "

string log_l2_fill (addr::L2LineNumber, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(addr) : " - fill - way " : [way::nat] : " - " :
    "old@" : l2addr_str(old.tag:L2Idx(addr)) : l2entry_str(old) : " - " :
    "new@" : l2addr_str(new.tag:L2Idx(addr)) : l2entry_str(new)

string log_l2_evict (addr::L2LineNumber, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(addr) : " - evict - way " : [way::nat] : " - " :
    "old@" : l2addr_str(old.tag:L2Idx(addr)) : l2entry_str(old) : " - " :
    "new@" : l2addr_str(new.tag:L2Idx(addr)) : l2entry_str(new)

string log_l2_write_hit (addr::L2LineNumber, way::nat, data::L2Data) =
    l2prefix_str(addr) : " - write hit - way " : [way::nat] : " - " :
    l2data_str(data)

string log_l2_write_miss (addr::L2LineNumber) =
    l2prefix_str(addr) : " - write miss"

string log_l2_updt_sharers (addr::L2LineNumber, old::L1Id list, new::L1Id list) =
    l2prefix_str(addr) : " - update sharers - " :
    sharers_str (old) : " <- " : sharers_str (new)

string log_l2_inval_l1 (l1id::nat, addr::L2LineNumber) =
    "L2 inval L1 " : [l1id::nat] : " @" : l2addr_str(addr) :
    " L2idx:" : addr_l2idx_str(addr) : ",L1idx:" : addr_l1idx_str(addr)

-- l2 API --

L1Id list L2UpdateSharers (l1id::L1Id, val::bool, sharers::L1Id list) = match val
{
    case true  => if not IsMember (l1id, sharers) then Cons (l1id, sharers) else sharers
    case false => Remove (sharers, list {l1id})
}

unit L2InvalL1 (addr::L2LineNumber, sharers::L1Id list, invalCurrent::bool) =
when totalCore > 1 do
{
    -- backup current procID / current_l1_type --
    currentProc = procID;
    currentL1   = current_l1_type;
    foreach sharer in sharers do
    when (invalCurrent or ([sharer::nat div 2] <> currentProc)) do
    {
        procID          <- [sharer::nat div 2];
        current_l1_type <- if (sharer mod 2) == 0 then Inst else Data;
        entry = L1Cache(L1Idx(addr));
        -- XXX test on short tags...
        when entry.valid and entry.tag<14:0> == L1Tag(addr)<14:0> do
            L1Cache(L1Idx(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN)
    };
    -- restore current procId / current_l1_type --
    procID          <- currentProc;
    current_l1_type <- currentL1
}

(L2Entry * nat) option L2Hit (addr::L2LineNumber) =
{
    var ret = None;
    for i in L2WAYS .. 1 do
    {
        cacheEntry = L2Cache(i, L2Idx(addr));
        when (cacheEntry.valid and cacheEntry.tag == L2Tag(addr)) do
            ret <- Some (cacheEntry, i)
    };
    ret
}

nat naiveReplace(addr::L2LineNumber) =
{
    ret = l2LastVictimWay;
    l2LastVictimWay <- (l2LastVictimWay + 1) mod L2WAYS;
    (ret + 1)
}

nat LRUReplace (addr::L2LineNumber) =
{
    r = Reverse(l2LRUBits(L2Idx(addr)));
    if r == Nil then 1
    else
    {
        l2LRUBits(L2Idx(addr)) <- Reverse(Tail(r));
        Head(r)
    }
}

nat innerL2ReplacePolicy (addr::L2LineNumber) = match l2ReplacePolicy
{
    case 0 => naiveReplace (addr)
    case 1 => LRUReplace (addr)
}

nat L2ReplacePolicy (addr::L2LineNumber) =
{
    var found_empty = false;
    var ret;
    -- chose empty way --
    for i in L2WAYS .. 1 do
        when ! L2Cache(i,L2Idx(addr)).valid do
            {found_empty <- true; ret <- i};
    -- potential call to inner replace policy --
    if ! found_empty then innerL2ReplacePolicy (addr)
    else ret
}

L2Data L2ServeMiss (addr::L2LineNumber) =
{
    -- read data from memory --
    data = MEM(addr);

    -- initialize a new entry --
    var new_entry;
    new_entry <- mkL2CacheEntry(true, L2Tag(addr), L2UpdateSharers (L1ID, true, Nil), data);

    -- take care of replacement --
    victimWay = L2ReplacePolicy (addr);
    old_entry = L2Cache (victimWay,L2Idx(addr));

    when old_entry.valid do
    {
        mem_addr = old_entry.tag : L2Idx(addr);
        -- write cache line back to memory --
        -- when old_entry.dirty do
        MEM(mem_addr) <- old_entry.data;
        -- take care of coherence --
        L2InvalL1(mem_addr, old_entry.sharers, true)
    };

    -- update cache --
    L2Cache(victimWay,L2Idx(addr)) <- new_entry;

    -- return data --
    data
}

L2Entry L2Update (addr::L2LineNumber, data::L2Data, mask::L2Data) =
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            var new_data = L2MergeData (cacheEntry.data, data, mask);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_data);
            mark_log (4, log_l2_write_hit (addr, way, new_data));
            L2Cache(way,L2Idx(addr))
        }
        case None =>
        {
            mark_log (4, log_l2_write_miss (addr));
            cacheLine = L2ServeMiss (addr);
            var retEntry;
            match L2Hit (addr)
            {
                case Some (cacheEntry, way) =>
                {
                    var new_data = L2MergeData (cacheEntry.data, data, mask);
                    retEntry <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_data);
                    L2Cache(way,L2Idx(addr)) <- retEntry
                }
                case _ => #UNPREDICTABLE ("L2 cache reached an inconsistent state (write miss -> serve miss -> no hit)")
            };
            retEntry
        }
    }

unit L2HandleCoherence (addr::L2LineNumber, data::L2Data, mask::L2Data, entry::L2Entry) =
    L2InvalL1(addr, entry.sharers, false)

L2Data L2Read (addr::L2LineNumber) =
{
    var cacheLine;
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            new_sharers = L2UpdateSharers(L1ID, true, cacheEntry.sharers);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, new_sharers, cacheEntry.data);
            mark_log (4, log_l2_read_hit(addr, way, L2Cache(way,L2Idx(addr))));
            mark_log (4, log_l2_updt_sharers(addr, cacheEntry.sharers, new_sharers));
            l2LRUBits(L2Idx(addr)) <- way @ l2LRUBits(L2Idx(addr));
            cacheLine <- cacheEntry.data
        }
        case None =>
        {
            mark_log (4, log_l2_read_miss(addr));
            cacheLine <- L2ServeMiss (addr)
        }
    };
    cacheLine
}

unit L2Write (addr::L2LineNumber, data::L2Data, mask::L2Data) =
{
    cacheEntry = L2Update(addr, data, mask);
    L2HandleCoherence(addr, data, mask, cacheEntry)
}

---------------
-- l1 caches --
---------------

-- l1 logs --

string l1entry_str (entry::L1Entry) =
    "[" : [entry.valid] :
    "|" : l1tag_str(entry.tag) : "]" :
    "|" : l1data_str(entry.data)

string l1prefix_str (addr::L1LineNumber) =
    "L1[core "  : [[procID]::nat] : ", " : l1type_str(current_l1_type) : "]" :
    "@line " : addr_l1line_str(addr) :
    "(tag:"  : addr_l1tag_str(addr) : ",idx:" : addr_l1idx_str(addr) : ")"

string log_l1_read (addr::L1LineNumber) =
    l1prefix_str (addr) : " - read"

string log_l1_read_hit (addr::L1LineNumber, data::L1Data) =
    l1prefix_str (addr) : " - read hit - " : l1data_str(data)

string log_l1_read_miss (addr::L1LineNumber) =
    l1prefix_str (addr) : " - read miss"

string log_l1_evict (addr::L1LineNumber, old::L1Entry, new::L1Entry) =
    l1prefix_str (addr) : " - evict - " :
    "old@line " : addr_l1line_str([old.tag:L1Idx(addr)]) : l1entry_str(old) : " - " :
    "new@line " : addr_l1line_str([new.tag:L1Idx(addr)]) : l1entry_str(new)

string log_l1_write (addr::L1LineNumber, data::L1Data) =
    l1prefix_str (addr) : " - write - " : l1data_str(data)

string log_l1_write_hit (addr::L1LineNumber, data::L1Data) =
    l1prefix_str (addr) : " - write hit - " : l1data_str(data)

string log_l1_write_miss (addr::L1LineNumber) =
    l1prefix_str (addr) : " - write miss"

-- l1 API --

L1Entry option L1Hit (addr::L1LineNumber) =
{
    cacheEntry = L1Cache(L1Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L1Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

L1Data L1ServeMiss (addr::L1LineNumber) =
{
    data = L2Read (addr); -- XXX l1 addr to l2 addr conversion needed
    new_entry = mkL1CacheEntry(true, L1Tag(addr), data);
    old_entry = L1Cache(L1Idx(addr));
    when old_entry.valid do
        mark_log (3, log_l1_evict(addr, old_entry, new_entry));
    L1Cache(L1Idx(addr)) <- new_entry;
    data
}

unit L1Update (addr::L1LineNumber, data::L1Data, mask::L1Data) =
match L1Hit (addr)
{
    case Some (cacheEntry) =>
    {
        var new_data = L1MergeData (cacheEntry.data, data, mask);
        L1Cache(L1Idx(addr)) <- mkL1CacheEntry(true, cacheEntry.tag, new_data);
        mark_log (3, log_l1_write_hit(addr, new_data))
    }
    case None => mark_log (3, log_l1_write_miss(addr))
}

unit L1ServeWrite (addr::L1LineNumber, data::L1Data, mask::L1Data) =
    -- XXX l1 addr/data/mask to l2 addr/data/mask conversion needed
    L2Write(addr, data, mask)

L1Data L1Read (addr::L1LineNumber) =
{
    var cacheLine;
    match L1Hit (addr)
    {
        case Some (cacheEntry) =>
        {
            mark_log (3, log_l1_read_hit (addr, cacheLine));
            cacheLine <- cacheEntry.data
        }
        case None =>
        {
            mark_log (3, log_l1_read_miss (addr));
            cacheLine <- L1ServeMiss (addr)
        }
    };
    cacheLine
}

unit L1Write (addr::L1LineNumber, data::L1Data, mask::L1Data) =
{
    L1Update (addr, data, mask);
    L1ServeWrite (addr, data, mask)
}

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
    PadRight (#" ", 16, "data_reads")  : " = " : PadLeft (#" ", 9, [memStats.data_reads::nat])  : "\\n" :
    PadRight (#" ", 16, "data_writes") : " = " : PadLeft (#" ", 9, [memStats.data_writes::nat]) : "\\n" :
    PadRight (#" ", 16, "inst_reads")  : " = " : PadLeft (#" ", 9, [memStats.inst_reads::nat])

--------------------
-- mips interface --
--------------------

unit InitMEM =
{
    initMemStats;
    MEM <- InitMap (UNKNOWN);
    l2LastVictimWay <- L2WAYS;
    for i in L2WAYS .. 1 do
        c_l2(i) <- DirectMappedL2Init ();
    for i in 0 .. totalCore-1 do
    {
        c_l1_cache (Data, [i]) <- DirectMappedL1Init ();
        c_l1_cache (Inst, [i]) <- DirectMappedL1Init ()
    }
}

dword ReadData (pAddr::dwordAddr) =
{
    memStats.data_reads <- memStats.data_reads + 1;
    current_l1_type <- Data;
    data = L1Read(L1LineNumberFromDwordAddr(pAddr));
    Element(L1LineDwordIdx(pAddr), L1DataToDwordList(data))
}

unit WriteData (pAddr::dwordAddr, data::dword, mask::dword) =
{
    memStats.data_writes <- memStats.data_writes + 1;

    var l1_data = L1DataToDwordList(0);
    var l1_mask = L1DataToDwordList(0);
    l1_dword_idx = L1LineDwordIdx (pAddr);
    l1_data <- Replace(l1_dword_idx, data, l1_data);
    l1_mask <- Replace(l1_dword_idx, mask, l1_mask);
    current_l1_type <- Data;
    L1Write(L1LineNumberFromDwordAddr(pAddr), DwordListToL1Data(l1_data), DwordListToL1Data(l1_mask))
}

word ReadInst (a::pAddr) =
{
    memStats.inst_reads <- memStats.inst_reads + 1;
    current_l1_type <- Inst;
    data = L1Read(L1LineNumberFromDwordAddr(a<39:3>));
    inst_pair = Element(L1LineDwordIdx(a<39:3>), L1DataToDwordList(data));
    if a<2> then inst_pair<31:0> else inst_pair<63:32>
}

-- sml helper function
unit WriteDWORD (pAddr::dwordAddr, data::dword) =
{
    var l1_data = L1DataToDwordList(0);
    l1_data <- Replace(L1LineDwordIdx (pAddr), data, l1_data);
    MEM(L1LineNumberFromDwordAddr(pAddr)) <- DwordListToL1Data(l1_data)
}

-- sml helper function
unit Write256 (pAddr::bits(35), data::bits(256)) =
    MEM(pAddr) <- data
