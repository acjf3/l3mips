--------------------------------------------------------------------------------
-- CHERI memory hierarchy (private L1s, shared L2)
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

dnl -- math utils
define(`log2', `ifelse($1, 1, 0, `eval(1 + log2(eval($1 / 2)))')')dnl -- compute log2
dnl -- compile time values
dnl -- L1 values (direct mapped L1)
define(`L1SIZE', 16384)dnl -- L1 cache size in bytes (default 16KB)
define(`L1LINESIZE', 32)dnl -- L1 line size in bytes (default 32B)
define(`L1ADDRWIDTH', 40)dnl -- address size in 40 bits
define(`L1OFFSETWIDTH', log2(L1LINESIZE))dnl -- size of offset feild in bits
define(`L1INDEXWIDTH', eval(log2(eval(L1SIZE/L1LINESIZE))))dnl -- size of index feild in bits
define(`L1TAGWIDTH', eval(L1ADDRWIDTH-L1INDEXWIDTH-L1OFFSETWIDTH))dnl -- size of tag feild in bits
define(`L1LINENUMBERWIDTH', eval(L1ADDRWIDTH-L1OFFSETWIDTH))dnl -- size of linenumber feild in bits
dnl -- L2 values
define(`L2SIZE', ifdef(`L2SIZE', L2SIZE, 65536))dnl -- L2 cache size in bytes (default 64KB)
define(`L2WAYS', ifdef(`L2WAYS', L2WAYS, 1))dnl -- L2 associativity (default direct mapped)
define(`L2LINESIZE', ifdef(`L2LINESIZE', L2LINESIZE, 32))dnl -- L2 line size in bytes (default 32B)
define(`L2ADDRWIDTH', 40)dnl -- address size in 40 bits
define(`L2OFFSETWIDTH', log2(L2LINESIZE))dnl -- size of offset feild in bits
define(`L2INDEXWIDTH', eval(log2(eval(L2SIZE/(L2WAYS*L2LINESIZE)))))dnl -- size of index feild in bits
define(`L2TAGWIDTH', eval(L2ADDRWIDTH-L2INDEXWIDTH-L2OFFSETWIDTH))dnl -- size of tag feild in bits
define(`L2LINENUMBERWIDTH', eval(L2ADDRWIDTH-L2OFFSETWIDTH))dnl -- size of linenumber feild in bits
dnl -- indexing utils
define(`L2CHUNKIDXWIDTH', eval(log2(L2LINESIZE)-5))dnl
define(`L2CHUNKIDX', ifelse(L2LINESIZE,32,0,L2LINESIZE,64,[$1<0>],L2LINESIZE,128,[$1<1:0>]))dnl
define(`REPLACE',Take($1,$3):Cons($2,Drop($1+1,$3)))dnl
dnl

--------------------------------------------------------------------------------
-- L1 types and declarations
--------------------------------------------------------------------------------

construct L1Type {Data, Instr}

nat L1ID (cacheType::L1Type, cid::bits(8)) =
    match cacheType
    {
        case Instr => (2 * [cid])
        case Data  => (2 * [cid]) + 1
    }

type L1Tag = bits(L1TAGWIDTH)
record L1Entry {valid::bool tag::L1Tag data::bits(257)}
type L1SetIndex = bits(L1INDEXWIDTH)
type DirectMappedL1 = L1SetIndex -> L1Entry

declare
{
    c_L1_data  :: id -> DirectMappedL1
    c_L1_instr :: id -> DirectMappedL1
}

component L1Cache (cacheType::L1Type, idx::L1SetIndex) :: L1Entry
{
    value =
        match cacheType
        {
            case Data  => {m = c_L1_data(procID); m(idx)}
            case Instr => {m = c_L1_instr(procID); m(idx)}
        }
    assign value =
        match cacheType
        {
            case Data  =>
            {
                var new_cache = c_L1_data([procID]);
                new_cache(idx) <- value;
                c_L1_data([procID]) <- new_cache
            }
            case Instr  =>
            {
                var new_cache = c_L1_instr([procID]);
                new_cache(idx) <- value;
                c_L1_instr([procID]) <- new_cache
            }
        }
}

L1SetIndex l1_hash_default(addr::CapAddr) = addr<eval(34-L1TAGWIDTH):eval(35-L1TAGWIDTH-L1INDEXWIDTH)>
L1Tag l1_tag_default(addr::CapAddr) = addr<34:eval(35-L1TAGWIDTH)>

L1SetIndex L1Idx(addr::CapAddr) = l1_hash_default(addr)
L1Tag L1Tag(addr::CapAddr) = l1_tag_default(addr)

L1Entry mkL1CacheEntry(valid::bool, tag::L1Tag, data::bits(257)) =
{
    var line::L1Entry;
    line.valid <- valid;
    line.tag   <- tag;
    line.data  <- data;
    line
}

--------------------------------------------------------------------------------
-- L2 types and declarations
--------------------------------------------------------------------------------

type L2Offset = bits(L2OFFSETWIDTH)
type L2Index = bits(L2INDEXWIDTH)
type L2Tag = bits(L2TAGWIDTH)
type L2LineNumber = bits(L2LINENUMBERWIDTH)

type pfetchStats = nat * nat -- smthg * smthg
type NatSet = nat list
record L2Entry {valid::bool tag::L2Tag stats::pfetchStats sharers::NatSet data::bits(257) list}
type DirectMappedL2 = L2Index -> L2Entry
record L2MetaEntry {wasInL2::bool wasUsed::bool evictedUseful::bool}

declare l2PrefetchDepth :: nat
declare l2Prefetcher :: nat

declare c_L2 :: nat -> DirectMappedL2
component L2Cache (way::nat, idx::L2Index) :: L2Entry
{
    value = { m = c_L2(way); m(idx) }
    assign value = { var m = c_L2(way)
                    ; m(idx) <- value
                    ; c_L2(way) <- m }
}

declare l2ReplacePolicy :: nat
-- naive replace policy
declare l2LastVictimWay::nat
-- LRU replace policy
declare l2LRUBits::L2Index -> nat list

declare metaL2 :: L2LineNumber -> L2MetaEntry

L2Index l2_hash_default(addr::CapAddr) = addr<eval(34-L2TAGWIDTH):eval(35-L2TAGWIDTH-L2INDEXWIDTH)>
L2Tag l2_tag_default(addr::CapAddr) = addr<34:eval(35-L2TAGWIDTH)>

L2Index L2Idx(addr::CapAddr) = l2_hash_default(addr)
L2Tag L2Tag(addr::CapAddr) = l2_tag_default(addr)

L2Entry mkL2CacheEntry(valid::bool, tag::L2Tag, stats::pfetchStats, sharers::NatSet, data::bits(257) list) =
{
    var line::L2Entry;
    line.valid      <- valid;
    line.tag        <- tag;
    line.stats      <- stats;
    line.sharers    <- sharers;
    line.data       <- data;
    line
}

L2MetaEntry mkL2MetaEntry(wasInL2::bool, wasUsed::bool, evictedUseful::bool) =
{
    var metaEntry::L2MetaEntry;
    metaEntry.wasInL2 <- wasInL2;
    metaEntry.wasUsed <- wasUsed;
    metaEntry.evictedUseful <- evictedUseful;
    metaEntry
}

--------------------------------------------------------------------------------
-- DRAM declaration
--------------------------------------------------------------------------------

declare DRAM :: CapAddr -> bits(257) -- 257 bits accesses (256 cap/data + tag bit)

dword list block257ToDWORDList (data::bits(257)) =
    list { data<63:0>, data<127:64>, data<191:128>, data<255:192> }

-----------------
-- stats utils --
-----------------

record MemStats
{
    -- hit / miss measures
	l2_read                        :: nat
	l2_read_hit                    :: nat
	l2_read_miss                   :: nat
	l2_write                       :: nat
	l2_write_hit                   :: nat
	l2_write_miss                  :: nat
    -- prefetch measures
	l2_mandatory_fetch             :: nat
	l2_prefetch                    :: nat
	l2_tlb_hit                     :: nat
	l2_tlb_miss                    :: nat
	l2_prefetch_alias              :: nat
    -- eviction measures
	l2_evict                       :: nat
	l2_mandatory_evict             :: nat
	l2_prefetch_evict              :: nat
	l2_fetched_used_on_evict       :: nat
	l2_fetched_unused_on_evict     :: nat
	l2_prefetched_used_on_evict    :: nat
	l2_prefetched_unused_on_evict  :: nat
}

declare memStats :: MemStats

unit initMemStats =
{
    -- hit / miss measures
	memStats.l2_read                       <- 0;
	memStats.l2_read_hit                   <- 0;
	memStats.l2_read_miss                  <- 0;
	memStats.l2_write                      <- 0;
	memStats.l2_write_hit                  <- 0;
	memStats.l2_write_miss                 <- 0;
    -- prefetch measures
	memStats.l2_mandatory_fetch            <- 0;
	memStats.l2_prefetch                   <- 0;
	memStats.l2_tlb_hit                    <- 0;
	memStats.l2_tlb_miss                   <- 0;
	memStats.l2_prefetch_alias             <- 0;
    -- eviction measures
	memStats.l2_evict                      <- 0;
	memStats.l2_mandatory_evict            <- 0;
	memStats.l2_prefetch_evict             <- 0;
	memStats.l2_fetched_used_on_evict      <- 0;
	memStats.l2_fetched_unused_on_evict    <- 0;
	memStats.l2_prefetched_used_on_evict   <- 0;
	memStats.l2_prefetched_unused_on_evict <- 0
}

string printMemStats =
    -- hit / miss measures
	PadRight (#" ", 35, "l2_read")                       : " = " : PadLeft (#" ", 9, [memStats.l2_read                       :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_read_hit")                   : " = " : PadLeft (#" ", 9, [memStats.l2_read_hit                   :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_read_miss")                  : " = " : PadLeft (#" ", 9, [memStats.l2_read_miss                  :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_write")                      : " = " : PadLeft (#" ", 9, [memStats.l2_write                      :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_write_hit")                  : " = " : PadLeft (#" ", 9, [memStats.l2_write_hit                  :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_write_miss")                 : " = " : PadLeft (#" ", 9, [memStats.l2_write_miss                 :: nat]) : "\\n" : 
	-- prefetch measures
	PadRight (#" ", 35, "l2_mandatory_fetch")            : " = " : PadLeft (#" ", 9, [memStats.l2_mandatory_fetch            :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_prefetch")                   : " = " : PadLeft (#" ", 9, [memStats.l2_prefetch                   :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_tlb_hit")                    : " = " : PadLeft (#" ", 9, [memStats.l2_tlb_hit                    :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_tlb_miss")                   : " = " : PadLeft (#" ", 9, [memStats.l2_tlb_miss                   :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_prefetch_alias")             : " = " : PadLeft (#" ", 9, [memStats.l2_prefetch_alias             :: nat]) : "\\n" : 
	-- eviction measures
	PadRight (#" ", 35, "l2_evict")                      : " = " : PadLeft (#" ", 9, [memStats.l2_evict                      :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_mandatory_evict")            : " = " : PadLeft (#" ", 9, [memStats.l2_mandatory_evict            :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_prefetch_evict")             : " = " : PadLeft (#" ", 9, [memStats.l2_prefetch_evict             :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_fetched_used_on_evict")      : " = " : PadLeft (#" ", 9, [memStats.l2_fetched_used_on_evict      :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_fetched_unused_on_evict")    : " = " : PadLeft (#" ", 9, [memStats.l2_fetched_unused_on_evict    :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_prefetched_used_on_evict")   : " = " : PadLeft (#" ", 9, [memStats.l2_prefetched_used_on_evict   :: nat]) : "\\n" : 
	PadRight (#" ", 35, "l2_prefetched_unused_on_evict") : " = " : PadLeft (#" ", 9, [memStats.l2_prefetched_unused_on_evict :: nat])

--------------------------------------------------------------------------------
-- General Log utils
--------------------------------------------------------------------------------

string addr_str (capAddr::CapAddr) =
    "0x" : PadLeft (#"0", 9, [capAddr])
string addr_l1tag_str (capAddr::CapAddr) =
    "0x" : PadLeft (`#'"0", eval((L1TAGWIDTH+3)/4), [L1Tag(capAddr)])
string addr_l1idx_str (capAddr::CapAddr) =
    PadLeft (`#'"0", eval((L1INDEXWIDTH+3)/4), [[L1Idx(capAddr)]::nat])
string addr_l2tag_str (capAddr::CapAddr) =
    "0x" : PadLeft (`#'"0", eval((L2TAGWIDTH+3)/4), [L2Tag(capAddr)])
string addr_l2idx_str (capAddr::CapAddr) =
    PadLeft (`#'"0", eval((L2INDEXWIDTH+3)/4), [[L2Idx(capAddr)]::nat])
string l1tag_str (tag::L1Tag) =
    "0x" : PadLeft (`#'"0", eval((L1TAGWIDTH+3)/4), [tag])
string l2tag_str (tag::L2Tag) =
    "0x" : PadLeft (`#'"0", eval((L2TAGWIDTH+3)/4), [tag])

string block257_str (data::bits(257)) =
{
    if data<256> then
        "{cap " : log_cap_write(Capability(data)) : "}"
    else
    {
        var str = "{";
        var i::nat = 0;
        dword_list = block257ToDWORDList(data);
        foreach elem in dword_list do
        {
            when i > 0 do
                str <- str : ",";
            str <- str : [i::nat] :":0x" : hex64(elem);
            i <- i + 1
        };
        return str : "}"
    }
}

string block257_list_str (data::bits(257) list) =
{
    var str = "";
    foreach chunk in data do
        str <- str : block257_str (chunk);
    str
}

--------------------------------------------------------------------------------
-- L1 LOGS
--------------------------------------------------------------------------------

-- l1 log utils --
------------------

string l1type_str (cacheType::L1Type) =
    match cacheType
    {
        case Data  => "DCache"
        case Instr => "ICache"
    }

string l1entry_str (entry::L1Entry) =
    "[" : [entry.valid] :
    "|" : l1tag_str(entry.tag) : "]" :
    "|" : block257_str(entry.data)

string l1prefix_str (cacheType::L1Type, addr::CapAddr) =
    "L1[" : [L1ID(cacheType, procID)::nat] :
    "(core" : [[procID]::nat] : "," : l1type_str(cacheType) : ")]" :
    "@" : addr_str(addr) :
    "(tag:" : addr_l1tag_str(addr) : ",idx:" : addr_l1idx_str(addr) : ")"

-- l1 logs --
-------------

string log_l1_read (cacheType::L1Type, addr::CapAddr) =
    l1prefix_str(cacheType, addr) : " - read"

string log_l1_read_hit (cacheType::L1Type, addr::CapAddr, data::bits(257)) =
    l1prefix_str(cacheType, addr) : " - read hit - " : block257_str(data)

string log_l1_read_miss (cacheType::L1Type, addr::CapAddr) =
    l1prefix_str(cacheType, addr) : " - read miss"

string log_l1_evict (cacheType::L1Type, addr::CapAddr, old::L1Entry, new::L1Entry) =
    l1prefix_str(cacheType, addr) : " - evict - " :
    "old@" : addr_str([old.tag:L1Idx(addr)]) : l1entry_str(old) : " - " :
    "new@" : addr_str([new.tag:L1Idx(addr)]) : l1entry_str(new)

string log_l1_write (cacheType::L1Type, addr::CapAddr, data::bits(257)) =
    l1prefix_str(cacheType, addr) : " - write - " : block257_str(data)

string log_l1_write_hit (cacheType::L1Type, addr::CapAddr, data::bits(257)) =
    l1prefix_str(cacheType, addr) : " - write hit - " : block257_str(data)

string log_l1_write_miss (cacheType::L1Type, addr::CapAddr) =
    l1prefix_str(cacheType, addr) : " - write miss"

--------------------------------------------------------------------------------
-- L2 LOGS
--------------------------------------------------------------------------------

-- l2 log utils --
------------------

string sharers_str (sharers::NatSet) =
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

string l2entry_str (entry::L2Entry) =
    "[" : [entry.valid] :
    "|" : l2tag_str(entry.tag) :
    "|(lvl: " : [Fst(entry.stats)] : ",reads: " : [Snd(entry.stats)] : ")" :
    "|sharers" : sharers_str(entry.sharers) :
    "|" : block257_list_str(entry.data) : "]"

string l2addr_str (addr::CapAddr) =
    addr_str(addr) :
    "(tag:" : addr_l2tag_str(addr) :
    ",idx:" : addr_l2idx_str(addr) : ")"

string l2addr_list_str (addr_list::CapAddr list) =
{
    var str = "{";
    var i :: nat = 0;
    foreach addr in addr_list do
    {
        when i > 0 do
            str <- str : ",";
        str <- str : l2addr_str (addr);
        i <- i + 1
    };
    return str : "}"
}

string l2prefix_str (cacheType::L1Type, addr::CapAddr) =
    "L2(core" : [[procID]::nat] : "," : l1type_str(cacheType) : ")" :
    "@" : l2addr_str(addr)

string l2metaEntry_str (mentry::L2MetaEntry) =
    "l2meta(wasIn: ": [mentry.wasInL2] : ",wasUsed: " : [mentry.wasUsed] : ",killedUsefull: " : [mentry.evictedUseful] : ")"

-- l2 logs --
-------------

string log_l2_read (cacheType::L1Type, addr::CapAddr) =
    l2prefix_str(cacheType, addr) : " - read"

string log_l2_read_hit (cacheType::L1Type, addr::CapAddr, way::nat, entry::L2Entry) =
    l2prefix_str(cacheType, addr) : " - read hit - way " : [way::nat] : " - " :
    l2entry_str(entry)

string log_l2_read_miss (cacheType::L1Type, addr::CapAddr) =
    l2prefix_str(cacheType, addr) : " - read miss - " :
    l2metaEntry_str (metaL2(addr<34:eval(35-L2LINENUMBERWIDTH)>))

string log_l2_fill (cacheType::L1Type, addr::CapAddr, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(cacheType, addr) : " - fill - way " : [way::nat] : " - " :
    "old@" : addr_str(old.tag:addr<34-L2TAGWIDTH:0>) : l2entry_str(old) : " - " :
    "new@" : addr_str(new.tag:addr<34-L2TAGWIDTH:0>) : l2entry_str(new)

string log_l2_evict (cacheType::L1Type, addr::CapAddr, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(cacheType, addr) : " - evict - way " : [way::nat] : " - " :
    "old@" : addr_str(old.tag:addr<34-L2TAGWIDTH:0>) : l2entry_str(old) : " - " :
    "new@" : addr_str(new.tag:addr<34-L2TAGWIDTH:0>) : l2entry_str(new)

string log_l2_write_hit (cacheType::L1Type, addr::CapAddr, way::nat, data::bits(257)) =
    l2prefix_str(cacheType, addr) : " - write hit - way " : [way::nat] : " - " :
    block257_str(data)

string log_l2_write_miss (cacheType::L1Type, addr::CapAddr) =
    l2prefix_str(cacheType, addr) : " - write miss"

string log_l2_updt_sharers (cacheType::L1Type, addr::CapAddr, old::NatSet, new::NatSet) =
    l2prefix_str(cacheType, addr) : " - update sharers - " :
    sharers_str (old) : " <- " : sharers_str (new)

string log_l2_inval_l1 (l1id::nat, addr::CapAddr) =
    "L2 inval L1 " : [l1id::nat] : " @" : addr_str(addr) :
    " L2idx:" : addr_l2idx_str(addr) : ",L1idx:" : addr_l1idx_str(addr)

string log_l2_prefetch (cacheType::L1Type, addr::CapAddr, depth::nat, past_addr::CapAddr list) =
    l2prefix_str(cacheType, addr) : " - prefetch - " :
    "depth " : [depth] : " - " : l2addr_list_str (past_addr)

--------------------------------------------------------------------------------
-- DRAM LOGS
--------------------------------------------------------------------------------

string log_w_dram (addr::CapAddr, cap::bits(257)) =
    "write DRAM[0x" : PadLeft (#"0", 9, [addr]) :
    "] <- " : block257_str(cap)

string log_r_dram (addr::CapAddr, cap::bits(257)) =
    "read DRAM[0x" : PadLeft (#"0", 9, [addr]) :
    "]: " : block257_str(cap)

string log_w_cap_mem (addr::CapAddr, cap::bits(257)) =
    "write cap MEM[0x" : PadLeft (#"0", 9, [addr]) :
    "] <- " : block257_str(cap)

string log_r_cap_mem (addr::CapAddr, cap::bits(257)) =
    "read cap MEM[0x" : PadLeft (#"0", 9, [addr]) :
    "]: " : block257_str(cap)

-----------
-- utils --
-----------

NatSet natSetInsert(x::nat, S::NatSet) =
{
    match S
    {
        case Nil    => list { x }
        case y @ ys =>
            if   x < y then x @ y @ ys
            else
            {
                if   x == y then y @ ys
                else y @ natSetInsert(x, ys)
            }
    }
}

NatSet natSetRemove(x::nat, S::NatSet) =
{
    match S
    {
        case Nil    => Nil
        case y @ ys =>
            if x == y then ys
            else y @ natSetRemove(x, ys)
    }
}

Capability block257ToCap (data::bits(257)) =
{
    var cap::Capability;
    &cap<256>     <- data<256>;
    &cap<255:192> <- data<63:0>;
    &cap<191:128> <- data<127:64>;
    &cap<127:64>  <- data<191:128>;
    &cap<63:0>    <- data<255:192>;
    cap
}

bits(257) capToblock257 (cap::Capability) =
{
    var data::bits(257);
    data<256>     <- &cap<256>;
    data<255:192> <- &cap<63:0>;
    data<191:128> <- &cap<127:64>;
    data<127:64>  <- &cap<191:128>;
    data<63:0>    <- &cap<255:192>;
    data
}

bits(257) DWORDListToBlock257 (data::dword list) =
{
    var d = data;
    var block;
    for i in 0 .. 3 do
    {
        block<((i*64)+63):(i*64)> <- Head(d);
        d <- Tail (d)
    };
    block
}

word list capToWORDList (cap::Capability) =
    list { &cap<255:224>, &cap<223:192>, &cap<191:160>, &cap<159:128>, &cap<127:96>, &cap<95:64>, &cap<63:32>, &cap<31:0> }

word list block257ToWORDList (data::bits(257)) =
    list { data<31:0>, data<63:32>, data<95:64>, data<127:96>, data<159:128>, data<191:160>, data<223:192>, data<255:224> }

bits(257) mergeBlocks257 (old::bits(257), new::bits(257), mask::bits(257)) = old && ~mask || new && mask

dword list mergeDWORDLists (old::dword list, new::dword list, mask::dword list) =
{
    var old_ = old;
    var n = new;
    var m = mask;
    var merged;
    for i in 0 .. 3 do
    {
        merged <- merged : list{(Head(old_) && ~Head(m) || Head(n) && Head(m))};
        old_ <- Tail (old_);
        n <- Tail (n);
        m <- Tail (m)
    };
    merged
}

Capability mkCapFromDWORDList (tag::bool, data::dword list) =
{
    var d = data;
    var cap::Capability;
    &cap<256> <- tag;
    for i in 3 .. 0 do
    {
        &cap<((i*64)+63):(i*64)> <- Head(d);
        d <- Tail(d)
    };
    cap
}

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

bits(257) list * CapAddr list getCapList (addr::CapAddr, width::nat) =
{
    var caps :: bits(257) list = Nil;

    addr_list :: CapAddr list = match width
    {
        case 1 => list {addr}
        case 2 => list {addr<34:1> : '1', addr<34:1> : '0'}
        case 4 => list {addr<34:2> : '11', addr<34:2> : '10', addr<34:2> : '01', addr<34:2> : '00'}
        case _ => #UNPREDICTABLE ("Unsupported fetch width (can only be 1, 2 or 4)")
    };

    foreach address in addr_list do
    {
        cap = DRAM(address);
        caps <- Cons(cap, caps);
        mark_log(5, log_r_dram (address, cap))
    };
    (caps, addr_list)
}

dword list * CapAddr list getDWordList (addr::CapAddr, width::nat) =
{
    var dwords :: dword list = Nil;

    addr_list :: CapAddr list = match width
    {
        case 1 => list {addr}
        case 2 => list {addr<34:1> : '1', addr<34:1> : '0'}
        case 4 => list {addr<34:2> : '11', addr<34:2> : '10', addr<34:2> : '01', addr<34:2> : '00'}
        case _ => #UNPREDICTABLE ("Unsupported fetch width (can only be 1, 2 or 4)")
    };

    foreach address in addr_list do
    {
        cap = DRAM(address);
        dwords <- Cons(cap<255:192>, dwords);
        dwords <- Cons(cap<191:128>, dwords);
        dwords <- Cons(cap<127:64> , dwords);
        dwords <- Cons(cap<63:0>   , dwords);
        mark_log(5, log_r_dram (address, cap))
    };
    (dwords, addr_list)
}

NatSet L2UpdateSharers (cacheType::L1Type, cid::bits(8), val::bool, sharers::NatSet) =
    match val
    {
        case true  => natSetInsert([L1ID(cacheType,cid)], sharers)
        case false => natSetRemove([L1ID(cacheType,cid)], sharers)
    }

--------------------------------------------------------------------------------
-- L2 API
--------------------------------------------------------------------------------

unit L2InvalL1(addr::CapAddr, sharers::NatSet, invalCurrent::bool) =
when totalCore > 1 do {
    currentProc = procID;
    foreach sharer in sharers do
    when (invalCurrent or ([sharer::nat div 2] <> currentProc)) do
    {
        procID <- [sharer::nat div 2];
        if (sharer mod 2) == 0 then
        {
            entry = L1Cache(Instr, L1Idx(addr));
            when entry.valid and entry.tag<14:0> == L1Tag(addr)<14:0> do
            {
                L1Cache(Instr, L1Idx(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN);
                mark_log(4, log_l2_inval_l1 (sharer, addr))
            }
        }
        else
        {
            entry = L1Cache(Data, L1Idx(addr));
            when entry.valid and entry.tag<14:0> == L1Tag(addr)<14:0> do
            {
                L1Cache(Data, L1Idx(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN);
                mark_log(4, log_l2_inval_l1 (sharer, addr))
            }
        }
    };
    procID <- currentProc
}

(L2Entry * nat) option L2Hit (addr::CapAddr) =
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

nat naiveReplace(addr::CapAddr) =
{
    ret = l2LastVictimWay;
    l2LastVictimWay <- (l2LastVictimWay + 1) mod L2WAYS;
    (ret + 1)
}

nat LRUReplace(addr::CapAddr) =
{
    r = Reverse(l2LRUBits(L2Idx(addr)));
    if r == Nil then 1
    else
    {
        l2LRUBits(L2Idx(addr)) <- Reverse(Tail(r));
        Head(r)
    }
}

nat innerL2ReplacePolicy(addr::CapAddr) =
match l2ReplacePolicy
{
    case 0 => naiveReplace(addr)
    case 1 => LRUReplace(addr)
}

nat L2ReplacePolicy(addr::CapAddr) =
{
    var found_empty = false;
    var ret;
    -- chose empty way
    for i in L2WAYS .. 1 do
        when ! L2Cache(i,L2Idx(addr)).valid do
            {found_empty <- true; ret <- i};
    -- potential call to inner replace policy
    if ! found_empty then innerL2ReplacePolicy(addr)
    else ret
}

-- Prefetcher helpers --
------------------------

Capability option firstcap (caps::bits(257) list) = match caps
{
    case Nil    => None
    case y @ ys => if Capability(y).tag then Some(Capability(y)) else firstcap (ys)
}

pAddr option firstptr (dwords::dword list) = match dwords
{
    case Nil    => None
    case y @ ys => match tryTranslation (y)
    {
        case Some(paddr) =>
        {
            memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
            Some(paddr)
        }
        case _ =>
        {
            memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1;
            firstptr (ys)
        }
    }
}

bool aliasWithAddrList (addr::CapAddr, addr_list::CapAddr list) =
{
    var alias = false;
    foreach a in addr_list do when L2Idx(a) == L2Idx(addr) do
    {
        memStats.l2_prefetch_alias <- memStats.l2_prefetch_alias + 1;
        alias <- true
    };
    alias
}

bits(257) L2ServeMiss (cacheType::L1Type, addr::CapAddr, past_addr::CapAddr list) =
{
    cap  = DRAM(addr);

    prefetchDepth = Length (past_addr) - 1;

    caps, addr_list = getCapList(addr, eval(L2LINESIZE/32));
    dwords, _ = getDWordList(addr, eval(L2LINESIZE/32));

    var new_entry;
    if (prefetchDepth == 0) then
    {
        memStats.l2_mandatory_fetch <- memStats.l2_mandatory_fetch + 1;
        new_entry <- mkL2CacheEntry(true, L2Tag(addr), (0, 1) , L2UpdateSharers(cacheType, procID, true, Nil), caps)
    }
    else
    {
        memStats.l2_prefetch <- memStats.l2_prefetch + 1;
        new_entry <- mkL2CacheEntry(true, L2Tag(addr), (prefetchDepth, 0) , Nil, caps)
    };

    victimWay = L2ReplacePolicy(addr);
    old_entry = L2Cache(victimWay,L2Idx(addr));
    var evicted_useful = false;
    when old_entry.valid do
    {
        -- stats updates --
        memStats.l2_evict <- memStats.l2_evict + 1;
        if (prefetchDepth == 0) then
            memStats.l2_mandatory_evict <- memStats.l2_mandatory_evict + 1
        else
            memStats.l2_prefetch_evict <- memStats.l2_prefetch_evict + 1;
        if (Fst(old_entry.stats) > 0) and (Snd(old_entry.stats) > 0) then
            memStats.l2_prefetched_used_on_evict <- memStats.l2_prefetched_used_on_evict + 1
        else if (Fst(old_entry.stats) > 0) and (Snd(old_entry.stats) == 0) then
            memStats.l2_prefetched_unused_on_evict <- memStats.l2_prefetched_unused_on_evict + 1
        else if (Fst(old_entry.stats) == 0) and (Snd(old_entry.stats) > 0) then
            memStats.l2_fetched_used_on_evict <- memStats.l2_fetched_used_on_evict + 1
        else -- if (Fst(old_entry.stats) == 0) and (Snd(old_entry.stats) == 0) then
            memStats.l2_fetched_unused_on_evict <- memStats.l2_fetched_unused_on_evict + 1;
        -- stats updates --
        evicted_useful <- (Snd(old_entry.stats) > 0);
        mark_log (4, log_l2_evict(cacheType, addr, victimWay, old_entry, new_entry));
        -- actual actions --
        ifelse(L2CHUNKIDXWIDTH,0,
        -- write cache line back to memory --
        address = old_entry.tag : addr<eval(34-L2TAGWIDTH):0>;
        DRAM(address) <- Head(old_entry.data);
        mark_log(5, log_w_dram (address, Head(old_entry.data)));
        ,
        -- write cache line back to memory --
        var chunk_idx::bits(L2CHUNKIDXWIDTH) = 0;
        foreach elem in old_entry.data do
        {
            address = old_entry.tag : addr<eval(34-L2TAGWIDTH):L2CHUNKIDXWIDTH> : chunk_idx;
            chunk_idx <- chunk_idx + 1;
            DRAM(address) <- elem;
            mark_log(5, log_w_dram (address, elem))
        };)dnl
        -- take care of coherence --
        L2InvalL1(old_entry.tag:addr<eval(34-L2TAGWIDTH):0>, old_entry.sharers, true)
    };

    -- Various Prefecth Flavors --
    ------------------------------
    when (prefetchDepth < l2PrefetchDepth) do match l2Prefetcher
    {
        -- Cap Prefetch - first --
        --------------------------
        case 0 => match firstcap (caps)
        {
            case Some(cap) => match tryTranslation (cap.base + cap.offset)
            {
                case Some(paddr) =>
                {
                    memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                    when ! aliasWithAddrList (paddr<39:5>, past_addr) do match L2Hit (paddr<39:5>)
                    {
                        case None =>
                        {
                            _ = L2ServeMiss(cacheType, paddr<39:5>, Cons(paddr<39:5>, past_addr));
                            memStats.l2_prefetch <- memStats.l2_prefetch + 1;
                            mark_log(4, log_l2_prefetch (cacheType, paddr<39:5>, prefetchDepth, past_addr))
                        }
                        case _ => nothing
                    }
                }
                case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
            }
            case _ => nothing
        }
        -- Ptr Prefetch - first --
        --------------------------
        case 1 => match firstptr (dwords)
        {
            case Some(ptr) =>
            {
                -- memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1; --> taken care of in firstptr
                when ! aliasWithAddrList (ptr<39:5>, past_addr) do match L2Hit (ptr<39:5>)
                {
                    case None =>
                    {
                        _ = L2ServeMiss(cacheType, ptr<39:5>, Cons(ptr<39:5>, past_addr));
                        memStats.l2_prefetch <- memStats.l2_prefetch + 1;
                        mark_log(4, log_l2_prefetch (cacheType, ptr<39:5>, prefetchDepth, past_addr))
                    }
                    case _ => nothing
                }
            }
            case _ => nothing -- memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1 --> taken care of in firstptr
        }
        -- Cap Prefetch - all --
        ------------------------
        case 2 => foreach el2 in caps do
        {
            var el::bits(257) = el2;
            when Capability(el).tag do
            match tryTranslation (Capability(el).base + Capability(el).offset)
            {
                case Some(paddr) =>
                {
                    memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                    when ! aliasWithAddrList (paddr<39:5>, past_addr) do match L2Hit (paddr<39:5>)
                    {
                        case None =>
                        {
                            _ = L2ServeMiss(cacheType, paddr<39:5>, Cons(paddr<39:5>, past_addr));
                            memStats.l2_prefetch <- memStats.l2_prefetch + 1;
                            mark_log(4, log_l2_prefetch (cacheType, paddr<39:5>, prefetchDepth, past_addr))
                        }
                        case _ => nothing
                    }
                }
                case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
            }
        }
        -- Ptr Prefetch - all --
        ------------------------
        case 3 => foreach elem in dwords do match tryTranslation (elem)
        {
            case Some(ptr) =>
            {
                memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                when ! aliasWithAddrList (ptr<39:5>, past_addr) do match L2Hit (ptr<39:5>)
                {
                    case None =>
                    {
                        _ = L2ServeMiss(cacheType, ptr<39:5>, Cons(ptr<39:5>, past_addr));
                        memStats.l2_prefetch <- memStats.l2_prefetch + 1;
                        mark_log(4, log_l2_prefetch (cacheType, ptr<39:5>, prefetchDepth, past_addr))
                    }
                    case _ => nothing
                }
            }
            case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
        }
        case _ => nothing
    };
    -- update cache --
    foreach a in addr_list do
        metaL2(a<34:eval(35-L2LINENUMBERWIDTH)>) <- mkL2MetaEntry(true, (prefetchDepth == 0), evicted_useful);
    mark_log(4, log_l2_fill(cacheType, addr, victimWay, old_entry, new_entry));
    L2Cache(victimWay,L2Idx(addr)) <- new_entry;
    -- return --
    cap
}

L2Entry L2Update (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            var new_data = mergeBlocks257 (Element(L2CHUNKIDX(addr),cacheEntry.data), data, mask);
            new_data<256> <- tag;
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.stats, cacheEntry.sharers, REPLACE(L2CHUNKIDX(addr),new_data,cacheEntry.data));
            memStats.l2_write_hit <- memStats.l2_write_hit + 1;
            mark_log (4, log_l2_write_hit(cacheType, addr, way, new_data));
            L2Cache(way,L2Idx(addr))
        }
        case None =>
        {
            memStats.l2_write_miss <- memStats.l2_write_miss + 1;
            mark_log (4, log_l2_write_miss(cacheType, addr));
            cacheLine = L2ServeMiss (cacheType, addr, list {addr});
            var retEntry;
            match L2Hit (addr)
            {
                case Some (cacheEntry, way) =>
                {
                    var new_data = mergeBlocks257 (Element(L2CHUNKIDX(addr),cacheEntry.data), data, mask);
                    new_data<256> <- tag;
                    retEntry <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.stats, cacheEntry.sharers, REPLACE(L2CHUNKIDX(addr),new_data,cacheEntry.data));
                    L2Cache(way,L2Idx(addr)) <- retEntry
                }
                case _ => #UNPREDICTABLE ("L2 cache reached an inconsistent state (write miss -> serve miss -> no hit)")
            };
            retEntry
        }
    }

unit L2HandleCoherence (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257), entry::L2Entry) =
    L2InvalL1(addr, entry.sharers, false)

bits(257) L2Read (cacheType::L1Type, addr::CapAddr) =
{
    memStats.l2_read <- memStats.l2_read + 1;
    mark_log (4, log_l2_read(cacheType, addr));
    var cacheLine;
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            new_sharers = L2UpdateSharers(cacheType, procID, true, cacheEntry.sharers);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, (Fst(cacheEntry.stats), Snd(cacheEntry.stats)+1), new_sharers, cacheEntry.data);
            memStats.l2_read_hit <- memStats.l2_read_hit + 1;
            mark_log (4, log_l2_read_hit(cacheType, addr, way, L2Cache(way,L2Idx(addr))));
            mark_log (4, log_l2_updt_sharers(cacheType, addr, cacheEntry.sharers, new_sharers));
            l2LRUBits(L2Idx(addr)) <- way @ l2LRUBits(L2Idx(addr));
            cacheLine <- Element(L2CHUNKIDX(addr), cacheEntry.data)
        }
        case None =>
        {
            memStats.l2_read_miss <- memStats.l2_read_miss + 1;
            mark_log (4, log_l2_read_miss(cacheType, addr));
            cacheLine <- L2ServeMiss(cacheType, addr, list {addr})
        }
    };
    cacheLine
}

unit L2Write (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
{
    memStats.l2_write <- memStats.l2_write + 1;
    cacheEntry = L2Update(cacheType, addr, tag, data, mask);
    L2HandleCoherence(cacheType, addr, tag, data, mask, cacheEntry)
}

--------------------------------------------------------------------------------
-- L1 API
--------------------------------------------------------------------------------

L1Entry option L1Hit (cacheType::L1Type, addr::CapAddr) =
{
    cacheEntry = L1Cache(cacheType, L1Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L1Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

bits(257) L1ServeMiss (cacheType::L1Type, addr::CapAddr) =
{
    cap = L2Read(cacheType, addr);
    new_entry = mkL1CacheEntry(true, L1Tag(addr), cap);
    old_entry = L1Cache(cacheType, L1Idx(addr));
    when old_entry.valid do
        mark_log (3, log_l1_evict(cacheType, addr, old_entry, new_entry));
    L1Cache(cacheType, L1Idx(addr)) <- new_entry;
    cap
}

unit L1Update (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    match L1Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            var new_data = mergeBlocks257 (cacheEntry.data, data, mask);
            new_data<256> <- tag;
            L1Cache(cacheType, L1Idx(addr)) <- mkL1CacheEntry(true, cacheEntry.tag, new_data);
            mark_log (3, log_l1_write_hit(cacheType, addr, new_data))
        }
        case None => mark_log (3, log_l1_write_miss(cacheType, addr))
    }

unit L1ServeWrite (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    L2Write(cacheType, addr, tag, data, mask)

bits(257) L1Read (cacheType::L1Type, addr::CapAddr) =
{
    mark_log (3, log_l1_read(cacheType, addr));
    var cacheLine;
    match L1Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            cacheLine <- cacheEntry.data;
            mark_log (3, log_l1_read_hit(cacheType, addr, cacheLine))
        }
        case None =>
        {
            mark_log (3, log_l1_read_miss(cacheType, addr));
            cacheLine <- L1ServeMiss(cacheType, addr)
        }
    };
    cacheLine
}

unit L1Write (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
{
    L1Update(cacheType, addr, tag, data, mask);
    L1ServeWrite(cacheType, addr, tag, data, mask)
}

--------------------------------------------------------------------------------
-- Proc memory API
--------------------------------------------------------------------------------

unit InitMEM =
{
    DRAM <- InitMap (UNKNOWN);
    for i in 0 .. totalCore-1 do
    {
        c_L1_data([i])  <- InitMap(mkL1CacheEntry(false, UNKNOWN, UNKNOWN));
        c_L1_instr([i]) <- InitMap(mkL1CacheEntry(false, UNKNOWN, UNKNOWN))
    };
    for i in 1 .. L2WAYS do
        c_L2(i) <- InitMap(mkL2CacheEntry(false, UNKNOWN, UNKNOWN, UNKNOWN, UNKNOWN));
    l2LastVictimWay <- 0;
    metaL2  <- InitMap(mkL2MetaEntry(false, false, false))
}

dword ReadData (dwordAddr::bits(37)) =
{
    cap = L1Read (Data, dwordAddr<36:2>);
    data = Element ([dwordAddr<1:0>], block257ToDWORDList (cap));
    mark_log (2, log_r_mem (dwordAddr, data));
    data
}

Capability ReadCap (capAddr::CapAddr) =
{
    cap = L1Read(Data, capAddr);
    mark_log (2, log_r_cap_mem (capAddr, cap));
    block257ToCap(cap)
}

unit WriteData (dwordAddr::bits(37), data::dword, mask::dword) =
{
    mark_log(2, log_w_mem(dwordAddr, mask, data));
    var data257 = 0;
    var mask257 = 0;
    idx = [dwordAddr<1:0>];
    data257<((idx*64)+63):(idx*64)> <- data;
    mask257<((idx*64)+63):(idx*64)> <- mask;
    L1Write(Data, dwordAddr<36:2>, false, data257, mask257)
}

unit WriteCap (capAddr::CapAddr, cap::Capability) =
{
    data = capToblock257(cap);
    mark_log (2, log_w_cap_mem (capAddr, data));
    L1Write(Data, capAddr, cap.tag, data, ~0)
}

word ReadInst (a::pAddr) =
{
    cap = L1Read(Instr, a<39:5>);
    Element ([a<4:3> : ~[a<2>]], block257ToWORDList (cap))
}

-- sml helper function
unit WriteDWORD (dwordAddr::bits(37), data::dword) =
{
    var new_data = DRAM(dwordAddr<36:2>);
    new_data<256> <- false;
    new_data<([dwordAddr<1:0>]+1)*64:[dwordAddr<1:0>]*64> <- data;
    DRAM(dwordAddr<36:2>) <- new_data
}

-- sml helper function
unit Write256 (capAddr::CapAddr, data::bits(256)) =
    DRAM(capAddr) <- '0':data
