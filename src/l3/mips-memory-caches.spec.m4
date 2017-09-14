---------------------------------------------------------------------------
-- BERI caches
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------
-- stats utils --
-----------------

record MemStats
{
    -- data / inst measures
       data_reads                     :: nat
       data_writes                    :: nat
       inst_reads                     :: nat
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
}

declare memStats :: MemStats

unit initMemStats =
{
    -- data / inst measures
       memStats.data_reads                    <- 0;
       memStats.data_writes                   <- 0;
       memStats.inst_reads                    <- 0;
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
       memStats.l2_prefetch_evict             <- 0
}

string MemStateLine (s::string, n::nat) =
   PadRight (#" ", 35, s) : " = " : PadLeft (#" ", 9, [n]) : "\n"

string printMemStats =
    -- data / inst measures
       MemStateLine ("data_reads",  memStats.data_reads) :
       MemStateLine ("data_writes", memStats.data_writes) :
       MemStateLine ("inst_reads",  memStats.inst_reads) :
    -- hit / miss measures
       MemStateLine ("l2_read",       memStats.l2_read) :
       MemStateLine ("l2_read_hit",   memStats.l2_read_hit) :
       MemStateLine ("l2_read_miss",  memStats.l2_read_miss) :
       MemStateLine ("l2_write",      memStats.l2_write) :
       MemStateLine ("l2_write_hit",  memStats.l2_write_hit) :
       MemStateLine ("l2_write_miss", memStats.l2_write_miss) :
    -- prefetch measures
       MemStateLine ("l2_mandatory_fetch", memStats.l2_mandatory_fetch) :
       MemStateLine ("l2_prefetch",        memStats.l2_prefetch) :
       MemStateLine ("l2_tlb_hit",         memStats.l2_tlb_hit) :
       MemStateLine ("l2_tlb_miss",        memStats.l2_tlb_miss) :
       MemStateLine ("l2_prefetch_alias",  memStats.l2_prefetch_alias) :
    -- eviction measures
       MemStateLine ("l2_evict",           memStats.l2_evict) :
       MemStateLine ("l2_mandatory_evict", memStats.l2_mandatory_evict) :
       MemStateLine ("l2_prefetch_evict",  memStats.l2_prefetch_evict)
string csvHeaderMemStats =
"data_reads,data_writes,inst_reads,l2_read,l2_read_hit,l2_read_miss,l2_write,l2_write_hit,l2_write_miss,l2_mandatory_fetch,l2_prefetch,l2_tlb_hit,l2_tlb_miss,l2_prefetch_alias,l2_evict,l2_mandatory_evict,l2_prefetch_evict"
string csvMemStats =
[memStats.data_reads] : "," :
[memStats.data_writes] : "," :
[memStats.inst_reads] : "," :
[memStats.l2_read] : "," :
[memStats.l2_read_hit] : "," :
[memStats.l2_read_miss] : "," :
[memStats.l2_write] : "," :
[memStats.l2_write_hit] : "," :
[memStats.l2_write_miss] : "," :
[memStats.l2_mandatory_fetch] : "," :
[memStats.l2_prefetch] : "," :
[memStats.l2_tlb_hit] : "," :
[memStats.l2_tlb_miss] : "," :
[memStats.l2_prefetch_alias] : "," :
[memStats.l2_evict] : "," :
[memStats.l2_mandatory_evict] : "," :
[memStats.l2_prefetch_evict]

unit clearDynamicMemStats () = nothing

-- address size in bits (40 bits)
nat L1ADDRWIDTH = 40
-- size of offset feild in bits
nat L1OFFSETWIDTH = Log2 (L1LINESIZE)
-- size of index feild in bits
nat L1INDEXWIDTH = Log2 (L1DSIZE div L1LINESIZE)
-- size of tag feild in bits
nat L1TAGWIDTH = L1ADDRWIDTH-L1INDEXWIDTH-L1OFFSETWIDTH
-- size of linenumber feild in bits
nat L1LINENUMBERWIDTH = L1ADDRWIDTH-L1OFFSETWIDTH

-- number of L1 line(s) per L2 line
nat L1LINEPERL2LINE = L2LINESIZE div L1LINESIZE
-- address size in bits (40 bits)
nat L2ADDRWIDTH = 40
-- size of offset feild in bits
nat L2OFFSETWIDTH = Log2 (L2LINESIZE)
-- size of index feild in bits
nat L2INDEXWIDTH = Log2 (L2SIZE div (L2WAYS * L2LINESIZE))
-- size of tag feild in bits
nat L2TAGWIDTH = L2ADDRWIDTH - L2INDEXWIDTH - L2OFFSETWIDTH
-- size of linenumber feild in bits
nat L2LINENUMBERWIDTH = L2ADDRWIDTH - L2OFFSETWIDTH

nat DATASIZE1 = L1LINESIZE*8
nat DATASIZE2 = L2LINESIZE*8

-----------------
-- basic types --
-----------------

-- mem types --

type dwordAddr = bits(37)
type MemAddr = bits(L2LINENUMBERWIDTH)
type MemData = bits(DATASIZE2)
MemAddr MemAddrFromDwordAddr (addr::dwordAddr) = addr<36:Log2(L2LINESIZE)-3>

-- mem log utils --

string MemAddr_str (addr::MemAddr) = hex (addr:'0')

string MemData_str (data::MemData) = "0x" : ToLower([data])

string log_mem_write (addr::MemAddr, data::MemData) =
    "write MEM[" : MemAddr_str (addr) : "] <- " : MemData_str (data)

string log_mem_read (addr::MemAddr, data::MemData) =
    "read MEM[" : MemAddr_str (addr) : "]: " : MemData_str (data)

-- l1 types --

type L1Offset = bits(L1OFFSETWIDTH)
type L1Index = bits(L1INDEXWIDTH)
type L1Tag = bits(L1TAGWIDTH)
type L1LineNumber = bits(L1LINENUMBERWIDTH)
type L1Addr = bits(L1ADDRWIDTH)
type L1Data = bits(DATASIZE1)
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

-- l2 types --

type L2Offset = bits(L2OFFSETWIDTH)
type L2Index = bits(L2INDEXWIDTH)
type L2Tag = bits(L2TAGWIDTH)
type L2LineNumber = bits(L2LINENUMBERWIDTH)
type L2Addr = bits(L2ADDRWIDTH)
type L2Data = bits(DATASIZE2)
record L2Entry {valid::bool tag::L2Tag sharers::L1Id list data::L2Data}
type DirectMappedL2 = L2Index -> L2Entry
type L2Cache = nat -> DirectMappedL2

-- instanciate l1 caches --

declare
{
    current_l1_type :: L1Type
    c_l1_dcache     :: id -> L1Cache
    c_l1_icache     :: id -> L1Cache
}

component L1Cache (idx::L1Index) :: L1Entry
{
    value =
    {
        cache = if current_l1_type == Data then c_l1_dcache (procID) else c_l1_icache (procID);
        cache (idx)
    }
    assign value =
    {
        var new_cache = if current_l1_type == Data then c_l1_dcache (procID) else c_l1_icache (procID);
        new_cache (idx) <- value;
        if current_l1_type == Data then c_l1_dcache (procID) <- new_cache else c_l1_icache (procID) <- new_cache
    }
}

L1Id L1ID =
match current_l1_type
{
    case Inst => (2 * [procID])
    case Data => (2 * [procID]) + 1
}

-- instantiate l2 cache --

declare {
  -- replacement policy   --
  l2ReplacePolicy :: nat
  l2LastVictimWay :: nat -- naive replace policy
  l2LRUBits       :: L2Index -> nat list -- LRU replace policy
  -- prefetching --
  l2PrefetchDepth :: nat
  l2Prefetcher    :: nat
  c_l2            :: nat -> DirectMappedL2
}

unit setL2 (replace_policy::nat, prefetch_depth::nat, l2_pefetcher::nat) =
{
  l2ReplacePolicy <- replace_policy;
  l2PrefetchDepth <- prefetch_depth;
  l2Prefetcher <- l2_pefetcher
}

component L2Cache (way::nat, idx::L2Index) :: L2Entry
{
    value = { m = c_l2(way); m(idx) }
    assign value = { var m = c_l2(way)
                    ; m(idx) <- value
                    ; c_l2(way) <- m }
}

-- instantiate memory --

declare MEM :: MemAddr -> MemData -- physical memory

-- general l1 utils --

L1Index l1_hash_default(addr::L1Addr) = addr<L1ADDRWIDTH-1-L1TAGWIDTH:L1ADDRWIDTH-L1TAGWIDTH-L1INDEXWIDTH>
L1Tag l1_tag_default(addr::L1Addr) = addr<L1ADDRWIDTH-1:L1ADDRWIDTH-L1TAGWIDTH>

L1Index L1Idx(addr::L1Addr) = l1_hash_default(addr)
L1Tag L1Tag(addr::L1Addr) = l1_tag_default(addr)
L1LineNumber L1LineNumber(addr::L1Addr) = addr<L1ADDRWIDTH-1:L1ADDRWIDTH-L1TAGWIDTH-L1INDEXWIDTH>

L1Index L1IdxFromLineNumber(addr::L1LineNumber) = l1_hash_default(addr:0)
L1Tag L1TagFromLineNumber(addr::L1LineNumber) = l1_tag_default(addr:0)

L1LineNumber L1LineNumberFromMemAddr (addr::MemAddr) = ifelse(eval(L2LINESIZE>L1LINESIZE), 0, `addr',`addr:0')

L1LineNumber L1LineNumberFromDwordAddr (addr::dwordAddr) = addr<36:37-L1LINENUMBERWIDTH>

nat L1LineDwordIdx (addr::dwordAddr) = [addr<Log2(L1LINESIZE div 8)-1:0>]

word list L1DataToWordList (data::L1Data) =
{
    var word_list = Nil;
    for i in L1LINESIZE div 4 .. 1 do
        word_list <- Cons (data<(i*32)-1:(i-1)*32>, word_list);
    word_list
}

dword list L1DataToDwordList (data::L1Data) =
{
    var dword_list = Nil;
    for i in L1LINESIZE div 8 .. 1 do
        dword_list <- Cons (data<(i*64)-1:(i-1)*64>, dword_list);
    dword_list
}

L1Data DwordListToL1Data (dword_list::dword list) =
{
    var data;
    var tmp_list = dword_list;
    for i in 1 .. L1LINESIZE div 8 do
    {
        data<(i*64)-1:(i-1)*64> <- Head(tmp_list);
        tmp_list <- Drop(1, tmp_list)
    };
    data
}

L1Addr L1AddrFromL2Addr(addr::L2Addr) = addr

L2Data L1DataToL2Data (addr::L1Addr, data::L1Data) =
{
    var out::L2Data = 0;
    offset::nat = ifelse(eval(L2LINESIZE/L1LINESIZE),1,0,`[addr<Log2(L1LINEPERL2LINE)+Log2(L1LINESIZE)-1:Log2(L1LINESIZE)>]');
    out<(offset+1)*DATASIZE1-1:offset*DATASIZE1> <- data;
    out
}

L1Data L2DataToL1Data (addr::L1Addr, data::L2Data) =
{
    offset::nat = ifelse(eval(L2LINESIZE/L1LINESIZE),1,0,`[addr<Log2(L1LINEPERL2LINE)+Log2(L1LINESIZE)-1:Log2(L1LINESIZE)>]');
    out = data<(offset+1)*DATASIZE1-1:offset*DATASIZE1>;
    out
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

L1Cache DirectMappedL1Init () = InitMap (mkL1CacheEntry(false, UNKNOWN(next_unknown("l1-tag")), UNKNOWN(next_unknown("l1-data"))))

-- l1 log utils --

string l1addr_str (addr::L1Addr) = hex (addr)
string l1addr_line_str (addr::L1Addr) =
    "0x" : PadLeft (#"0", (L1LINENUMBERWIDTH+3) div 4, [addr<L1ADDRWIDTH-1:L1ADDRWIDTH-L1LINENUMBERWIDTH>])
string l1addr_tag_str (addr::L1Addr) =
    "0x" : PadLeft (#"0", (L1TAGWIDTH+3) div 4, [L1Tag(addr)])
string l1addr_idx_str (addr::L1Addr) =
    [[L1Idx(addr)]::nat]
string l1line_str (line::L1LineNumber) =
    "0x" : PadLeft (#"0", (L1LINENUMBERWIDTH+3) div 4, [line])
string l1tag_str (tag::L1Tag) =
    "0x" : PadLeft (#"0", (L1TAGWIDTH+3) div 4, [tag])
string l1idx_str (idx::L1Index) =
    [[idx]::nat]

string l1data_str (data::L1Data) =
{
    var str = "0x[";
    var i::nat = 0;
    word_list = L1DataToWordList (data);
    foreach elem in word_list do
    {
        when i > 0 do str <- str : ":";
        str <- str : dhex (elem);
        i <- i + 1
    };
    return str : "]"
}

-- general l2 utils --

L2Index l2_hash_default(addr::L2Addr) = addr<L2ADDRWIDTH-1-L2TAGWIDTH:L2ADDRWIDTH-L2TAGWIDTH-L2INDEXWIDTH>
L2Tag l2_tag_default(addr::L2Addr) = addr<L2ADDRWIDTH-1:L2ADDRWIDTH-L2TAGWIDTH>

L2Index L2Idx(addr::L2Addr) = l2_hash_default(addr)
L2Tag L2Tag(addr::L2Addr) = l2_tag_default(addr)
L2LineNumber L2LineNumber(addr::L2Addr) = addr<L2ADDRWIDTH-1:L2ADDRWIDTH-L2TAGWIDTH-L2INDEXWIDTH>

MemAddr MemAddrFromL2Addr (addr::L2Addr) = L2LineNumber(addr)

L2Data L2MergeData (old::L2Data, new::L2Data, mask::L2Data) = old && ~mask || new && mask

nat L2LineDwordIdx (addr::dwordAddr) = [addr<Log2(L2LINESIZE div 8)-1:0>]

dword list L2DataToDwordList (data::L2Data) =
{
    var dword_list = Nil;
    for i in L2LINESIZE div 8 .. 1 do
        dword_list <- Cons (data<(i*64)-1:(i-1)*64>, dword_list);
    dword_list
}

L2Data DwordListToL2Data (dword_list::dword list) =
{
    var data;
    var tmp_list = dword_list;
    for i in 1 .. L2LINESIZE div 8 do
    {
        data<(i*64)-1:(i-1)*64> <- Head(tmp_list);
        tmp_list <- Drop(1, tmp_list)
    };
    data
}

L2Entry mkL2CacheEntry(valid::bool, tag::L2Tag, sharers::L1Id list, data::L2Data) =
{
    var line::L2Entry;
    line.valid   <- valid;
    line.tag     <- tag;
    line.sharers <- sharers;
    line.data    <- data;
    line
}

DirectMappedL2 DirectMappedL2Init () = InitMap (mkL2CacheEntry(false, UNKNOWN(next_unknown("l2-tag")), UNKNOWN(next_unknown("l1-sharers")), UNKNOWN(next_unknown("l2-data"))))

-- l2 log utils --

string l2addr_str (addr::L2Addr) = hex (addr)
string l2addr_line_str (addr::L2Addr) =
    "0x" : PadLeft (#"0", (L2LINENUMBERWIDTH+3) div 4, [addr<L2ADDRWIDTH-1:L2ADDRWIDTH-L2LINENUMBERWIDTH>])
string l2addr_tag_str (addr::L2Addr) =
    "0x" : PadLeft (#"0", (L2TAGWIDTH+3) div 4, [L2Tag(addr)])
string l2addr_idx_str (addr::L2Addr) =
    [[L2Idx(addr)]::nat]
string l2line_str (line::L2LineNumber) =
    "0x" : PadLeft (#"0", (L2LINENUMBERWIDTH+3) div 4, [line])
string l2tag_str (tag::L2Tag) =
    "0x" : PadLeft (#"0", (L2TAGWIDTH+3) div 4, [tag])
string l2idx_str (idx::L2Index) =
    [[idx]::nat]

string sharers_str (sharers::L1Id list) =
{
    var str = "{";
    var i::nat = 0;
    foreach sharer in sharers do
    {
        when i > 0 do
            str <- str : ",";
        str <- str : [sharer];
        i <- i + 1
    };
    return str : "}"
}

string l2data_str (data::L2Data) =
{
    var ret = "{";
    for i in L1LINEPERL2LINE .. 1 do
    {
        ret <- ret : [i-1] : "." : l1data_str(data<i*DATASIZE1-1:(i-1)*DATASIZE1>);
        when i > 1 do ret <- ret : ", "
    };
    ret <- ret : "}";
    ret
}

string l2entry_str (entry::L2Entry) =
    "[" : [entry.valid] :
    "|" : l2tag_str(entry.tag) :
    "|sharers" : sharers_str(entry.sharers) :
    "|" : l2data_str(entry.data) : "]"

string l2addr_details_str (addr::L2Addr) =
    l2addr_line_str(addr) :
    "(tag:" : l2addr_tag_str(addr) :
    ",idx:" : l2addr_idx_str(addr) : ")"

--------------
-- l2 cache --
--------------

-- l2 logs --

string l2prefix_str (addr::L2Addr) =
    "L2(core" : [[procID]::nat] : "," : l1type_str(current_l1_type) : ")" :
    "@" : l2addr_str(addr)

string log_l2_read (addr::L2Addr) =
    l2prefix_str(addr) : " - read"

string log_l2_read_hit (addr::L2Addr, way::nat, entry::L2Entry) =
    l2prefix_str(addr) : " - read hit - way " : [way] : " - " :
    l2entry_str(entry)

string log_l2_read_miss (addr::L2Addr) =
    l2prefix_str(addr) : " - read miss"

string log_l2_fill (addr::L2Addr, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(addr) : " - fill - way " : [way] : " - " :
    "old@" : l2line_str(old.tag:L2Idx(addr)) : l2entry_str(old) : " - " :
    "new@" : l2line_str(new.tag:L2Idx(addr)) : l2entry_str(new)

string log_l2_evict (addr::L2Addr, way::nat, old::L2Entry, new::L2Entry) =
    l2prefix_str(addr) : " - evict - way " : [way] : " - " :
    "old@" : l2line_str(old.tag:L2Idx(addr)) : l2entry_str(old) : " - " :
    "new@" : l2line_str(new.tag:L2Idx(addr)) : l2entry_str(new)

string log_l2_write_hit (addr::L2Addr, way::nat, data::L2Data) =
    l2prefix_str(addr) : " - write hit - way " : [way] : " - " :
    l2data_str(data)

string log_l2_write_miss (addr::L2Addr) =
    l2prefix_str(addr) : " - write miss"

string log_l2_updt_sharers (addr::L2Addr, old::L1Id list, new::L1Id list) =
    l2prefix_str(addr) : " - update sharers - " :
    sharers_str (old) : " <- " : sharers_str (new)

string log_inval_l1 (l1id::nat, addr::L1LineNumber) =
    "inval L1 " : [l1id] : " @" : l1line_str(addr) :
    " ,L1idx:" : l1addr_idx_str(addr:0)

-- l2 API --

L1Id list L2UpdateSharers (l1id::L1Id, val::bool, sharers::L1Id list) = match val
{
    case true  => if not IsMember (l1id, sharers) then Cons (l1id, sharers) else sharers
    case false => Remove (sharers, list {l1id})
}

unit invalL1 (addr::L1LineNumber, sharers::L1Id list, invalCurrent::bool) =
{
    -- backup current procID / current_l1_type --
    currentProc = procID;
    currentL1   = current_l1_type;
    currentL1Id = if current_l1_type == Inst then (procID * 2) else (procID * 2) + 1;
    foreach sharer in sharers do
    when (invalCurrent or [sharer] <> currentL1Id) do
    {
        mark_log (4, log_inval_l1 (sharer, addr));
        procID          <- [sharer::nat div 2];
        current_l1_type <- if (sharer mod 2) == 0 then Inst else Data;
        entry = L1Cache(L1IdxFromLineNumber(addr));
        -- XXX test on short tags...
        when entry.valid and entry.tag<14:0> == L1TagFromLineNumber(addr)<14:0> do
            L1Cache(L1IdxFromLineNumber(addr)) <- mkL1CacheEntry(false, UNKNOWN(next_unknown("l1-tag")), UNKNOWN(next_unknown("l1-data")))
    };
    -- restore current procId / current_l1_type --
    procID          <- currentProc;
    current_l1_type <- currentL1
}

(L2Entry * nat) option L2Hit (addr::L2Addr) =
{
    var ret = None;
    var found::nat = 0;
    for i in L2WAYS .. 1 do
    {
        cacheEntry = L2Cache(i, L2Idx(addr));
        when (cacheEntry.valid and cacheEntry.tag == L2Tag(addr)) do
        {
            found <- found + 1;
            ret <- Some (cacheEntry, i)
        }
    };
    when found > 1 do
        #UNPREDICTABLE ("Found ":[found]:" duplicates for @ ":hex (addr):" in L2 cache");
    ret
}

nat naiveReplace(addr::L2Addr) =
{
    ret = l2LastVictimWay;
    l2LastVictimWay <- (l2LastVictimWay + 1) mod L2WAYS;
    (ret + 1)
}

nat LRUReplace (addr::L2Addr) =
{
    r = Reverse(l2LRUBits(L2Idx(addr)));
    if r == Nil then 1
    else
    {
        l2LRUBits(L2Idx(addr)) <- Reverse(Tail(r));
        Head(r)
    }
}

nat innerL2ReplacePolicy (addr::L2Addr) = match l2ReplacePolicy
{
    case 0 => naiveReplace (addr)
    case 1 => LRUReplace (addr)
}

nat L2ReplacePolicy (addr::L2Addr) =
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

-- Prefetcher helpers --

pAddr option firstptr (data::dword list) = match data
{
    case Nil    => None
    case y @ ys => match tlbTryTranslation (y)
    {
        case Some (paddr) =>
        {
            memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
            Some (paddr)
        }
        case _ =>
        {
            memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1;
            firstptr (ys)
        }
    }
}

bool aliasWithAddrList (addr::L2Addr, addr_list::L2Addr list) =
{
    var alias = false;
    foreach a in addr_list do when L2Idx(a) == L2Idx(addr) do
    {
        memStats.l2_prefetch_alias <- memStats.l2_prefetch_alias + 1;
        alias <- true
    };
    alias
}

dnl-- l2 prefetchers --
define(`MACRO_PftchFirstPtr',`dnl
match firstptr (L2DataToDwordList (data))
        {
            case Some (ptr) => when not aliasWithAddrList (ptr, past_addr_list) do match L2Hit (ptr)
            {
                case None => _ = L2ServeMiss (ptr, past_addr_list)
                case _    => nothing
            }
            case _ => nothing
        }')dnl
dnl
define(`MACRO_PftchAllPtr',`dnl
foreach elem in L2DataToDwordList (data) do match tlbTryTranslation (elem)
        {
            case Some(ptr) =>
            {
                memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                when not aliasWithAddrList (ptr, past_addr_list) do match L2Hit (ptr)
                {
                    case None => _ = L2ServeMiss (ptr, past_addr_list)
                    case _ => nothing
                }
            }
            case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
        }')dnl
dnl
L2Data L2ServeMiss (addr::L2Addr, past_addr::L2Addr list) =
{
    -- read data from memory --
    data = MEM(MemAddrFromL2Addr(addr));

    -- initialize a new entry --
    var new_entry;
    new_entry <- mkL2CacheEntry(true, L2Tag(addr), L2UpdateSharers (L1ID, true, Nil), data);

    -- get current prefetch depth --
    prefetchDepth = Length (past_addr);

    -- take care of replacement --
    victimWay = L2ReplacePolicy (addr);
    old_entry = L2Cache (victimWay, L2Idx(addr));

    when old_entry.valid do
    {
        -- stats --
        memStats.l2_evict <- memStats.l2_evict + 1;
        if (prefetchDepth == 0) then
            memStats.l2_mandatory_evict <- memStats.l2_mandatory_evict + 1
        else
            memStats.l2_prefetch_evict <- memStats.l2_prefetch_evict + 1;
        -- implementation --
        mem_addr = old_entry.tag : L2Idx(addr);
        -- write cache line back to memory --
        -- when old_entry.dirty do
        mark_log (4, log_l2_evict (addr, victimWay, old_entry, new_entry));
        MEM(mem_addr) <- old_entry.data;
        -- take care of coherence --
        for i in 0 .. L1LINEPERL2LINE - 1 do
            invalL1(L1LineNumber(L1AddrFromL2Addr(addr)) + [i], old_entry.sharers, true)
    };

    -- update cache --
    mark_log (4, log_l2_fill (addr, victimWay, old_entry, new_entry));
    L2Cache(victimWay,L2Idx(addr)) <- new_entry;

    -- prefetch stats --
    if (prefetchDepth == 0) then
        memStats.l2_mandatory_fetch <- memStats.l2_mandatory_fetch + 1
    else
        memStats.l2_prefetch <- memStats.l2_prefetch + 1;
    -- prefecth --
    when prefetchDepth < l2PrefetchDepth do
    {
        past_addr_list = Cons (addr, past_addr);
        match l2Prefetcher
        {
            -- First-Ptr prefetch --
            case 0 => MACRO_PftchFirstPtr
            -- All-Ptr prefetch --
            case 1 => MACRO_PftchAllPtr
            -- no prefetch --
            case _ => nothing
        }
    };

    -- return data --
    data
}

L2Entry L2Update (addr::L2Addr, data::L2Data, mask::L2Data) =
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            memStats.l2_write_hit <- memStats.l2_write_hit + 1;
            var new_data = L2MergeData (cacheEntry.data, data, mask);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_data);
            mark_log (4, log_l2_write_hit (addr, way, new_data));
            L2Cache(way,L2Idx(addr))
        }
        case None =>
        {
            memStats.l2_write_miss <- memStats.l2_write_miss + 1;
            mark_log (4, log_l2_write_miss (addr));
            cacheLine = L2ServeMiss (addr, list{});
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

unit L2HandleCoherence (addr::L2Addr, data::L2Data, mask::L2Data, entry::L2Entry) =
  for i in 0 .. L1LINEPERL2LINE - 1 do
    invalL1(L1LineNumber(L1AddrFromL2Addr(addr)) + [i], entry.sharers, false)

L2Data L2Read (addr::L2Addr) =
{
    memStats.l2_read <- memStats.l2_read + 1;
    var cacheLine;
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            memStats.l2_read_hit <- memStats.l2_read_hit + 1;
            new_sharers = L2UpdateSharers(L1ID, true, cacheEntry.sharers);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, new_sharers, cacheEntry.data);
            mark_log (4, log_l2_read_hit(addr, way, L2Cache(way,L2Idx(addr))));
            mark_log
              (4, log_l2_updt_sharers(addr, cacheEntry.sharers, new_sharers));
            l2LRUBits(L2Idx(addr)) <- way @ l2LRUBits(L2Idx(addr));
            cacheLine <- cacheEntry.data
        }
        case None =>
        {
            memStats.l2_read_miss <- memStats.l2_read_miss + 1;
            mark_log (4, log_l2_read_miss(addr));
            cacheLine <- L2ServeMiss (addr, list{})
        }
    };
    cacheLine
}

unit L2Write (addr::L2Addr, data::L2Data, mask::L2Data) =
{
    memStats.l2_write <- memStats.l2_write + 1;
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

string l1prefix_str (addr::L1Addr) =
    "L1[core "  : [[procID]::nat] : ", " : l1type_str(current_l1_type) : "]" :
    "@line " : l1addr_line_str(addr) :
    "(tag:"  : l1addr_tag_str(addr) : ",idx:" : l1addr_idx_str(addr) : ")"

string log_l1_read (addr::L1Addr) =
    l1prefix_str (addr) : " - read"

string log_l1_read_hit (addr::L1Addr, data::L1Data) =
    l1prefix_str (addr) : " - read hit - " : l1data_str(data)

string log_l1_read_miss (addr::L1Addr) =
    l1prefix_str (addr) : " - read miss"

string log_l1_fill (addr::L1Addr, old::L1Entry, new::L1Entry) =
    l1prefix_str(addr) : " - fill - " :
    "old@" : l1line_str(old.tag:L1Idx(addr)) : l1entry_str(old) : " - " :
    "new@" : l1line_str(new.tag:L1Idx(addr)) : l1entry_str(new)

string log_l1_evict (addr::L1Addr, old::L1Entry, new::L1Entry) =
    l1prefix_str (addr) : " - evict - " :
    "old@line " : l1line_str([old.tag:L1Idx(addr)]) : l1entry_str(old) : " - " :
    "new@line " : l1line_str([new.tag:L1Idx(addr)]) : l1entry_str(new)

string log_l1_write (addr::L1Addr, data::L1Data) =
    l1prefix_str (addr) : " - write - " : l1data_str(data)

string log_l1_write_hit (addr::L1Addr, data::L1Data) =
    l1prefix_str (addr) : " - write hit - " : l1data_str(data)

string log_l1_write_miss (addr::L1Addr) =
    l1prefix_str (addr) : " - write miss"

-- l1 API --

L1Entry option L1Hit (addr::L1Addr) =
{
    cacheEntry = L1Cache(L1Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L1Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

L1Data L1ServeMiss (addr::L1Addr) =
{
    data = L2DataToL1Data(addr, L2Read (addr));
    new_entry = mkL1CacheEntry(true, L1Tag(addr), data);
    old_entry = L1Cache(L1Idx(addr));
    when old_entry.valid do
      mark_log (3, log_l1_evict(addr, old_entry, new_entry));
    mark_log (3, log_l1_fill(addr, old_entry, new_entry));
    L1Cache(L1Idx(addr)) <- new_entry;
    data
}

unit L1Update (addr::L1Addr, data::L1Data, mask::L1Data) =
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

unit L1ServeWrite (addr::L1Addr, data::L1Data, mask::L1Data) =
    L2Write(addr, L1DataToL2Data(addr, data), L1DataToL2Data(addr, mask))

L1Data L1Read (addr::L1Addr) =
{
    var cacheLine;
    match L1Hit (addr)
    {
        case Some (cacheEntry) =>
        {
            cacheLine <- cacheEntry.data;
            mark_log (3, log_l1_read_hit (addr, cacheLine))
        }
        case None =>
        {
            mark_log (3, log_l1_read_miss (addr));
            cacheLine <- L1ServeMiss (addr)
        }
    };
    cacheLine
}

unit L1Write (addr::L1Addr, data::L1Data, mask::L1Data) =
{
    L1Update (addr, data, mask);
    L1ServeWrite (addr, data, mask)
}

--------------------
-- mips interface --
--------------------

unit InitMEM =
{
    initMemStats;
    MEM <- InitMap (UNKNOWN(next_unknown("mem-data")));
    l2LastVictimWay <- 0;
    for i in L2WAYS .. 1 do
        c_l2(i) <- DirectMappedL2Init ();
    for i in 0 .. totalCore-1 do
    {
        c_l1_dcache ([i]) <- DirectMappedL1Init ();
        c_l1_icache ([i]) <- DirectMappedL1Init ()
    }
}

dword ReadData (pAddr::dwordAddr) =
{
    memStats.data_reads <- memStats.data_reads + 1;
    current_l1_type <- Data;
    data = L1Read(pAddr:0);
    Element(L1LineDwordIdx(pAddr), L1DataToDwordList(data))
}

unit WriteData (pAddr::dwordAddr, data::dword, mask::dword) =
{
    memStats.data_writes <- memStats.data_writes + 1;

    var l1_data = L1DataToDwordList(0);
    var l1_mask = L1DataToDwordList(0);
    l1_dword_idx = L1LineDwordIdx (pAddr);
    l1_data <- Update(data, l1_dword_idx, l1_data);
    l1_mask <- Update(mask, l1_dword_idx, l1_mask);
    current_l1_type <- Data;
    L1Write(pAddr:0, DwordListToL1Data(l1_data), DwordListToL1Data(l1_mask))
}

word ReadInst (a::pAddr) =
{
    memStats.inst_reads <- memStats.inst_reads + 1;
    current_l1_type <- Inst;
    data = L1Read(a);
    inst_pair = Element(L1LineDwordIdx(a<39:3>), L1DataToDwordList(data));
    if a<2> then inst_pair<31:0> else inst_pair<63:32>
}

-- sml helper function
unit WriteDWORD (pAddr::dwordAddr, data::dword) =
{
    var l2_data = L2DataToDwordList(MEM(MemAddrFromDwordAddr(pAddr)));
    l2_data <- Update(data, L2LineDwordIdx (pAddr), l2_data);
    MEM(MemAddrFromDwordAddr(pAddr)) <- DwordListToL2Data(l2_data)
}

-- sml helper function
unit Write256 (pAddr::bits(35), data::bits(256)) =
    for i in 3 .. 0 do WriteDWORD (pAddr:[i], data<((i+1)*64)-1:(i)*64>)
