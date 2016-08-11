---------------------------------------------------------------------------
-- CHERI caches
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------
-- stats utils --
-----------------

record MemStats
{
    -- data / inst / cap measures
    data_reads                     :: nat
    data_writes                    :: nat
    inst_reads                     :: nat
    cap_reads                      :: nat
    cap_writes                     :: nat
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
    -- data / inst / cap measures
	memStats.data_reads                    <- 0;
	memStats.data_writes                   <- 0;
	memStats.inst_reads                    <- 0;
	memStats.cap_reads                     <- 0;
	memStats.cap_writes                    <- 0;
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
   PadRight (#" ", 35, s) : " = " : PadLeft (#" ", 9, [n]) : "\\n"

string printMemStats =
    -- data / inst / cap measures
       MemStateLine ("data_reads",  memStats.data_reads) :
       MemStateLine ("data_writes", memStats.data_writes) :
       MemStateLine ("inst_reads",  memStats.inst_reads) :
       MemStateLine ("cap_reads",   memStats.cap_reads) :
       MemStateLine ("cap_writes",  memStats.cap_writes) :
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

-----------------
-- basic types --
-----------------

-- mem types --

type dwordAddr = bits(37)
type capAddr   = CAPADDR
construct MemData {Cap :: Capability, Raw :: CAPRAWBITS}
CAPRAWBITS RawMemData (data::MemData) = match data
{
    case Cap(cap) => capToBits(cap)
    case Raw(raw) => raw
}
type MemAddr = capAddr

-- mem log utils --

string MemAddr_str (addr::MemAddr) =
    "0x" : hex40(addr:0)

string RawMemData_str (data::CAPRAWBITS) =
{
    var ret = "[";
    for i in ((CAPBYTEWIDTH*8) div 32) .. 1 do
    {
        tmp::bits(32) = data<(i*32)-1:(i-1)*32>;
        ret <- ret : hex32 (tmp);
        when i > 1 do ret <- ret : ":"
    };
    ret <- ret : "]";
    ret
}

string dword_list_str (dword_list::dword list) =
{
    var ret = "{";
    var i::nat = 0;
    foreach dw in dword_list do
    {
        when i > 0 do ret <- ret : ", ";
        ret <- ret : [i] :".[":hex32(dw<63:32>):":":hex32(dw<31:0>):"]";
        i <- i + 1
    };
    ret <- ret : "}";
    ret
}

string MemData_str (data::MemData) = match data
{
    case Cap (cap) => if getTag(cap) then "cap " : log_cap_write(cap) else RawMemData_str (capToBits(cap))
    case Raw (raw) => RawMemData_str(raw)
}

string log_mem_write (addr::MemAddr, data::MemData) =
    "write MEM[" : MemAddr_str (addr) : "] <- " : MemData_str (data)

string log_mem_read (addr::MemAddr, data::MemData) =
    "read MEM[" : MemAddr_str (addr) : "]: " : MemData_str (data)

-- L1 compile time values (direct mapped L1)
nat L1LINEPERL2LINE = L2LINESIZE div L1LINESIZE
-- address size in bits (40 bits)
nat L1ADDRWIDTH = 40
-- size of offset feild in bits
nat L1OFFSETWIDTH = Log2(L1LINESIZE)
-- size of index feild in bits
nat L1INDEXWIDTH = Log2(L1SIZE div L1LINESIZE)
-- size of tag feild in bits
nat L1TAGWIDTH = L1ADDRWIDTH-L1INDEXWIDTH-L1OFFSETWIDTH
-- size of linenumber feild in bits
nat L1LINENUMBERWIDTH = L1ADDRWIDTH-L1OFFSETWIDTH

type L1Offset = bits(L1OFFSETWIDTH)
type L1Index = bits(L1INDEXWIDTH)
type L1Tag = bits(L1TAGWIDTH)
type L1LineNumber = bits(L1LINENUMBERWIDTH)
type L1Addr = bits(L1ADDRWIDTH)
type L1Data = MemData list
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

-- L2 compile time values
-- address size in bits (40 bits)
nat L2ADDRWIDTH = 40
-- size of offset feild in bits
nat L2OFFSETWIDTH = Log2(L2LINESIZE)
-- size of index feild in bits
nat L2INDEXWIDTH = Log2(L2SIZE div (L2WAYS*L2LINESIZE))
-- size of tag feild in bits
nat L2TAGWIDTH = L2ADDRWIDTH-L2INDEXWIDTH-L2OFFSETWIDTH
-- size of linenumber feild in bits
nat L2LINENUMBERWIDTH = L2ADDRWIDTH-L2OFFSETWIDTH

type L2Offset = bits(L2OFFSETWIDTH)
type L2Index = bits(L2INDEXWIDTH)
type L2Tag = bits(L2TAGWIDTH)
type L2LineNumber = bits(L2LINENUMBERWIDTH)
type L2Addr = bits(L2ADDRWIDTH)
type L2Data = MemData list
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

-- instanciate l2 cache --

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

-- instanciate memory --

declare MEM :: MemAddr -> MemData -- physical memory

-- general l1 utils --

L1Index l1_hash_default(addr::L1Addr) = addr<(L1ADDRWIDTH-L1TAGWIDTH-1):(L1ADDRWIDTH-L1TAGWIDTH-L1INDEXWIDTH)>
L1Tag l1_tag_default(addr::L1Addr) = addr<(L1ADDRWIDTH-1):(L1ADDRWIDTH-L1TAGWIDTH)>

L1Index L1Idx(addr::L1Addr) = l1_hash_default(addr)
L1Tag L1Tag(addr::L1Addr) = l1_tag_default(addr)
L1LineNumber L1LineNumber(addr::L1Addr) = addr<(L1ADDRWIDTH-1):(L1ADDRWIDTH-L1TAGWIDTH-L1INDEXWIDTH)>

L1Index L1IdxFromLineNumber(addr::L1LineNumber) = l1_hash_default(addr:0)
L1Tag L1TagFromLineNumber(addr::L1LineNumber) = l1_tag_default(addr:0)

L1LineNumber L1LineNumberFromMemAddr (addr::MemAddr) = addr<(40-Log2(CAPBYTEWIDTH)-1):(40-Log2(CAPBYTEWIDTH)-L1LINENUMBERWIDTH)>

nat L1LineDwordIdx (addr::dwordAddr) = [addr<(Log2(L1LINESIZE div 8)-1):0>]

L1Addr L1AddrFromL2Addr(addr::L2Addr) = addr

dword list L1DataToDwordList (data::L1Data) =
{
    var dword_list = Nil;
    foreach elem in Reverse (data) do
    {
        raw = RawMemData(elem);
        for i in (CAPBYTEWIDTH div 8) .. 1 do
            dword_list <- Cons (raw<(i*64)-1:(i-1)*64>, dword_list)
    };
    dword_list
}

L1Data DwordListToL1Data (dword_list::dword list) =
{
    var data::L1Data = Nil;
    var tmp_list::dword list = Reverse (dword_list);
    var tmp_elem::CAPRAWBITS;
    for i in CAPPERL1LINE .. 1 do
    {
        for j in (CAPBYTEWIDTH div 8) .. 1 do
        {
            tmp_elem<(j*64)-1:(j-1)*64> <- Head(tmp_list);
            tmp_list <- Drop(1, tmp_list)
        };
        data <- Cons (Raw(tmp_elem), data)
    };
    data
}

nat OFFSET (addr::L1Addr) =
{
    mask::L1Addr = [L1LINEPERL2LINE-1];
    if L1LINEPERL2LINE == 1 then 0 else [mask && (addr >>+ Log2(L1LINESIZE))]
}

L2Data L1DataToL2Data (addr::L1Addr, data::L1Data) =
{
    var inpt = Reverse(data);
    var out = Nil;
    for i in (L1LINEPERL2LINE-1) .. 0 do
        if i == OFFSET(addr) then for j in 1 .. CAPPERL1LINE do
        {
            out <- Cons(Head(inpt), out);
            inpt <- Drop(1,inpt)
        }
        else for j in 1 .. CAPPERL1LINE do
            out <- Cons(Raw(0), out);
    out
}

L1Data L2DataToL1Data (addr::L1Addr, data::L2Data) = Take(CAPPERL1LINE, Drop(OFFSET(addr)*CAPPERL1LINE, data))

L1Data L1MergeData (old::L1Data, new::L1Data, mask::L1Data) =
{
    var tmp_old = old;
    var tmp_new = new;
    var tmp_mask = mask;
    var tmp = Nil;
    for i in CAPPERL1LINE .. 1 do
    {
        o = Head(tmp_old);
        n = Head(tmp_new);
        m = Head(tmp_mask);
        if RawMemData(m) <> 0 then match n
        {
            case Raw (d) => tmp <- Cons(Raw(RawMemData(o) && ~RawMemData(m) || d && RawMemData(m)), tmp)
            case Cap (c) => tmp <- Cons(Cap(c), tmp)
        }
        else
            tmp <- Cons(o, tmp);
        tmp_old <- Drop(1, tmp_old);
        tmp_new <- Drop(1, tmp_new);
        tmp_mask <- Drop(1, tmp_mask)
    };
    Reverse(tmp)
}

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

string l1addr_str (addr::L1Addr) =
    "0x" : hex40(addr)
string l1addr_line_str (addr::L1Addr) =
    "0x" : PadLeft (`#'"0", ((L1LINENUMBERWIDTH+3) div 4), [addr<(L1ADDRWIDTH-1):(L1ADDRWIDTH-L1LINENUMBERWIDTH)>])
string l1addr_tag_str (addr::L1Addr) =
    "0x" : PadLeft (`#'"0", ((L1TAGWIDTH+3) div 4), [L1Tag(addr)])
string l1addr_idx_str (addr::L1Addr) =
    [[L1Idx(addr)]::nat]
string l1line_str (line::L1LineNumber) =
    "0x" : PadLeft (`#'"0", ((L1LINENUMBERWIDTH+3) div 4), [line])
string l1tag_str (tag::L1Tag) =
    "0x" : PadLeft (`#'"0", ((L1TAGWIDTH+3) div 4), [tag])
string l1idx_str (idx::L1Index) =
    [[idx]::nat]

string l1data_str (data::L1Data) =
{
    var ret = "{";
    var i::nat = 0;
    foreach elem in data do
    {
        when i > 0 do ret <- ret : ", ";
        ret <- ret : [i] : ":" : MemData_str(elem);
        i <- i+1
    };
    ret <- ret : "}";
    ret
}

-- general l2 utils --

L2Index l2_hash_default(addr::L2Addr) = addr<(L2ADDRWIDTH-1-L2TAGWIDTH):(L2ADDRWIDTH-L2TAGWIDTH-L2INDEXWIDTH)>
L2Tag l2_tag_default(addr::L2Addr) = addr<(L2ADDRWIDTH-1):(L2ADDRWIDTH-L2TAGWIDTH)>

L2Index L2Idx(addr::L2Addr) = l2_hash_default(addr)
L2Tag L2Tag(addr::L2Addr) = l2_tag_default(addr)

L2Addr L2AddrFromL1Addr (addr::L1Addr) = addr
MemAddr MemAddrFromL2Addr (addr::L2Addr) = addr<(L2ADDRWIDTH-1):Log2(CAPBYTEWIDTH)>

L2Data L2MergeData (old::L2Data, new::L2Data, mask::L2Data) =
{
    var tmp_old = old;
    var tmp_new = new;
    var tmp_mask = mask;
    var tmp = Nil;
    for i in CAPPERL2LINE .. 1 do
    {
        o = Head(tmp_old);
        n = Head(tmp_new);
        m = Head(tmp_mask);
        if RawMemData(m) <> 0 then match n
        {
            case Raw (d) => tmp <- Cons(Raw(RawMemData(o) && ~RawMemData(m) || d && RawMemData(m)), tmp)
            case Cap (c) => tmp <- Cons(Cap(c), tmp)
        }
        else
            tmp <- Cons(o, tmp);
        tmp_old <- Drop(1, tmp_old);
        tmp_new <- Drop(1, tmp_new);
        tmp_mask <- Drop(1, tmp_mask)
    };
    Reverse(tmp)
}

dword list L2DataToDwordList (data::L2Data) =
{
    var dword_list = Nil;
    foreach elem in Reverse (data) do
    {
        raw = RawMemData(elem);
        for i in (CAPBYTEWIDTH div 8) .. 1 do
            dword_list <- Cons (raw<(i*64)-1:(i-1)*64>, dword_list)
    };
    dword_list
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

DirectMappedL2 DirectMappedL2Init () = InitMap (mkL2CacheEntry(false, UNKNOWN, UNKNOWN, UNKNOWN))

-- l2 log utils --

string l2addr_str (addr::L2Addr) =
    "0x" : hex40(addr)
string l2addr_line_str (addr::L2Addr) =
    "0x" : PadLeft (`#'"0", ((L2LINENUMBERWIDTH+3) div 4), [addr<(L2ADDRWIDTH-1):(L2ADDRWIDTH-L2LINENUMBERWIDTH)>])
string l2addr_tag_str (addr::L2Addr) =
    "0x" : PadLeft (`#'"0", ((L2TAGWIDTH+3) div 4), [L2Tag(addr)])
string l2addr_idx_str (addr::L2Addr) =
    [[L2Idx(addr)]::nat]
string l2addr_details_str (addr::L2Addr) =
    l2addr_str(addr) :
    "(tag " : l2addr_tag_str(addr) :
    ", idx " : l2addr_idx_str(addr) : ")"
string l2line_str (line::L2LineNumber) =
    "0x" : PadLeft (`#'"0", ((L2LINENUMBERWIDTH+3) div 4), [line])
string l2tag_str (tag::L2Tag) =
    "0x" : PadLeft (`#'"0", ((L2TAGWIDTH+3) div 4), [tag])
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
    var i::nat = 0;
    foreach elem in data do
    {
        when i > 0 do ret <- ret : ", ";
        ret <- ret : [i] : ":" : MemData_str(elem);
        i <- i+1
    };
    ret <- ret : "}";
    ret
}

string l2entry_str (entry::L2Entry) =
    "[" : [entry.valid] :
    "|" : l2tag_str(entry.tag) :
    "|sharers" : sharers_str(entry.sharers) :
    "|" : l2data_str(entry.data) : "]"

--------------
-- l2 cache --
--------------

-- l2 logs --

string l2prefix_str (addr::L2Addr) =
    "L2(core" : [[procID]::nat] : "," : l1type_str(current_l1_type) : ")" :
    "@" : l2addr_details_str(addr)

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

string log_l2_write_hit (addr::L2Addr, way::nat, new::L2Entry) =
    l2prefix_str(addr) : " - write hit - way " : [way] : " - " :
    l2entry_str(new)

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
            L1Cache(L1IdxFromLineNumber(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN)
    };
    -- restore current procId / current_l1_type --
    procID          <- currentProc;
    current_l1_type <- currentL1
}

(L2Entry * nat) option L2Hit (addr::L2Addr) =
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
    case 1 => LRUReplace   (addr)
    case _ => naiveReplace (addr)
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

Capability option firstcap (data::L2Data) = match data
{
    case Cap(y) @ ys => if getTag(y) then Some(y) else firstcap (ys)
    case Raw(y) @ ys => firstcap (ys)
    case _           => None
}

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
                case None =>
                {
                    mark_log (5, "firstPtr prefetcher triggered");
                    _ = L2ServeMiss (ptr, past_addr_list);
                    nothing
                }
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
                    case None =>
                    {
                        mark_log (5, "allPtr prefetcher triggered");
                        _ = L2ServeMiss (ptr, past_addr_list);
                        nothing
                    }
                    case _ => nothing
                }
            }
            case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
        }')dnl
dnl
define(`MACRO_PftchFirstCap',`dnl
match firstcap (data)
        {
            case Some(cap) => match tlbTryTranslation (getBase(cap) + getOffset(cap))
            {
                case Some(paddr) =>
                {
                    memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                    when not aliasWithAddrList (paddr, past_addr_list) do match L2Hit (paddr)
                    {
                        case None =>
                        {
                            mark_log (5, "firstCap prefetcher triggered");
                            _ = L2ServeMiss (paddr, past_addr_list);
                            nothing
                        }
                        case _ => nothing
                    }
                }
                case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
            }
            case _ => nothing
        }')dnl
dnl
define(`MACRO_PftchAllCap',`dnl
foreach elem in data do match elem
        {
            case Cap (cap) =>
            when getTag(cap) do match tlbTryTranslation (getBase(cap) + getOffset(cap))
            {
                case Some(paddr) =>
                {
                    memStats.l2_tlb_hit <- memStats.l2_tlb_hit + 1;
                    when not aliasWithAddrList (paddr, past_addr_list) do match L2Hit (paddr)
                    {
                        case None =>
                        {
                            mark_log (5, "allCap prefetcher triggered");
                            _ = L2ServeMiss (paddr, past_addr_list);
                            nothing
                        }
                        case _ => nothing
                    }
                }
                case _ => memStats.l2_tlb_miss <- memStats.l2_tlb_miss + 1
            }
            case _ => nothing
        }
')dnl
dnl
L2Data L2ServeMiss (addr::L2Addr, past_addr::L2Addr list) =
{
    -- read data from memory --
    var data = Nil;
    for i in (CAPPERL2LINE - 1) .. 0 do
    {
        chunck_addr = MemAddrFromL2Addr(addr)<(40-Log2(CAPBYTEWIDTH)-1):(Log2(CAPPERL2LINE))> ifelse(CAPPERL2LINE,1,,`: [i]');
        data <- Cons(MEM(chunck_addr), data)
    };

    -- initialize a new entry --
    var new_entry;
    new_entry <- mkL2CacheEntry(true, L2Tag(addr), L2UpdateSharers (L1ID, true, Nil), data);

    -- get current prefetch depth --
    prefetchDepth = Length (past_addr);

    -- take care of replacement --
    victimWay = L2ReplacePolicy (addr);
    old_entry = L2Cache (victimWay,L2Idx(addr));

    when old_entry.valid do
    {
        -- stats --
        memStats.l2_evict <- memStats.l2_evict + 1;
        if (prefetchDepth == 0) then
            memStats.l2_mandatory_evict <- memStats.l2_mandatory_evict + 1
        else
            memStats.l2_prefetch_evict <- memStats.l2_prefetch_evict + 1;
        -- implementation --
        mark_log (4, log_l2_evict (addr, victimWay, old_entry, new_entry));
        var tmp_data = old_entry.data;
        for i in 0 .. (CAPPERL2LINE - 1) do
        {
            -- write cache line back to memory --
            -- when old_entry.dirty do
            mem_addr = old_entry.tag : MemAddrFromL2Addr(addr)<(40-Log2(CAPBYTEWIDTH)-L2TAGWIDTH-1):(Log2(CAPPERL2LINE))> ifelse(CAPPERL2LINE,1,,`: [i]');
            MEM(mem_addr) <- Head(tmp_data);
            tmp_data <- Drop(1, tmp_data);
            -- take care of coherence --
            invalL1(L1LineNumber(L1AddrFromL2Addr(addr)) + [i], old_entry.sharers, true)
        }
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
            -- First-Cap prefetch --
            case 2 => MACRO_PftchFirstCap
            -- All-Cap Prefetch --
            case 3 => MACRO_PftchAllCap
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
            new_entry = mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_data);
            L2Cache(way,L2Idx(addr)) <- new_entry;
            mark_log (4, log_l2_write_hit (addr, way, new_entry));
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
                case _ => #UNPREDICTABLE ("L2 cache reached an inconsistent state (write miss -> serve miss -> no hit for @ " : l2addr_details_str(addr) : ")")
            };
            retEntry
        }
    }

unit L2HandleCoherence (addr::L2Addr, data::L2Data, mask::L2Data, entry::L2Entry) =
for i in 0 .. (L1LINEPERL2LINE - 1) do
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
    data = L2DataToL1Data (addr, L2Read (L2AddrFromL1Addr(addr)));
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
    MEM <- InitMap (UNKNOWN);
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

    var zeros = Nil;
    for i in 1 .. CAPPERL1LINE do zeros <- Cons (Raw(0), zeros);
    var l1_data = L1DataToDwordList(zeros);
    var l1_mask = L1DataToDwordList(zeros);
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

Capability ReadCap (capAddr::CAPADDR) =
{
    memStats.cap_reads <- memStats.cap_reads + 1;
    current_l1_type <- Data;
    elem = if CAPPERL1LINE == 1 then Head(L1Read(capAddr:0)) else Element([capAddr<(Log2(CAPPERL1LINE)-1):0>],L1Read(capAddr:0));
    data = match elem
    {
        case Cap (cap) => cap
        case Raw (raw) => bitsToCap(raw)
    };
    mark_log(4, "read " : (if getTag(data) then "valid" else "invalid") :
                " cap from 0x" : hex40(capAddr:0));
    data
}

unit WriteCap (capAddr::CAPADDR, cap::Capability) =
{
    memStats.cap_writes <- memStats.cap_writes + 1;
    var zeros = Nil;
    for i in 1 .. CAPPERL1LINE do zeros <- Cons (Raw(0), zeros);
    var l1_data = zeros;
    var l1_mask = zeros;
    offst::nat = if CAPPERL1LINE == 1 then 0 else [capAddr<(Log2(CAPPERL1LINE)-1):0>];
    l1_data <- Update(Cap(cap), offst, l1_data);
    l1_mask <- Update(Raw(~0), offst, l1_mask);
    current_l1_type <- Data;
    L1Write(capAddr:0, l1_data, l1_mask);
    mark_log(4, "write " : (if getTag(cap) then "valid" else "invalid") :
                " cap @ 0x" : hex40(capAddr:0))
}
