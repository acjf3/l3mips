--------------------------------------------------------------------------------
-- CHERI memory hierarchy (private L1s, shared L2)
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

dnl -- math utils
define(`log2', `ifelse($1, 1, 0, `eval(1 + log2(eval($1 / 2)))')')dnl -- compute log2
dnl -- compile time values
define(`L2SIZE', ifdef(`L2SIZE', L2SIZE, 65536))dnl -- L2 cache size in bytes (default 64KB)
define(`L2WAYS', ifdef(`L2WAYS', L2WAYS, 1))dnl -- L2 associativity (default direct mapped)
define(`L2LINESIZE', ifdef(`L2LINESIZE', L2LINESIZE, 32))dnl -- L2 line size in bytes (default 32B)
define(`L2ADDRWIDTH', 40)dnl -- address size in 40 bits
define(`L2OFFSETWIDTH', log2(L2LINESIZE))dnl -- size of offset feild in bits
define(`L2INDEXWIDTH', eval(log2(eval(L2SIZE/(L2WAYS*L2LINESIZE)))))dnl -- size of index feild in bits
define(`L2TAGWIDTH', eval(L2ADDRWIDTH-L2INDEXWIDTH-L2OFFSETWIDTH))dnl -- size of tag feild in bits
define(`L2LINENUMBERWIDTH', eval(L2ADDRWIDTH-L2OFFSETWIDTH))dnl -- size of linenumber feild in bits
dnl -- indexing utils
define(`L2CHUNKIDX', ifelse(L2LINESIZE,32,0,L2LINESIZE,64,[$1<0>],L2LINESIZE,128,[$1<1:0>]))dnl
define(`REPLACE',Take($1,$3):Cons($2,Drop($1+1,$3)))dnl
dnl

string printMemStats = " !!! unimplemented !!! \\n"

type NatSet = nat list

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

{-
(bool list) list chunkN(chunkSize :: nat, xs :: bool list) =
    if (xs == Nil) then
        Nil
    else
        Take(chunkSize, xs) @ chunkN(chunkSize, Drop(chunkSize, xs))
-}

dword list capToDWORDList (cap::Capability) =
    list { &cap<255:192>, &cap<191:128>, &cap<127:64>, &cap<63:0> }

dword list block257ToDWORDList (data::bits(257)) =
    list { data<63:0>, data<127:64>, data<191:128>, data<255:192> }

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
-- types and declarations
--------------------------------------------------------------------------------

construct L1Type {Data, Instr}
string L1TypeToString (cacheType::L1Type) =
    match cacheType
    {
        case Data  => "DCache"
        case Instr => "ICache"
    }

type L1Tag = bits(26)
record L1Entry {valid::bool tag::L1Tag data::bits(257)}
type L1SetIndex = bits(9)
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

type L2Offset = bits(L2OFFSETWIDTH)
type L2Index = bits(L2INDEXWIDTH)
type L2Tag = bits(L2TAGWIDTH)
type L2LineNumber = bits(L2LINENUMBERWIDTH)

type pfetchStats = nat * nat
record L2Entry {valid::bool tag::L2Tag stats::pfetchStats sharers::NatSet data::bits(257) list}
type DirectMappedL2 = L2Index -> L2Entry
record L2MetaEntry {wasInL2::bool wasUsed::bool evictedUseful::bool}

declare l2PtrPrefetchDepth :: nat
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

declare DRAM :: CapAddr -> bits(257) -- 257 bits accesses (256 cap/data + tag bit)

--------------------------------------------------------------------------------
-- Log utils
--------------------------------------------------------------------------------

nat L1ID (cacheType::L1Type, cid::bits(8)) =
    match cacheType
    {
        case Instr => (2 * [cid])
        case Data  => (2 * [cid]) + 1
    }

string log_257_block (data::bits(257)) =
{
    if data<256> then
        "{cap  " : log_cap_write(Capability(data)) : "}"
    else
    {
        var str = "{";
        var i::nat = 0;
        dword_list = block257ToDWORDList(data);
        foreach elem in dword_list do
        {
            when i > 0 do
                str <- str : ", ";
            str <- str : [i] :": 0x" : hex64(elem);
            i <- i + 1
        };
        return str : "}"
    }
}

string log_sharers (sharers::NatSet) =
{
    var str = "{";
    var i::nat = 0;
    foreach sharer in sharers do
    {
        when i > 0 do
            str <- str : ", ";
        str <- str : "0x" : [sharer::nat];
        i <- i + 1
    };
    return str : "}"
}

string log_l1_entry(entry::L1Entry) =
    "[" : [entry.valid] : "|0x" : PadLeft (#"0", 7, [entry.tag]) : "]"
    -- : "|" : log_257_block(entry.data)

string log_l1_read (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) :") " :
    "read line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l1_read_hit (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex, data::bits(257)) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read hit line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " = " : log_257_block(data)

string log_l1_read_miss (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read miss line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l1_evict (cacheType::L1Type, idx::L1SetIndex, old::L1Entry, new::L1Entry) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "evict @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - old: " : log_l1_entry(old) :
    " - new: " : log_l1_entry(new)

string log_l1_write (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex, data::bits(257)) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l1_write_hit (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex, data::bits(257)) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write hit, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l1_write_miss (cacheType::L1Type, addr::CapAddr, idx::L1SetIndex) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write miss line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_entry_data (data::bits(257) list) =
{
    var str = "";
    foreach chunk in data do
        str <- str : log_257_block (chunk);
    str
}

string log_l2_entry(entry::L2Entry) =
    "prefetch stats: (lvl: ": [Fst(entry.stats)] : ", reads: " : [Snd(entry.stats)] : ")" :
    " [" : [entry.valid] : "|0x" : PadLeft (#"0", 7, [entry.tag]) : "]"
    -- : "|" : log_l2_entry_data(entry.data)

string log_l2_meta_entry(mentry::L2MetaEntry) =
    "l2 meta entry (was in L2: ": [mentry.wasInL2] : ", was used: " : [mentry.wasUsed] : ", evicted useful: " : [mentry.evictedUseful] : ")"

string log_l2_read (cacheType::L1Type, addr::CapAddr, idx::L2Index) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_read_hit (cacheType::L1Type, addr::CapAddr, idx::L2Index, entry::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read hit, prefetch stats: (lvl: ": [Fst(entry.stats)] : ", reads: " : [Snd(entry.stats)] : ")":
    " line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " = " : log_l2_entry_data(entry.data) :
    " sharers = " : log_sharers(entry.sharers)

string log_l2_read_miss (cacheType::L1Type, addr::CapAddr, idx::L2Index) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read miss, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - " : log_l2_meta_entry (metaL2(addr<34:eval(35-L2LINENUMBERWIDTH)>))

string log_l2_evict (cacheType::L1Type, idx::L2Index, old::L2Entry, new::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "evict @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - old: " : log_l2_entry(old) : " | " : log_l2_meta_entry (metaL2(old.tag:idx)) :
    " - new: " : log_l2_entry(new) : " | " : log_l2_meta_entry (metaL2(new.tag:idx))

string log_l2_write (cacheType::L1Type, addr::CapAddr, idx::L2Index, data::bits(257)) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l2_write_hit (cacheType::L1Type, addr::CapAddr, idx::L2Index, data::bits(257)) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write hit, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l2_write_miss (cacheType::L1Type, addr::CapAddr, idx::L2Index) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write miss, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_updt_sharers (cacheType::L1Type, addr::CapAddr, idx::L2Index, old::NatSet, new::NatSet) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "update sharers, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " | " : log_sharers (old) : " <- " : log_sharers (new)

string log_l2_inval_l1 (l1id::nat, addr::CapAddr, idx1::L1SetIndex, idx2::L2Index) =
    "L2 inval L1 " : [l1id] :
    " line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @L2idx: 0x" : PadLeft (#"0", 3, [idx2]) :
    " @L1idx: 0x" : PadLeft (#"0", 3, [idx1])

string log_l2_pointer_prefetch (cacheType::L1Type, addr::CapAddr, idx::L2Index) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "pointer prefetch, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_w_dram (addr::CapAddr, cap::bits(257)) =
    "write DRAM[0x" : PadLeft (#"0", 9, [addr]) :
    "] <- " : log_257_block(cap)

string log_r_dram (addr::CapAddr, cap::bits(257)) =
    "read DRAM[0x" : PadLeft (#"0", 9, [addr]) :
    "]: " : log_257_block(cap)

string log_w_cap_mem (addr::CapAddr, cap::bits(257)) =
    "write cap MEM[0x" : PadLeft (#"0", 9, [addr]) :
    "] <- " : log_257_block(cap)

string log_r_cap_mem (addr::CapAddr, cap::bits(257)) =
    "read cap MEM[0x" : PadLeft (#"0", 9, [addr]) :
    "]: " : log_257_block(cap)

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

{-
dword idx(line::CacheLine, i::bits(2)) =
  if i == 0 then Head(line) else idx(Tail(line), i-1)

CacheLine upd(line::CacheLine, i::bits(2), dword::dword) =
  if i == 0 then Cons(dword, Tail(line))
            else Cons(Head(line),upd(Tail(line), i-1, dword))
-}

-- Direct mapped L1

L1SetIndex l1_hash_default(addr::CapAddr) = addr<8:0>
L1Tag l1_tag_default(addr::CapAddr) = addr<34:9>

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

-- Direct mapped L2

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

{-
bool * nat L2idxSharers (cacheType::L1Type, cid::bits(8), sharers::NatSet) =
    sharers<L1ID(cacheType,cid)>, L1ID(cacheType,cid)
-}

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
                mark_log(4, log_l2_inval_l1 (sharer, addr, L1Idx(addr), L2Idx(addr)))
            }
        }
        else
        {
            entry = L1Cache(Data, L1Idx(addr));
            when entry.valid and entry.tag<14:0> == L1Tag(addr)<14:0> do
            {
                L1Cache(Data, L1Idx(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN);
                mark_log(4, log_l2_inval_l1 (sharer, addr, L1Idx(addr), L2Idx(addr)))
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
        case Some(paddr) => Some(paddr)
        case _ => firstptr (ys)
    }
}

bits(257) L2ServeMiss (cacheType::L1Type, addr::CapAddr, prefetchDepth::nat) =
{
    cap  = DRAM(addr);

    caps, addr_list = getCapList(addr, eval(L2LINESIZE/32));
    dwords, _ = getDWordList(addr, eval(L2LINESIZE/32));

    var new_entry;
    if (prefetchDepth == l2PtrPrefetchDepth) then
        new_entry <- mkL2CacheEntry(true, L2Tag(addr), (0, 1) , L2UpdateSharers(cacheType, procID, true, Nil), caps)
    else
        new_entry <- mkL2CacheEntry(true, L2Tag(addr), (l2PtrPrefetchDepth-prefetchDepth, 0) , Nil, caps);

    victimWay = L2ReplacePolicy(addr);
    old_entry = L2Cache(victimWay,L2Idx(addr));
    var evicted_useful = false;
    when old_entry.valid do
    {
        evicted_useful <- (Snd(old_entry.stats) > 0);
        mark_log (4, log_l2_evict(cacheType, L2Idx(addr), old_entry, new_entry));
        L2InvalL1(old_entry.tag:addr<eval(34-L2TAGWIDTH):0>, old_entry.sharers, true)
    };

    -- Various Prefecth Flavors --
    ------------------------------
    when prefetchDepth > 0 do match l2Prefetcher
    {
        -- Cap Prefetch - first --
        --------------------------
        case 0 => match firstcap (caps)
        {
            case Some(cap) => match tryTranslation (cap.base + cap.offset)
            {
                case Some(paddr) => match L2Hit (paddr<39:5>)
                {
                    case None =>
                    {
                        _ = L2ServeMiss(cacheType, paddr<39:5>, prefetchDepth - 1);
                        mark_log(4, log_l2_pointer_prefetch (cacheType, paddr<39:5>, L2Idx(paddr<39:5>)))
                    }
                    case _ => nothing
                }
                case _ => nothing
            }
            case _ => nothing
        }
        -- Ptr Prefetch - first --
        --------------------------
        case 1 => match firstptr (dwords)
        {
            case Some(ptr) => match L2Hit (ptr<39:5>)
            {
                case None =>
                {
                    _ = L2ServeMiss(cacheType, ptr<39:5>, prefetchDepth - 1);
                    mark_log(4, log_l2_pointer_prefetch (cacheType, ptr<39:5>, L2Idx(ptr<39:5>)))
                }
                case _ => nothing
            }
            case _ => nothing
        }
        -- Cap Prefetch - all --
        ------------------------
        case 2 => foreach elem in caps do when Capability(elem).tag do
        match tryTranslation (Capability(elem).base + Capability(elem).offset)
        {
            case Some(paddr) => match L2Hit (paddr<39:5>)
            {
                case None =>
                {
                    _ = L2ServeMiss(cacheType, paddr<39:5>, prefetchDepth - 1);
                    mark_log(4, log_l2_pointer_prefetch (cacheType, paddr<39:5>, L2Idx(paddr<39:5>)))
                }
                case _ => nothing
            }
            case _ => nothing
        }
        -- Ptr Prefetch - all --
        ------------------------
        case 3 => foreach elem in dwords do match tryTranslation (elem)
        {
            case Some(ptr) => match L2Hit (ptr<39:5>)
            {
                case None =>
                {
                    _ = L2ServeMiss(cacheType, ptr<39:5>, prefetchDepth - 1);
                    mark_log(4, log_l2_pointer_prefetch (cacheType, ptr<39:5>, L2Idx(ptr<39:5>)))
                }
                case _ => nothing
            }
            case _ => nothing
        }
        case _ => nothing
    };
    -- update cache --
    foreach a in addr_list do
        metaL2(a<34:eval(35-L2LINENUMBERWIDTH)>) <- mkL2MetaEntry(true, (prefetchDepth == l2PtrPrefetchDepth), evicted_useful);
    L2Cache(victimWay,L2Idx(addr)) <- new_entry;
    -- return --
    cap
}

L2Entry option L2Update (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            var new_data = mergeBlocks257 (Element(L2CHUNKIDX(addr),cacheEntry.data), data, mask);
            new_data<256> <- tag;
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.stats, cacheEntry.sharers, REPLACE(L2CHUNKIDX(addr),new_data,cacheEntry.data));
            mark_log (4, log_l2_write_hit(cacheType, addr, L2Idx(addr), new_data));
            Some (L2Cache(way,L2Idx(addr)))
        }
        case None =>
        {
            mark_log (4, log_l2_write_miss(cacheType, addr, L2Idx(addr)));
            None
        }
    }

unit L2ServeWrite (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
{
    var new_data = mergeBlocks257 (DRAM(addr), data, mask);
    new_data<256> <- tag;
    DRAM(addr) <- new_data;
    mark_log (5, log_w_dram (addr, new_data))
}

unit L2HandleCoherence (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257), entry::L2Entry option) =
    match entry
    {
        case Some (cacheEntry) => L2InvalL1(addr, cacheEntry.sharers, false)
        case None              => nothing
    }

bits(257) L2Read (cacheType::L1Type, addr::CapAddr) =
{
    mark_log (4, log_l2_read(cacheType, addr, L2Idx(addr)));
    var cacheLine;
    match L2Hit (addr)
    {
        case Some (cacheEntry, way) =>
        {
            new_sharers = L2UpdateSharers(cacheType, procID, true, cacheEntry.sharers);
            L2Cache(way,L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, (Fst(cacheEntry.stats), Snd(cacheEntry.stats)+1), new_sharers, cacheEntry.data);
            mark_log (4, log_l2_read_hit(cacheType, addr, L2Idx(addr), L2Cache(way,L2Idx(addr))));
            l2LRUBits(L2Idx(addr)) <- way @ l2LRUBits(L2Idx(addr));
            cacheLine <- Element(L2CHUNKIDX(addr), cacheEntry.data)
        }
        case None =>
        {
            mark_log (4, log_l2_read_miss(cacheType, addr, L2Idx(addr)));
            cacheLine <- L2ServeMiss(cacheType, addr, l2PtrPrefetchDepth)
        }
    };
    cacheLine
}

unit L2Write (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
{
    cacheEntry = L2Update(cacheType, addr, tag, data, mask);
    L2ServeWrite(cacheType, addr, tag, data, mask);
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
        mark_log (3, log_l1_evict(cacheType, L1Idx(addr), old_entry, new_entry));
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
            mark_log (3, log_l1_write_hit(cacheType, addr, L1Idx(addr), new_data))
        }
        case None => mark_log (3, log_l1_write_miss(cacheType, addr, L1Idx(addr)))
    }

unit L1ServeWrite (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    L2Write(cacheType, addr, tag, data, mask)

bits(257) L1Read (cacheType::L1Type, addr::CapAddr) =
{
    mark_log (3, log_l1_read(cacheType, addr, L1Idx(addr)));
    var cacheLine;
    match L1Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            cacheLine <- cacheEntry.data;
            mark_log (3, log_l1_read_hit(cacheType, addr, L1Idx(addr), cacheLine))
        }
        case None =>
        {
            mark_log (3, log_l1_read_miss(cacheType, addr, L1Idx(addr)));
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
    if a<2> then
        L1Read (Instr, a<39:3>) <31:0>
    else
        L1Read (Instr, a<39:3>) <63:32>
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
