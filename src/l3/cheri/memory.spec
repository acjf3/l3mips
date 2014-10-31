--------------------------------------------------------------------------------
-- CHERI memory hierarchy (private L1s, shared L2)
-- (c) Alexandre Joannou, University of Cambridge
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- types and declarations
--------------------------------------------------------------------------------

type CacheLine = dword list

construct L1Type {Data, Instr}
string L1TypeToString (cacheType::L1Type) =
    match cacheType
    {
        case Data  => "DCache"
        case Instr => "ICache"
    }

type L1Tag = bits(26)
record L1Entry {valid::bool tag::L1Tag data::CacheLine}
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

type L2Tag = bits(24)
record L2Entry {valid::bool tag::L2Tag sharers::NatSet data::CacheLine}
type L2SetIndex = bits(11)
type DirectMappedL2 = L2SetIndex -> L2Entry

declare L2Cache :: DirectMappedL2

declare MEM  :: mAddr -> dword -- physical memory (37 bits), doubleword access

--------------------------------------------------------------------------------
-- Log utils
--------------------------------------------------------------------------------

nat L1ID (cacheType::L1Type, cid::bits(8)) =
    match cacheType
    {
        case Instr => (2 * [cid])
        case Data  => (2 * [cid]) + 1
    }

string log_cache_line (data::CacheLine) =
{
    var str = "{";
    var i::nat = 0;
    foreach elem in data do
    {
        when i > 0 do
            str <- str : ", ";
        str <- str : [i] :": 0x" : hex64(elem);
        i <- i + 1
    };
    return str : "}"
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
    -- : "|" : [entry.data]

string log_l1_read (cacheType::L1Type, addr::mAddr, idx::L1SetIndex) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) :") " :
    "read dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l1_read_hit (cacheType::L1Type, addr::mAddr, idx::L1SetIndex, data::CacheLine) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read hit dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " = " : log_cache_line(data)

string log_l1_read_miss (cacheType::L1Type, addr::mAddr, idx::L1SetIndex) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read miss dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l1_evict (cacheType::L1Type, idx::L1SetIndex, old::L1Entry, new::L1Entry) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "evict @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - old: " : log_l1_entry(old) :
    " - new: " : log_l1_entry(new)

string log_l1_write (cacheType::L1Type, addr::mAddr, idx::L1SetIndex, data::CacheLine) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_cache_line(data)

string log_l1_write_hit (cacheType::L1Type, addr::mAddr, idx::L1SetIndex, data::CacheLine) =
    "L1 " : [L1ID(cacheType, procID)] :
    " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write hit, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_cache_line(data)

string log_l2_entry(entry::L2Entry) =
    "[" : [entry.valid] : "|0x" : PadLeft (#"0", 7, [entry.tag]) : "]"
    -- : "|" : [entry.data]

string log_l2_read (cacheType::L1Type, addr::mAddr, idx::L2SetIndex) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_read_hit (cacheType::L1Type, addr::mAddr, idx::L2SetIndex, entry::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read hit, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " = " : log_cache_line(entry.data) :
    " sharers = " : log_sharers(entry.sharers)

string log_l2_read_miss (cacheType::L1Type, addr::mAddr, idx::L2SetIndex) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read miss, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_evict (cacheType::L1Type, idx::L2SetIndex, old::L2Entry, new::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "evict @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - old: " : log_l2_entry(old) :
    " - new: " : log_l2_entry(new)

string log_l2_write (cacheType::L1Type, addr::mAddr, idx::L2SetIndex, data::CacheLine) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_cache_line(data)

string log_l2_write_hit (cacheType::L1Type, addr::mAddr, idx::L2SetIndex, data::CacheLine) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write hit, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_cache_line(data)

string log_l2_updt_sharers (cacheType::L1Type, addr::mAddr, idx::L2SetIndex, old::NatSet, new::NatSet) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "update sharers, dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " | " : log_sharers (old) : " <- " : log_sharers (new)

string log_l2_inval_l1 (l1id::nat, addr::mAddr, idx1::L1SetIndex, idx2::L2SetIndex) =
    "L2 inval L1 " : [l1id] :
    " dword_addr: 0x" : PadLeft (#"0", 10, [addr]) :
    " @L2idx: 0x" : PadLeft (#"0", 3, [idx2]) :
    " @L1idx: 0x" : PadLeft (#"0", 3, [idx1])

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

dword idx(line::CacheLine, i::bits(2)) =
  if i == 0 then Head(line) else idx(Tail(line), i-1)

CacheLine upd(line::CacheLine, i::bits(2), dword::dword) =
  if i == 0 then Cons(dword, Tail(line))
            else Cons(Head(line),upd(Tail(line), i-1, dword))

-- Direct mapped L1

L1SetIndex l1_hash_default(addr::mAddr) = addr<10:2>
L1Tag l1_tag_default(addr::mAddr) = addr<36:11>

L1SetIndex L1Idx(addr::mAddr) = l1_hash_default(addr)
L1Tag L1Tag(addr::mAddr) = l1_tag_default(addr)

L1Entry mkL1CacheEntry(valid::bool, tag::L1Tag, data::CacheLine) =
{
    var line::L1Entry;
    line.valid <- valid;
    line.tag   <- tag;
    line.data  <- data;
    line
}

-- Direct mapped L2

L2SetIndex l2_hash_default(addr::mAddr) = addr<12:2>
L2Tag l2_tag_default(addr::mAddr) = addr<36:13>

L2SetIndex L2Idx(addr::mAddr) = l2_hash_default(addr)
L2Tag L2Tag(addr::mAddr) = l2_tag_default(addr)

L2Entry mkL2CacheEntry(valid::bool, tag::L2Tag, sharers::NatSet, data::CacheLine) =
{
    var line::L2Entry;
    line.valid      <- valid;
    line.tag        <- tag;
    line.sharers    <- sharers;
    line.data       <- data;
    line
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

unit L2InvalL1(addr::mAddr, sharers::NatSet) =
{
    currentProc = procID;
    foreach sharer in sharers do
    {
        procID <- [sharer::nat div 2];
        if (sharer mod 2) == 0 then
            L1Cache(Instr, L1Idx(addr)) <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN)
        else
            L1Cache(Data, L1Idx(addr))  <- mkL1CacheEntry(false, UNKNOWN, UNKNOWN);
        mark_log(4, log_l2_inval_l1 (sharer, addr, L1Idx(addr), L2Idx(addr)))
    };
    procID <- currentProc
}

L2Entry option L2Hit (cacheType::L1Type, addr::mAddr) =
{
    cacheEntry = L2Cache(L2Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L2Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

CacheLine L2ServeMiss (cacheType::L1Type, addr::mAddr) =
{
    var cacheLine = Nil;
    for i in 0 .. 3 do cacheLine <- cacheLine : list { MEM(addr<36:2> :[i]) };
    new_sharers = L2UpdateSharers(cacheType, procID, true, Nil);
    new_entry = mkL2CacheEntry(true, L2Tag(addr), new_sharers, cacheLine);
    old_entry = L2Cache(L2Idx(addr));
    when old_entry.valid do
    {
        mark_log (4, log_l2_evict(cacheType, L2Idx(addr), old_entry, new_entry));
        L2InvalL1(old_entry.tag:L2Idx(addr):'00', old_entry.sharers)
    };
    L2Cache(L2Idx(addr)) <- new_entry;
    cacheLine
}

L2Entry option L2Update (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
    match L2Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            old_cacheLine = cacheEntry.data;
            masked_data = idx(old_cacheLine, addr<1:0>) && ~mask || data && mask;
            new_cacheLine = upd(old_cacheLine, addr<1:0>, masked_data);
            L2Cache(L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_cacheLine);
            mark_log (4, log_l2_write_hit(cacheType, addr, L2Idx(addr), new_cacheLine));
            Some (L2Cache(L2Idx(addr)))
        }
        case None => None
    }

unit L2ServeWrite (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
{
    MEM(addr) <- MEM(addr) && ~mask || data && mask;
    mark_log (2, log_w_mem (addr, mask, data))
}

unit L2HandleCoherence (cacheType::L1Type, addr::mAddr, data::dword, mask::dword, entry::L2Entry option) =
    match entry
    {
        case Some (cacheEntry) => L2InvalL1(addr, cacheEntry.sharers)
        case None              => nothing
    }

CacheLine L2Read (cacheType::L1Type, addr::mAddr) =
{
    mark_log (4, log_l2_read(cacheType, addr, L2Idx(addr)));
    var cacheLine;
    match L2Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            new_sharers = L2UpdateSharers(cacheType, procID, true, cacheEntry.sharers);
            L2Cache(L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, new_sharers, cacheEntry.data);
            mark_log (4, log_l2_read_hit(cacheType, addr, L2Idx(addr), L2Cache(L2Idx(addr))));
            cacheLine <- cacheEntry.data
        }
        case None =>
        {
            mark_log (4, log_l2_read_miss(cacheType, addr, L2Idx(addr)));
            cacheLine <- L2ServeMiss(cacheType, addr)
        }
    };
    cacheLine
}

unit L2Write (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
{
    cacheEntry = L2Update(cacheType, addr, data, mask);
    L2ServeWrite(cacheType, addr, data, mask);
    L2HandleCoherence(cacheType, addr, data, mask, cacheEntry)
}

--------------------------------------------------------------------------------
-- L1 API
--------------------------------------------------------------------------------

L1Entry option L1Hit (cacheType::L1Type, addr::mAddr) =
{
    cacheEntry = L1Cache(cacheType, L1Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L1Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

CacheLine L1ServeMiss (cacheType::L1Type, addr::mAddr) =
{
    var cacheLine = L2Read(cacheType, addr);
    new_entry = mkL1CacheEntry(true, L1Tag(addr), cacheLine);
    old_entry = L1Cache(cacheType, L1Idx(addr));
    when old_entry.valid do
        mark_log (3, log_l1_evict(cacheType, L1Idx(addr), old_entry, new_entry));
    L1Cache(cacheType, L1Idx(addr)) <- new_entry;
    cacheLine
}

unit L1Update (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
    match L1Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            cacheLine = cacheEntry.data;
            masked_data = idx(cacheLine, addr<1:0>) && ~mask || data && mask;
            cacheLine = upd(cacheLine, addr<1:0>, masked_data);
            L1Cache(cacheType, L1Idx(addr)) <- mkL1CacheEntry(true, cacheEntry.tag, cacheLine);
            mark_log (3, log_l1_write_hit(cacheType, addr, L1Idx(addr), cacheLine))
        }
        case None => nothing
    }

unit L1ServeWrite (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
    L2Write(cacheType, addr, data, mask)

dword L1Read (cacheType::L1Type, addr::mAddr) =
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
    idx(cacheLine, addr<1:0>)
}

unit L1Write (cacheType::L1Type, addr::mAddr, data::dword, mask::dword) =
{
    L1Update(cacheType, addr, data, mask);
    L1ServeWrite(cacheType, addr, data, mask)
}

--------------------------------------------------------------------------------
-- Proc memory API
--------------------------------------------------------------------------------

unit InitMEM =
{
    MEM <- InitMap (UNKNOWN);
    for i in 0 .. totalCore-1 do
    {
        c_L1_data([i])  <- InitMap(mkL1CacheEntry(false, UNKNOWN, UNKNOWN));
        c_L1_instr([i]) <- InitMap(mkL1CacheEntry(false, UNKNOWN, UNKNOWN))
    };
    L2Cache <- InitMap(mkL2CacheEntry(false, UNKNOWN, UNKNOWN, UNKNOWN))
}

dword ReadData (pAddr::mAddr) =
{
    data = L1Read(Data, pAddr);
    mark_log (2, log_r_mem (pAddr, data));
    data
}

unit WriteData (pAddr::mAddr, data::dword, mask::dword) =
    L1Write(Data, pAddr, data, mask)

word ReadInst (a::pAddr) =
    if a<2> then
        L1Read (Instr, a<39:3>) <31:0>
    else
        L1Read (Instr, a<39:3>) <63:32>
>>>>>>> Adding a cheri specific memory subsystem
