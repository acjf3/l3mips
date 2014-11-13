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

type L2Tag = bits(24)
record L2Entry {valid::bool tag::L2Tag sharers::NatSet data::bits(257)}
type L2SetIndex = bits(11)
type DirectMappedL2 = L2SetIndex -> L2Entry

declare L2Cache :: DirectMappedL2

declare DRAM  :: CapAddr -> bits(257) -- 257 bits accesses (256 cap/data + tag bit)

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

string log_l2_entry(entry::L2Entry) =
    "[" : [entry.valid] : "|0x" : PadLeft (#"0", 7, [entry.tag]) : "]"
    -- : "|" : [entry.data]

string log_l2_read (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_read_hit (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex, entry::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read hit, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " = " : log_257_block(entry.data) :
    " sharers = " : log_sharers(entry.sharers)

string log_l2_read_miss (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "read miss, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx])

string log_l2_evict (cacheType::L1Type, idx::L2SetIndex, old::L2Entry, new::L2Entry) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "evict @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " - old: " : log_l2_entry(old) :
    " - new: " : log_l2_entry(new)

string log_l2_write (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex, data::bits(257)) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l2_write_hit (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex, data::bits(257)) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "write hit, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " <- " : log_257_block(data)

string log_l2_updt_sharers (cacheType::L1Type, addr::CapAddr, idx::L2SetIndex, old::NatSet, new::NatSet) =
    "L2 " : " (Core:" : [procID] : " " : L1TypeToString(cacheType) : ") " :
    "update sharers, line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @idx: 0x" : PadLeft (#"0", 3, [idx]) :
    " | " : log_sharers (old) : " <- " : log_sharers (new)

string log_l2_inval_l1 (l1id::nat, addr::CapAddr, idx1::L1SetIndex, idx2::L2SetIndex) =
    "L2 inval L1 " : [l1id] :
    " line_addr: 0x" : PadLeft (#"0", 9, [addr]) :
    " @L2idx: 0x" : PadLeft (#"0", 3, [idx2]) :
    " @L1idx: 0x" : PadLeft (#"0", 3, [idx1])

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

L2SetIndex l2_hash_default(addr::CapAddr) = addr<10:0>
L2Tag l2_tag_default(addr::CapAddr) = addr<34:11>

L2SetIndex L2Idx(addr::CapAddr) = l2_hash_default(addr)
L2Tag L2Tag(addr::CapAddr) = l2_tag_default(addr)

L2Entry mkL2CacheEntry(valid::bool, tag::L2Tag, sharers::NatSet, data::bits(257)) =
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

unit L2InvalL1(addr::CapAddr, sharers::NatSet) =
{
    currentProc = procID;
    foreach sharer in sharers do
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

L2Entry option L2Hit (cacheType::L1Type, addr::CapAddr) =
{
    cacheEntry = L2Cache(L2Idx(addr));
    if (cacheEntry.valid and cacheEntry.tag == L2Tag(addr)) then
        Some (cacheEntry)
    else
        None
}

bits(257) L2ServeMiss (cacheType::L1Type, addr::CapAddr) =
{
    cap = DRAM(addr);
    mark_log(5, log_r_dram (addr, cap));
    new_sharers = L2UpdateSharers(cacheType, procID, true, Nil);
    new_entry = mkL2CacheEntry(true, L2Tag(addr), new_sharers, cap);
    old_entry = L2Cache(L2Idx(addr));
    when old_entry.valid do
    {
        mark_log (4, log_l2_evict(cacheType, L2Idx(addr), old_entry, new_entry));
        L2InvalL1(old_entry.tag:L2Idx(addr), old_entry.sharers)
    };
    L2Cache(L2Idx(addr)) <- new_entry;
    cap
}

L2Entry option L2Update (cacheType::L1Type, addr::CapAddr, tag::bool, data::bits(257), mask::bits(257)) =
    match L2Hit (cacheType, addr)
    {
        case Some (cacheEntry) =>
        {
            var new_data = mergeBlocks257 (cacheEntry.data, data, mask);
            new_data<256> <- tag;
            L2Cache(L2Idx(addr)) <- mkL2CacheEntry(true, cacheEntry.tag, cacheEntry.sharers, new_data);
            mark_log (4, log_l2_write_hit(cacheType, addr, L2Idx(addr), new_data));
            Some (L2Cache(L2Idx(addr)))
        }
        case None => None
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
        case Some (cacheEntry) => L2InvalL1(addr, cacheEntry.sharers)
        case None              => nothing
    }

bits(257) L2Read (cacheType::L1Type, addr::CapAddr) =
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
        case None => nothing
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
    L2Cache <- InitMap(mkL2CacheEntry(false, UNKNOWN, UNKNOWN, UNKNOWN))
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
