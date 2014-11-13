---------------------------------------------------------------------------
-- MIPS memory with private L1 DCache
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- types and declarations
-------------------------------------------------------------------------------

type CacheLine = dword list
type Tag = bits(26)
record CacheEntry {valid::bool tag::Tag data::CacheLine}
type CacheSetIndex = bits(9)
type DirectMappedCache = CacheSetIndex -> CacheEntry

declare c_dl1 :: id -> DirectMappedCache
component DL1 (idx::CacheSetIndex) :: CacheEntry
{
   value = { m = c_dl1(procID); m(idx) }
   assign value = { var m = c_dl1(procID)
                  ; m(idx) <- value
                  ; c_dl1(procID) <- m }
}
declare MEM  :: mAddr -> dword -- physical memory (37 bits), doubleword access

-------------------------------------------------------------------------------
-- Log utils
-------------------------------------------------------------------------------

string log_cache_entry(entry::CacheEntry) = "[":[entry.valid]:"|0x":PadLeft (#"0", 7, [entry.tag]):"]"--:"|":[entry.data]

string log_dcache_evict (idx::CacheSetIndex, old::CacheEntry, new::CacheEntry) =
    "[Core:":[procID]:"] DCache evict @idx 0x":PadLeft (#"0", 3, [idx]):
    " - old: ":log_cache_entry(old):
    " - new: ":log_cache_entry(new)

string log_dcache_read_hit (addr::mAddr, idx::CacheSetIndex) =
    "[Core:":[procID]:"] DCache read hit, addr:0x":PadLeft (#"0", 10, [addr]):" @idx 0x":PadLeft (#"0", 3, [idx])

-------------------------------------------------------------------------------
-- Internal functions
-------------------------------------------------------------------------------

dword idx(line::CacheLine, i::bits(2)) =
  if i == 0 then Head(line) else idx(Tail(line), i-1)

CacheLine upd(line::CacheLine, i::bits(2), dword::dword) =
  if i == 0 then Cons(dword, Tail(line))
            else Cons(Head(line),upd(Tail(line), i-1, dword))

CacheSetIndex hash_default(addr::mAddr) = addr<10:2>
Tag tag_default(addr::mAddr) = addr<36:11>

{- experiments
CacheSetIndex hash_test1(addr::mAddr) = [addr<9:2>]:[addr<12>]
Tag tag_test1(addr::mAddr) = addr<36:13> : addr<11:10>

CacheSetIndex hash_test2(addr::mAddr) = [addr<12>]:[addr<9:2>]
Tag tag_test2(addr::mAddr) = addr<36:13> : addr<11:10>
-}

CacheSetIndex getIdx(addr::mAddr) = hash_default(addr)
Tag getTag(addr::mAddr) = tag_default(addr)

CacheEntry option hit (addr::mAddr) =
{
    cacheEntry = DL1(getIdx(addr));
    if (cacheEntry.valid and cacheEntry.tag == getTag(addr)) then
        Some (cacheEntry)
    else
        None
}

CacheEntry mkCacheEntry(valid::bool, tag::Tag, data::CacheLine) =
{
    var line;
    line.valid <- valid;
    line.tag   <- tag;
    line.data  <- data;
    line
}

unit updateDCache (pAddr::mAddr, data::dword, mask::dword) =
{
    match hit (pAddr)
    {
        case Some (cacheEntry) =>
        {
            cacheLine = cacheEntry.data;
            masked_data = idx(cacheLine, pAddr<1:0>) && ~mask || data && mask;
            cacheLine = upd(cacheLine, pAddr<1:0>, masked_data);
            DL1(getIdx(pAddr)) <- mkCacheEntry(true, getTag(pAddr), cacheLine)
        }
        case None => nothing
    }
}

CacheLine serveDCacheMiss (pAddr::mAddr) =
{
    var cacheLine = Nil;
    for i in 0 .. 3 do cacheLine <- cacheLine : list { MEM(pAddr<36:2> :[i]) };
    new_entry = mkCacheEntry(true, getTag(pAddr), cacheLine);
    old_entry = DL1(getIdx(pAddr));
    when old_entry.valid do mark_log (3, log_dcache_evict(getIdx(pAddr), old_entry, new_entry));
    DL1(getIdx(pAddr)) <- new_entry;
    cacheLine
}

unit serveDCacheWrite (pAddr::mAddr, data::dword, mask::dword) = 
{
    MEM(pAddr) <- MEM(pAddr) && ~mask || data && mask;
    for i in 0 .. totalCore-1 do
        when procID != [i] do
        {
            oldProcID = procID;
            procID <- [i];
            updateDCache (pAddr, data, mask);
            procID <- oldProcID
        }
}

-------------------------------------------------------------------------------
-- Memory API
-------------------------------------------------------------------------------

unit InitMEM =
{
    MEM <- InitMap (0x0);
    for i in 0 .. totalCore-1 do c_dl1([i]) <- InitMap(mkCacheEntry(false, 0x0, Nil))
}

dword ReadData (pAddr::mAddr) =
{
    var cacheLine;
    match hit (pAddr)
    {
        case Some (cacheEntry) =>
        {
            mark_log (3,log_dcache_read_hit(pAddr,getIdx(pAddr)));
            cacheLine<-cacheEntry.data
        }
        case None => cacheLine <- serveDCacheMiss(pAddr)
    };
    data = idx(cacheLine, pAddr<1:0>);
    mark_log (2, log_r_mem (pAddr, data));
    data
}

unit WriteData (pAddr::mAddr, data::dword, mask::dword) =
{
    updateDCache(pAddr, data, mask);
    serveDCacheWrite(pAddr, data, mask);
    mark_log (2, log_w_mem (pAddr, mask, data))
}

word ReadInst (a::pAddr) =
{
    if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>
}

-- sml helper function
unit WriteDWORD (pAddr::mAddr, data::dword) =
    MEM(pAddr) <- data

-- sml helper function
unit Write256 (pAddr::bits(35), data::bits(256)) =
{
    MEM(pAddr:'00') <- data<63:0>;
    MEM(pAddr:'01') <- data<127:64>;
    MEM(pAddr:'10') <- data<191:128>;
    MEM(pAddr:'11') <- data<255:192>
}
