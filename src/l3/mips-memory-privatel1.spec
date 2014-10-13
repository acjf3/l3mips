---------------------------------------------------------------------------
-- MIPS default memory
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

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

dword idx(line::CacheLine, i::bits(2)) =
  if i == 0 then Head(line) else idx(Tail(line), i-1)

CacheLine upd(line::CacheLine, i::bits(2), dword::dword) =
  if i == 0 then Cons(dword, Tail(line))
            else Cons(Head(line),upd(Tail(line), i-1, dword))

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
    cacheEntry = DL1(pAddr<10:2>);
    when (cacheEntry.valid and cacheEntry.tag == pAddr<36:11>) do
    {
        cacheLine = cacheEntry.data;
        masked_data = idx(cacheLine, pAddr<1:0>) && ~mask || data && mask;
        cacheLine = upd(cacheLine, pAddr<1:0>, masked_data);
        DL1(pAddr<10:2>) <- mkCacheEntry(true, pAddr<36:11>, cacheLine)
    }
}

CacheLine serveDCacheMiss (pAddr::mAddr) =
{
    var cacheLine = Nil;
    for i in 0 .. 3 do cacheLine <- cacheLine : list { MEM(pAddr<36:2> :[i]) };
    DL1(pAddr<10:2>) <- mkCacheEntry(true, pAddr<36:11>, cacheLine);
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


-- memory API

unit InitMEM =
{
    MEM <- InitMap (0x0);
    for i in 0 .. totalCore-1 do c_dl1([i]) <- InitMap(mkCacheEntry(false, 0x0, Nil))
}

dword ReadData (pAddr::mAddr) =
{
    cacheEntry = DL1(pAddr<10:2>);
    cacheLine = if cacheEntry.valid and cacheEntry.tag == pAddr<36:11> then
                    cacheEntry.data
                else
                    serveDCacheMiss(pAddr);
    idx(cacheLine, pAddr<1:0>)
}

unit WriteData (pAddr::mAddr, data::dword, mask::dword) =
{
    updateDCache(pAddr, data, mask);
    serveDCacheWrite(pAddr, data, mask)
}

word ReadInst (a::pAddr) =
{
    if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>
}
