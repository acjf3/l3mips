---------------------------------------------------------------------------
-- CHERI types for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

record UnsealedFields
{
    baseBits :: bits(20)
    topBits  :: bits(20)
}

record SealedFields
{
    otypeHi  :: bits(12)
    baseBits :: bits(8)
    otypeLo  :: bits(12)
    topBits  :: bits(8)
}

construct SFields {Sealed :: SealedFields, Unsealed :: UnsealedFields}

construct RepRegion {Low :: bits(20), Hi :: bits(20)}
string repRegionStr (r::RepRegion) = match r
{
    case Low(b) => "Low(":[b]:")"
    case Hi(b)  => "Hi(":[b]:")"
}

type OType = bits(OTYPEWIDTH)

record Capability
{
    tag      :: bool
    uperms   :: UPerms
    perms    :: Perms
    reserved :: bits(2)
    exp      :: nat
    sFields  :: SFields
    cursor   :: bits(64)
}

---------------------------------
-- capability helper functions --
--------------------------------------------------------------------------------

bits(6) encExp (e::nat) =
{
    ebits::bits(6) = [e];
    ~ebits<5:4> : ebits<3:0>
}
nat decExp (e::bits(6)) = [~e<5:4> : e<3:0>]

bits(4) encUPerms (up::UPerms) = &up<3:0>
UPerms decUPerms (up::bits(4)) = UPerms(ZeroExtend(up))

bits(11) encPerms (p::Perms) = &p<10:0>
Perms decPerms (p::bits(11)) = Perms(SignExtend(p))

{-
RepRegion * RepRegion * RepRegion getRepRegions (cap::Capability) =
{
    tb, bb = match cap.sFields
    {
        case Unsealed(uf) => uf.topBits, uf.baseBits
        case Sealed(sf)   => (0`10):sf.topBits, (0`10):sf.baseBits
    };
    ptr = cap.cursor<cap.exp+19:cap.exp>;
    var repBound::bits(21) = (('0':tb)+('0':bb))>>+1;
    when tb >+ bb do repBound <- repBound + (1 << 19);
    pr = if ptr <+ [repBound] then Hi (ptr) else Low (ptr);
    tr = if tb  <+ [repBound] then Hi (tb)  else Low (tb);
    br = if bb  <+ [repBound] then Hi (bb)  else Low (bb);
    (pr, tr, br)
}
-}
RepRegion * RepRegion * RepRegion getRepRegions (cap::Capability) =
{
    tb, bb = match cap.sFields
    {
        case Unsealed(uf) => uf.topBits, uf.baseBits
        case Sealed(sf)   => sf.topBits:(0`12), sf.baseBits:(0`12)
    };
    e = cap.exp;
    ptr = cap.cursor<e+19:e>;
    var repBound::bits(20) = bb - 0x1000;
    pr = if ptr <+ repBound then Hi (ptr) else Low (ptr);
    tr = if tb  <+ repBound then Hi (tb)  else Low (tb);
    br = if bb  <+ repBound then Hi (bb)  else Low (bb);
    (pr, tr, br)
}

nat getBound (cap::Capability, ptr::RepRegion, bound::RepRegion) =
{
    e = cap.exp;                    -- exponent
    s = 2**(e+20);                  -- region size
    c::nat = [cap.cursor];          -- cursor
    cAlign::nat = c - (c mod s);    -- aligned cursor
    match (ptr, bound)
    {
        -- same region as cursor => no correction
        case Low(p), Low(b) => [cAlign + [b]*2**e]
        case Hi(p) , Hi(b)  => [cAlign + [b]*2**e]
        -- region above cursor => add a region size
        case Low(p), Hi(b)  => [cAlign + [b]*2**e + s]
        -- region below cursor => take away a region size
        case Hi(p) , Low(b) => [cAlign + [b]*2**e - s]
    }
}

bits(64) getTop (cap::Capability) =
{
    pr, tr, br = getRepRegions(cap);
    [getBound(cap, pr, tr)]
}

nat innerZeroCount (data::bool list, acc::nat) = match data
{
    case Nil => acc
    case Cons(hd, tl) => if hd then acc else innerZeroCount (tl, acc + 1)
}

nat countLeadingZeros  (data::bits(64)) = innerZeroCount ([data], 0)
nat countTrailingZeros (data::bits(20)) = innerZeroCount ([Reverse(data)], 0)

nat idxMSNZ (data::bits(64)) = 63-countLeadingZeros(data)

---------------------------------------
-- standard capabilities definitions --
--------------------------------------------------------------------------------

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- true;
    new_cap.uperms   <- UPerms(~0);
    new_cap.perms    <- Perms(~0);
    new_cap.reserved <- 0;
    new_cap.exp      <- 45;
    var uf :: UnsealedFields;
    uf.baseBits <- 0;
    uf.topBits  <- 0x80000;
    new_cap.sFields  <- Unsealed(uf);
    new_cap.cursor   <- 0;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.uperms   <- UPerms(0);
    new_cap.perms    <- Perms(0);
    new_cap.reserved <- 0;
    new_cap.exp      <- 48; -- this exponent maps to a 0 representation
    var uf :: UnsealedFields;
    uf.baseBits <- 0;
    uf.topBits  <- 0;
    new_cap.sFields  <- Unsealed(uf);
    new_cap.cursor   <- 0;
    new_cap
}

------------------------------------
-- capability "typeclass" getters --
--------------------------------------------------------------------------------

bool  getTag (cap::Capability) = cap.tag

OType  getType (cap::Capability) = match cap.sFields
{
    case Sealed(sf) => sf.otypeHi:sf.otypeLo
    case _          => 0
}

UPerms getUPerms (cap::Capability) = cap.uperms

Perms getPerms (cap::Capability) = cap.perms

bool getSealed (cap::Capability) = match cap.sFields
{
    case Sealed(sf) => true
    case _          => false
}

bits(64) getBase (cap::Capability) =
{
    pr, tr, br = getRepRegions(cap);
    [getBound(cap, pr, br)]
}

bits(64) getOffset (cap::Capability) = cap.cursor - getBase(cap)

bits(65) getFullLength (cap::Capability) =
{
    pr, tr, br = getRepRegions(cap);
    b = getBound (cap, pr, br);
    t = getBound (cap, pr, tr);
    len = t - b;
    [len]
}

bits(64) getLength (cap::Capability) =
{
    len = getFullLength(cap);
    if len<64> then ~0 else len<63:0>
}

------------------------------------
-- capability "typeclass" setters --
--------------------------------------------------------------------------------

Capability setOffset (cap::Capability, offset::bits(64)) =
{
    var new_cap = cap;
    new_cap.cursor <- getBase(cap) + offset;
    new_cap
}

Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    match cap.sFields
    {
        case Sealed(_)    => new_cap <- UNKNOWN
        case Unsealed(uf) =>
        {
            -- aranges for a minimun 2 pages (2*4K) out of bounds buffer to be present
            inflated_length::bits(65) = ZeroExtend(length) + (ZeroExtend(length) >> 6);
            -- deriving e from the inflated length
            --e = [Log2(inflated_length >>+ 19)]; XXX Don't know why this doesn't work
            var e = idxMSNZ([inflated_length >>+ 19]);
            when e mod 4 <> 0 do e <- e + (4 - (e mod 4));
            -- deriving the new base
            newBase = cap.cursor;
            newBaseBits = newBase<e+19:e>; -- no need to round down explicitly
            -- deriving the new top
            newTop::bits(65) = ZeroExtend(cap.cursor) + ZeroExtend(length);
            var newTopBits = newTop<e+19:e>;
            when (newTop && ~(~0 << e)) <> 0 do newTopBits <- newTopBits + 1; -- round up if significant bits are lost
            -- fold the derived values back in new_cap
            new_cap.exp <- e;
            var uf :: UnsealedFields;
            uf.baseBits <- newBaseBits;
            uf.topBits  <- newTopBits;
            new_cap.sFields <- Unsealed(uf)
        }
    };
    new_cap
}
Capability setTag (cap::Capability, tag::bool) =
{ var new_cap = cap; new_cap.tag <- tag; new_cap }

Capability setUPerms (cap::Capability, uperms::UPerms) =
{
    var new_cap = cap;
    new_cap.uperms <- uperms;
    new_cap
}

Capability setPerms (cap::Capability, perms::Perms) =
{
    var new_cap = cap;
    new_cap.perms <- perms;
    new_cap
}

Capability setSealed (cap::Capability, sealed::bool) =
{
    var new_cap = cap;
    match cap.sFields
    {
        case Sealed(sf) => when not sealed do
        {
            -- construct the new base and top bits upper 12 bits
            e = cap.exp;
            lowbits::bits(12) = cap.cursor<11+e:e>;
            -- assemble the new unsealed fields
            var uf::UnsealedFields;
            uf.baseBits <- sf.baseBits:lowbits;
            uf.topBits  <- sf.topBits:lowbits;
            new_cap.sFields <- Unsealed(uf)
        }
        case Unsealed(uf) => when sealed do
        {
            var sf::SealedFields;
            sf.baseBits <- uf.baseBits<19:12>;
            sf.otypeHi  <- 0;
            sf.topBits  <- uf.topBits<19:12>;
            sf.otypeLo  <- 0;
            new_cap.sFields <- Sealed (sf)
        }
    };
    new_cap
}

Capability setType (cap::Capability, otype::OType) = match cap.sFields
{
    case Sealed(sf) =>
    {
        var new_cap = cap;
        var new_sf  = sf;
        new_sf.otypeHi  <- otype<23:12>;
        new_sf.otypeLo  <- otype<11:0>;
        new_cap.sFields <- Sealed(new_sf);
        new_cap
    }
    case _ => cap
}

--------------------------------------
-- capability "typeclass" functions --
--------------------------------------------------------------------------------

bool isCapAligned (addr::bits(64)) = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) = match cap.sFields
{
    case Sealed(sf)   => cap.cursor:encUPerms(cap.uperms):encPerms(cap.perms):cap.reserved:encExp(cap.exp):'1':sf.otypeHi:sf.baseBits:sf.otypeLo:sf.topBits
    case Unsealed(uf) => cap.cursor:encUPerms(cap.uperms):encPerms(cap.perms):cap.reserved:encExp(cap.exp):'0':uf.baseBits:uf.topBits
}

Capability bitsToCap (raw :: CAPRAWBITS) =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.uperms   <- decUPerms(raw<63:60>);
    new_cap.perms    <- decPerms(raw<59:49>);
    new_cap.reserved <- raw<48:47>;
    new_cap.exp      <- decExp(raw<46:41>);
    var f;
    if raw<40> then
    {
        var sf :: SealedFields;
        sf.otypeHi  <- raw<39:28>;
        sf.baseBits <- raw<27:20>;
        sf.otypeLo  <- raw<19:8>;
        sf.topBits  <- raw<7:0>;
        f <- Sealed(sf)
    }
    else
    {
        var uf :: UnsealedFields;
        uf.baseBits <- raw<39:20>;
        uf.topBits  <- raw<19:0>;
        f <- Unsealed(uf)
    };
    new_cap.sFields  <- f;
    new_cap.cursor   <- raw<127:64>;
    new_cap
}

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
if dwordAddr<0> then raw<127:64> else raw<63:0>

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
if dwordAddr<0> then
    (old_blob<127:64> && ~mask || data && mask) : old_blob<63:0>
else
    old_blob<127:64> : (old_blob<63:0> && ~mask || data && mask)

---------------
-- log utils --
--------------------------------------------------------------------------------
string cap_inner_rep (cap::Capability) =
    "v:":(if cap.tag then "1" else "0"):
    " uperms:":hex(cap.&uperms<3:0>):
    " perms:":hex(cap.&perms<10:0>):
    " exp:":[cap.exp]:
    match cap.sFields
    {
        case Sealed(sf)   =>
            " sealed:1 baseBits:":hex (sf.baseBits):" topBits:":hex (sf.topBits):" otype:":hex (sf.otypeHi:sf.otypeLo):"(hi:":hex (sf.otypeHi):", lo:":hex (sf.otypeLo)
        case Unsealed(uf) =>
            " sealed:0 baseBits:":hex (uf.baseBits):" topBits:":hex (uf.topBits)
    }:
    " cursor:":hex (cap.cursor)
string log_cap_write (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " s:":(if getSealed(cap) then "1" else "0"):
    " perms:":hex (cap.&uperms<3:0> : SignExtend(cap.&perms<10:0>)`15): -- TODO report 2 architectural fields
    " type:":hex (getType(cap)):
    " offset:":hex (getOffset(cap)):
    " base:":hex (getBase(cap)):
    " length:":hex (getLength(cap)):"\\n(":cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[":hex (pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[":hex (pAddr):"]"
