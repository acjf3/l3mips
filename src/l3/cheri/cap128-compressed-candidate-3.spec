---------------------------------------------------------------------------
-- CHERI types for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

register Perms :: bits (15)
{
    14-11 : soft
       10 : Access_System_Registers
      9-8 : Reserved
        7 : Permit_Seal
        6 : Permit_Store_Local_Capability
        5 : Permit_Store_Capability
        4 : Permit_Load_Capability
        3 : Permit_Store
        2 : Permit_Load
        1 : Permit_Execute
        0 : Global
}

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
    perms    :: Perms
    reserved :: bits(2)
    exp      :: bits(6)
    sFields  :: SFields
    cursor   :: bits(64)
}

---------------------------------
-- capability helper functions --
--------------------------------------------------------------------------------
{-
RepRegion * RepRegion * RepRegion getRepRegions (cap::Capability) =
{
    tb, bb = match cap.sFields
    {
        case Unsealed(uf) => uf.topBits, uf.baseBits
        case Sealed(sf)   => (0`10):sf.topBits, (0`10):sf.baseBits
    };
    ptr = cap.cursor<[cap.exp]+19:[cap.exp]>;
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
    ptr = cap.cursor<[cap.exp]+19:[cap.exp]>;
    var repBound::bits(20) = bb - 0x1000;
    pr = if ptr <+ repBound then Hi (ptr) else Low (ptr);
    tr = if tb  <+ repBound then Hi (tb)  else Low (tb);
    br = if bb  <+ repBound then Hi (bb)  else Low (bb);
    (pr, tr, br)
}

nat getBound (cap::Capability, ptr::RepRegion, bound::RepRegion) =
{
    e::nat = [cap.exp];             -- exponent
    s::nat = 2**(e+20);             -- region size
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
    new_cap.perms    <- Perms(~0);
    new_cap.reserved <- 0;
    new_cap.exp      <- 0x2D; -- 45
    var uf :: UnsealedFields;
    uf.baseBits <- 0;
    uf.topBits  <- 0x80000;
    new_cap.sFields  <- Unsealed(uf);
    new_cap.cursor   <- 0;
    new_cap
}

Capability nullCap = -- FIXME
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.perms    <- Perms(0);
    new_cap.reserved <- 0;
    new_cap.exp      <- 0;
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
            new_cap.exp <- [e];
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

Capability setPerms (cap::Capability, perms::Perms) =
{ var new_cap = cap; new_cap.perms <- perms; new_cap }

Capability setSealed (cap::Capability, sealed::bool) =
{
    var new_cap = cap;
    match cap.sFields
    {
        case Sealed(sf) => when not sealed do
        {
            -- construct the new base and top bits upper 12 bits
            e::nat = [cap.exp];
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

bool allow_system_reg_access(p::Perms, r::reg) =
((r == 31 or r == 30 or r == 29 or r == 27 or r == 28) and not p.Access_System_Registers)

bool isCapAligned (addr::bits(64)) = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) = match cap.sFields
{
    case Sealed(sf)   => cap.cursor:cap.&perms:'00':cap.exp:'1':sf.otypeHi:sf.baseBits:sf.otypeLo:sf.topBits
    case Unsealed(uf) => cap.cursor:cap.&perms:'00':cap.exp:'0':uf.baseBits:uf.topBits
}

Capability bitsToCap (raw :: CAPRAWBITS) =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.perms    <- Perms(raw<63:49>);
    new_cap.reserved <- 0;
    new_cap.exp      <- raw<46:41>;
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

bool isCapRepresentable(sealed::bool,
                        base::bits(64),
                        length::bits(64),
                        offset::bits(64)) =
{
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base);
    test_cap <- setBounds(test_cap, length);
    test_cap <- setOffset(test_cap, offset);
    test_cap <- setSealed(test_cap, sealed);
    if getBase(test_cap)   == base   and
       getLength(test_cap) == length and
       getOffset(test_cap) == offset then
       true else false
}

---------------
-- log utils --
--------------------------------------------------------------------------------
string hex8 (x::bits(8)) = ToLower (PadLeft (#"0", 2, [x]))
string hex10 (x::bits(10)) = ToLower (PadLeft (#"0", 3, [x]))
string hex12 (x::bits(12)) = ToLower (PadLeft (#"0", 3, [x]))
string hex16 (x::bits(16)) = ToLower (PadLeft (#"0", 4, [x]))
string hex20 (x::bits(20)) = ToLower (PadLeft (#"0", 5, [x]))
string hex23 (x::bits(23)) = ToLower (PadLeft (#"0", 6, [x]))
string hex24 (x::bits(24)) = ToLower (PadLeft (#"0", 6, [x]))
string dec6  (x::bits(6))  = ToLower (PadLeft (#" ", 2, [[x]::nat]))
string cap_inner_rep (cap::Capability) =
    "v:":(if cap.tag then "1" else "0"):
    " perms:0x":hex16(ZeroExtend(cap.&perms)):
    " exp:":dec6(cap.exp):
    match cap.sFields
    {
        case Sealed(sf)   =>
            " sealed:1 baseBits:0x":hex8(sf.baseBits):" topBits:0x":hex8(sf.topBits):" otype:0x":hex24(sf.otypeHi:sf.otypeLo):"(hi:0x":hex12(sf.otypeHi):", lo:0x":hex12(sf.otypeLo)
        case Unsealed(uf) =>
            " sealed:0 baseBits:0x":hex20(uf.baseBits):" topBits:0x":hex20(uf.topBits)
    }:
    " cursor:0x":hex64(cap.cursor)
string log_cap_write (cap::Capability) =
    --"v:":(if getTag(cap) then "1" else "0"):
    " s:":(if getSealed(cap) then "1" else "0"):
    " perms:0x":hex16(ZeroExtend(&getPerms(cap))):
    " type:0x":hex24(getType(cap)):
    " offset:0x":hex64(getOffset(cap)):
    " base:0x":hex64(getBase(cap)):
    " length:0x":hex64(getLength(cap)):"\\n(":cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
