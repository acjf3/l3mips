---------------------------------------------------------------------------
-- CHERI types for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-params.m4')dnl

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
    otypeHi  :: bits(10)
    baseBits :: bits(10)
    otypeLo  :: bits(10)
    topBits  :: bits(10)
}

construct SFields {Sealed :: SealedFields, Unsealed :: UnsealedFields}

construct RepRegion {Low :: bits(20), Hi :: bits(20)}
string repRegionStr (r::RepRegion) = match r
{
    case Low(b) => "Low(":[b]:")"
    case Hi(b)  => "Hi(":[b]:")"
}

type OType = bits(20)

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
changequote(!,!)dnl
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
changequote(`,')dnl

bits(66) getBound (cap::Capability, ptr::RepRegion, bound::RepRegion) =
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
    (getBound(cap, pr, tr))<63:0>
}

nat innerZeroCount (data::bool list, acc::nat) = match data
{
    case Nil => acc
    case Cons(hd, tl) => if hd then acc else innerZeroCount (tl, acc + 1)
}

nat countLeadingZeros  (data::bits(64)) = innerZeroCount ([data], 0)
nat countTrailingZeros (data::bits(20)) = innerZeroCount ([Reverse(data)], 0)

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
    b = (getBound(cap, pr, br));
    return b<63:0>
}

bits(64) getOffset (cap::Capability) = cap.cursor - getBase(cap)

bits(65) getFullLength (cap::Capability) =
{
    pr, tr, br = getRepRegions(cap);
    b = getBound (cap, pr, br);
    t = getBound (cap, pr, tr);
    len = t - b;
    return len<64:0>
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

define(`BUFFSIZE', `4096')dnl
Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    match cap.sFields
    {
        case Sealed(_)    => new_cap <- UNKNOWN
        case Unsealed(uf) =>
        {
            -- set length (pick best representation)
            zeros = countLeadingZeros (length);
            var new_exp = Max(44-zeros, 0);
            var rep_len::bits(65) = 1 << (new_exp + 20);
            when not rep_len > ZeroExtend(length+2*BUFFSIZE) do
                new_exp <- new_exp + 1;
            new_base = cap.cursor;
            new_top::bits(65) = ZeroExtend(cap.cursor) + ZeroExtend(length);
            new_baseBits = new_base<new_exp+19:new_exp>;
            var new_topBits = new_top<new_exp+19:new_exp>;
            when (new_top && ~(~0 << new_exp)) <> 0 do new_topBits  <- new_topBits + 1; -- XXX what if new_topBits where all Fs at the begining of the operation ?
            new_cap.exp <- [new_exp];
            var uf :: UnsealedFields;
            uf.baseBits <- new_baseBits;
            uf.topBits  <- new_topBits;
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
            -- construct the new base and top bits upper 10 bits
            e::nat = [cap.exp];
            cb::bits(10) = cap.cursor<19+e:10+e>;
            -- assemble the new unsealed fields
            var uf::UnsealedFields;
            uf.baseBits <- cb:sf.baseBits;
            uf.topBits  <- cb:sf.topBits;
            new_cap.sFields <- Unsealed(uf)
        }
        case Unsealed(uf) => when sealed do
        {
            -- count trailing zeroes
            baseZeros  = countTrailingZeros(uf.baseBits);
            topZeros   = countTrailingZeros(uf.topBits);
            trailZeros = Min(10,Min(baseZeros, topZeros));
            -- assemble the new sealed fileds
            var sf::SealedFields;
            sf.baseBits <- [uf.baseBits >> trailZeros];
            sf.otypeHi  <- 0;
            sf.topBits  <- [uf.topBits >> trailZeros];
            sf.otypeLo  <- 0;
            new_cap.sFields <- Sealed (sf);
            new_cap.exp     <- cap.exp + [trailZeros]
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
        new_sf.otypeHi  <- otype<19:10>;
        new_sf.otypeLo  <- otype<9:0>;
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
        sf.otypeHi  <- raw<39:30>;
        sf.baseBits <- raw<29:20>;
        sf.otypeLo  <- raw<19:10>;
        sf.topBits  <- raw<9:0>;
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
string hex10 (x::bits(10)) = ToLower (PadLeft (#"0", 3, [x]))
string hex16 (x::bits(16)) = ToLower (PadLeft (#"0", 4, [x]))
string hex20 (x::bits(20)) = ToLower (PadLeft (#"0", 5, [x]))
string hex23 (x::bits(23)) = ToLower (PadLeft (#"0", 6, [x]))
string dec6  (x::bits(6))  = ToLower (PadLeft (#" ", 2, [[x]::nat]))
string cap_inner_rep (cap::Capability) =
    "v:":(if cap.tag then "1" else "0"):
    " perms:0x":hex16(ZeroExtend(cap.&perms)):
    " exp:":dec6(cap.exp):
    match cap.sFields
    {
        case Sealed(sf)   =>
            " sealed:1 baseBits:0x":hex10(sf.baseBits):" topBits:0x":hex10(sf.topBits):" otype:0x":hex20(sf.otypeHi:sf.otypeLo):"(hi:0x":hex10(sf.otypeHi):", lo:0x":hex10(sf.otypeLo)
        case Unsealed(uf) =>
            " sealed:0 baseBits:0x":hex20(uf.baseBits):" topBits:0x":hex20(uf.topBits)
    }:
    " cursor:0x":hex64(cap.cursor)
string log_cap_write (cap::Capability) =
    "s:":(if getSealed(cap) then "1" else "0"):
    " perms:0x":hex16(ZeroExtend(&getPerms(cap))):
    " type:0x":hex20(getType(cap)):
    " offset:0x":hex64(getOffset(cap)):
    " base:0x":hex64(getBase(cap)):
    " length:0x":hex64(getLength(cap)):"\\n(":cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
