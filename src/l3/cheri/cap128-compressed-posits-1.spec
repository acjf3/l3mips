---------------------------------------------------------------------------
-- CHERI types for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

record UnsealedFields
{
    bitsTopBase :: bits(40)
}

record SealedFields
{
    otype       :: bits(24)
    bitsTopBase :: bits(16)
}

construct SFields {Sealed :: SealedFields, Unsealed :: UnsealedFields}

-- NB: bit negation of positions 15 and 14 are for 0 in mem representation of the null cap
SFields decSFields (raw::bits(41)) = if raw<40> then
{
    -- sealed
    var sf :: SealedFields;
    sf.otype <- raw<39:16>;
    sf.bitsTopBase <- ~raw<15:14> : raw<13:0>;
    Sealed(sf)
} else {
    -- unsealed
    var uf :: UnsealedFields;
    uf.bitsTopBase <- raw<39:16> : ~raw<15:14> : raw<13:0>;
    Unsealed(uf)
}
bits(41) encSFields (sFields::SFields) = match sFields
{
    case Unsealed(uf) => '0' : uf.bitsTopBase<39:16> : ~uf.bitsTopBase<15:14> : uf.bitsTopBase<13:0>
    case Sealed(sf)   => '1' : sf.otype : ~sf.bitsTopBase<15:14> : sf.bitsTopBase<13:0>
}

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
    reserved :: bits(6)
    e0       :: bool
    r0       :: bool
    sFields  :: SFields
    cursor   :: bits(64)
}
{--
    e0 will always be bit 0 of exp
    r0 is bit 0 of the regime
    - if r0 = 1, do not continue, Top and Base of 20 bits
    - if r0 = 0, continue into Top and Base,
      use a single continuation token scheme:
        - '00' is the continuation token
        - {'01','10','11'} are termination tokens
--}

{--
string printBitList (lst::bits(n) list) =
{
    var str = "{ ";
    foreach e in lst do str <- str : hex(e) : ", ";
    (str : "}")
}
--}

-- token size = 2 bits
bits(2) list tokenize (f::bits(n)) =
{
    -- TODO check for n divisible by token size (2 bits)
    var tok_list = Nil;
    for i in 0 .. ((n div 2)-1) do
    {
        tok_list <- Cons (f<(2*i)+1:2*i>, tok_list)
    };
    tok_list
}

bits(2) list getTokens (cap::Capability) = match cap.sFields
{
    case Unsealed(uf) => tokenize(uf.bitsTopBase)
    case Sealed(sf)   => tokenize(sf.bitsTopBase)
}

---------------------------------
-- capability helper functions --
--------------------------------------------------------------------------------

bits(4) encUPerms (up::UPerms) = &up<3:0>
UPerms decUPerms (up::bits(4)) = UPerms(ZeroExtend(up))

bits(11) encPerms (p::Perms) = &p<10:0>
Perms decPerms (p::bits(11)) = Perms(SignExtend(p))

nat getExp (cap::Capability) = if cap.r0 then [cap.e0] else
{
    var term = None;
    var nb_cont::nat = 0;
    -- emulates "while not terminated" loop
    foreach t in Reverse(getTokens(cap)) do when not IsSome(term) do match t
    {
        -- continuation
        case '00' => nb_cont <- nb_cont + 1
        -- termination
        case '01' => term <- Some (0)
        case '10' => term <- Some (2)
        case '11' => term <- Some (4)
    };
    start::nat  = (nb_cont * 6) + 2;
    offset::nat = if IsSome(term) then ValOf(term) else 0;
    (start + offset + [cap.e0])
}

bits(20) * bits(20) getBoundBits (cap::Capability) =
{
    var toks  = Nil;
    var latch = false;
    -- emulates "while not done" loop
    -- builds up list of "useful" tokens
    foreach t in Reverse(getTokens(cap)) do match t
    {
        -- found continuation token from the other side
        case '00' => when latch do
                        toks <- Cons ('00', toks)
        case x    => if latch then
                        toks <- Cons (x, toks)
                     else latch <- true
    };
    -- buid Top and base fields
    var top::bits(20)  = 0;
    var base::bits(20) = 0;
    var i = 19;
    foreach tok in toks do match [tok]
    {
        case t@b@Nil => {
            top<i>  <- t;
            base<i> <- b;
            i <- i - 1
        }
    };
    (top,base)
}
bits(20) getTopBits  (cap::Capability) = Fst(getBoundBits(cap))
bits(20) getBaseBits (cap::Capability) = Snd(getBoundBits(cap))

bool * bool * SFields encPosit(exp::nat,top::bits(20),base::bits(20),otype::bits(24) option) =
{
    e0 = exp mod 2; -- e0 is the parity of the exponent
    r0 = not (exp > 1); -- r0 need continuation for exp > 1, and continuation is encoded as 0s
    nb_cont = (exp - e0 - 2) div 6; -- how many continuation tokens required
    offset  = exp - nb_cont * 6 - e0 - 2; -- offset within termination token
    ttok = match offset
    {
        case 0 => '01'
        case 2 => '10'
        case 4 => '11'
    };
    var sFields;
    match otype
    {
        case None => {
            var bitsTopBase;
            for i in 0 .. 19 do {
                bitsTopBase<39-(2*i)>   <- top<19-i>;
                bitsTopBase<39-(2*i)-1> <- base<19-i>
            };
            for i in 0 .. (nb_cont - 1) do
                bitsTopBase<(2*i)+1:2*i> <- '00';
            bitsTopBase<(2*nb_cont)+1:2*nb_cont> <- ttok;
            var uf :: UnsealedFields;
            uf.bitsTopBase <- bitsTopBase;
            sFields <- Unsealed(uf)
        }
        case Some(t) => {
            var bitsTopBase;
            for i in 0 .. 7 do {
                bitsTopBase<15-(2*i)>   <- top<19-i>;
                bitsTopBase<15-(2*i)-1> <- base<19-i>
            };
            for i in 0 .. (nb_cont - 1) do
                bitsTopBase<(2*i)+1:2*i> <- '00';
            bitsTopBase<(2*nb_cont)+1:2*nb_cont> <- ttok;
            var sf :: SealedFields;
            sf.bitsTopBase <- bitsTopBase;
            sf.otype <- t;
            sFields <- Sealed(sf)
        }
    };
    ([e0],r0,sFields)
}

RepRegion * RepRegion * RepRegion getRepRegions (cap::Capability) =
{
    tb, bb = getBoundBits(cap);
    e = getExp(cap);
    ptr = cap.cursor<e+19:e>;
    var repBound::bits(20) = bb - 0x1000;
    pr = if ptr <+ repBound then Hi (ptr) else Low (ptr);
    tr = if tb  <+ repBound then Hi (tb)  else Low (tb);
    br = if bb  <+ repBound then Hi (bb)  else Low (bb);
    (pr, tr, br)
}

nat getBound (cap::Capability, ptr::RepRegion, bound::RepRegion) =
{
    e = getExp(cap);                -- exponent
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

------------------------------------
-- capability "typeclass" getters --
--------------------------------------------------------------------------------

bool  getTag (cap::Capability) = cap.tag

OType  getType (cap::Capability) = match cap.sFields
{
    case Sealed(sf) => sf.otype
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

bits(64) * bits(64) getBaseAndLength (cap::Capability) =
  return (getBase(cap), getLength(cap))

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
            e0, r0, sFields = encPosit(e,newTopBits,newBaseBits,None);
            new_cap.e0      <- e0;
            new_cap.r0      <- r0;
            new_cap.sFields <- sFields
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
            e = getExp(cap);
            t, b = getBoundBits(cap);
            lowbits::bits(12) = cap.cursor<11+e:e>;
            -- assemble the new unsealed fields
            e0, r0, sFields = encPosit(
                e,
                t<19:12> : lowbits,
                b<19:12> : lowbits,
                None
            );
            new_cap.e0      <- e0;
            new_cap.r0      <- r0;
            new_cap.sFields <- sFields
        }
        case Unsealed(uf) => when sealed do
        {
            t, b = getBoundBits(cap);
            e0, r0, sFields = encPosit(
                getExp(cap),
                t<19:12> : 0`12,
                b<19:12> : 0`12,
                Some(0)
            );
            new_cap.e0      <- e0;
            new_cap.r0      <- r0;
            new_cap.sFields <- sFields
        }
    };
    new_cap
}

Capability setType (cap::Capability, otype::OType) = match cap.sFields
{
    case Sealed(sf) =>
    {
        var new_cap = cap;
        t, b = getBoundBits(cap);
        e0, r0, sFields = encPosit(getExp(cap),t,b,Some(otype));
        new_cap.e0      <- e0;
        new_cap.r0      <- r0;
        new_cap.sFields <- sFields;
        new_cap
    }
    case _ => cap
}

--------------------------------------
-- capability "typeclass" functions --
--------------------------------------------------------------------------------

bool isCapAligned (addr::bits(64)) = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) = 
    cap.cursor:encUPerms(cap.uperms):encPerms(cap.perms):cap.reserved:[cap.e0]:[cap.r0]:encSFields(cap.sFields)

Capability bitsToCap (raw :: CAPRAWBITS) =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.uperms   <- decUPerms(raw<63:60>);
    new_cap.perms    <- decPerms(raw<59:49>);
    new_cap.reserved <- raw<48:43>;
    new_cap.e0       <- raw<42>;
    new_cap.r0       <- raw<41>;
    new_cap.sFields  <- decSFields(raw<40:0>);
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
{
    t, b = getBoundBits(cap);
    e = getExp(cap);
    "v:":(if cap.tag then "1" else "0"):
    " uperms:":hex(cap.&uperms<3:0>):
    " perms:":hex(cap.&perms<10:0>):
    " exp:":[e]:
    " e0:":if cap.e0 then "1" else "0":
    " r0:":if cap.r0 then "1" else "0":
    match cap.sFields
    {
        case Sealed(sf)   =>
            " sealed:1 topBits:":hex(t):" baseBits:":hex(b):" otype:":hex(sf.otype)
        case Unsealed(uf) =>
            " sealed:0 topBits:":hex(t):" baseBits:":hex(b)
    }:
    " cursor:":hex (cap.cursor)
}
string log_cap_write (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " s:":(if getSealed(cap) then "1" else "0"):
    " perms:":hex (cap.&uperms<3:0> : SignExtend(cap.&perms<10:0>)`15): -- TODO report 2 architectural fields
    " type:":hex (getType(cap)):
    " offset:":hex (getOffset(cap)):
    " base:":hex (getBase(cap)):
    " length:":hex (getLength(cap)):"\n(":cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[":hex (pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[":hex (pAddr):"]"

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
    e0, r0, sFields = encPosit(45,0x80000,0x0,None);
    new_cap.e0       <- e0;
    new_cap.r0       <- r0;
    new_cap.sFields  <- sFields;
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
    e0, r0, sFields = encPosit(48,0x0,0x0,None);
    new_cap.e0       <- e0;
    new_cap.r0       <- r0;
    new_cap.sFields  <- sFields;
    new_cap.cursor   <- 0;
    new_cap
}

bool isCapRepresentable(cap::Capability,
                        newSealed::bool,
                        newOffset::bits(64)) =
{
    base   = getBase(cap);
    length = getLength(cap);
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base);
    test_cap <- setBounds(test_cap, length);
    test_cap <- setOffset(test_cap, newOffset);
    test_cap <- setSealed(test_cap, newSealed);
    if getBase(test_cap)   == base   and
       getLength(test_cap) == length and
       getOffset(test_cap) == newOffset then
       true else false
}
