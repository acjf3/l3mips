---------------------------------------------------------------------------
-- CHERI Concentrate
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- types definitions
--------------------------------------------------------------------------------

type OType = bits(OTYPEWIDTH)
record Bounds
{
    otype    :: OType
    exp      :: nat
    len19    :: bits(1)
    topBits  :: bits(21)
    baseBits :: bits(21)
}

construct Format {
    Exp0,
    EmbeddedExp,
    Sealed
}

record Capability
{
    tag      :: bool     -- 1-bit validity tag
    uperms   :: UPerms   -- 4-bit user permissions
    perms    :: Perms    -- 11-bit permissions
    reserved :: bits(7)  -- 7-bit reserved
    format   :: Format   -- 42-bit Format+Bounds field
    bounds   :: Bounds   --
    address  :: bits(64) -- 64-bit address
}

-- helper functions
--------------------------------------------------------------------------------
construct RepRegion {Low :: bits(21), Hi :: bits(21)}

RepRegion * RepRegion * RepRegion getRepRegions (cap::Capability) =
{
    e   = cap.bounds.exp;
    tb  = cap.bounds.topBits;
    bb  = cap.bounds.baseBits;
    ptr = cap.address<e+20:e>;
    repBound`21 = (bb<20:18> - '001') : 0`18;
    pr  = if ptr <+ repBound then Hi (ptr) else Low (ptr);
    tr  = if tb  <+ repBound then Hi (tb)  else Low (tb);
    br  = if bb  <+ repBound then Hi (bb)  else Low (bb);
    (pr, tr, br)
}

inline bits(65) getBoundAux
   (ptr::RepRegion, bound::RepRegion, addrAlign::bits(65), e::nat, s::nat) =
    match (ptr, bound)
    {
        -- same region as ptr => no correction
        case Low(_), Low(b) => addrAlign + [b] << e
        case Hi(_) , Hi(b)  => addrAlign + [b] << e
        -- region above ptr => add a region size
        case Low(_), Hi(b)  => addrAlign + [b] << e + 1 << s
        -- region below ptr => take away a region size
        case Hi(_), Low(b)  => addrAlign + [b] << e - 1 << s
    }

bits(65) * bits(65) getBounds
  (cap::Capability, ptr::RepRegion, bound1::RepRegion, bound2::RepRegion) =
{
    e = cap.bounds.exp;          -- exponent
    s = e+21;                    -- region size
    addr`65 = [cap.address];     -- address
    addrAlign = addr >>+ s << s; -- aligned address
    return (getBoundAux (ptr, bound1, addrAlign, e, s),
            getBoundAux (ptr, bound2, addrAlign, e, s))
}

bits(64) * bits(64) * bits(64) getBaseTopLength (cap::Capability) =
{
    t, b = getBounds (cap, getRepRegions(cap));
    len = t - b;
    return ([b], [t], if len<64> then ~0 else [len])
}

-- standard capabilities definitions
--------------------------------------------------------------------------------

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- true;
    new_cap.uperms   <- UPerms(~0);
    new_cap.perms    <- Perms(0x7FF`32);
    new_cap.reserved <- 0;
    new_cap.format   <- EmbeddedExp;
    new_cap.bounds.exp      <- 44;
    new_cap.bounds.otype    <- 0;
    new_cap.bounds.len19    <- 0;
    new_cap.bounds.topBits  <- 0x100000;
    new_cap.bounds.baseBits <- 0;
    new_cap.address <- 0;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.uperms   <- UPerms(0);
    new_cap.perms    <- Perms(0);
    new_cap.reserved <- 0;
    new_cap.format   <- EmbeddedExp;
    new_cap.bounds.exp      <- 44;  -- need to map exponent 44 to 0 mem representation
    new_cap.bounds.otype    <- 0;
    new_cap.bounds.len19    <- 0;
    new_cap.bounds.topBits  <- 0x100000;
    new_cap.bounds.baseBits <- 0;
    new_cap.address <- 0;
    new_cap
}

-- Interface
--------------------------------------------------------------------------------

bool getTag (cap::Capability) = cap.tag

OType getType (cap::Capability) = match cap.format
{
    case Sealed => cap.bounds.otype
    case _      => 0
}

UPerms getUPerms (cap::Capability) = cap.uperms

Perms getPerms (cap::Capability) = Perms(SignExtend(cap.&perms<10:0>))

bool getSealed (cap::Capability) = match cap.format
{
    case Sealed => true
    case _      => false
}

bits(64) getBase (cap::Capability)   = {b,_,_ = getBaseTopLength(cap); b}

bits(64) getTop (cap::Capability)    = {_,t,_ = getBaseTopLength(cap); t}

bits(64) getLength (cap::Capability) = {_,_,l = getBaseTopLength(cap); l}

bits(64) * bits(64) getBaseAndLength (cap::Capability) =
{b,_,l = getBaseTopLength(cap); (b,l)}

bits(64) getOffset (cap::Capability) = cap.address - getBase(cap)

Capability setOffset (cap::Capability, offset::bits(64)) =
{
    var new_cap = cap;
    new_cap.address <- getBase(cap) + offset;
    new_cap
}

nat idxMSNZ (data::bits(64)) = if data == 0 then 0 else [Log2 (data)]
Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    -- deriving new exponent
    var e = idxMSNZ([length >>+ 20]);
    -- deriving the new base
    newBaseBits = cap.address<e+20:e>;
    -- deriving the new top
    newTop = ('0':cap.address) + ('0':length);
    var newTopBits = newTop<e+20:e>;
    when (newTop && ~(~0 << e)) <> 0 do newTopBits <- newTopBits + 1; -- round up if significant bits are lost
    -- fold the derived values back in new_cap
    new_cap.bounds.exp      <- e;
    new_cap.bounds.otype    <- 0;
    new_cap.bounds.len19    <- [length<19>];
    new_cap.bounds.topBits  <- newTopBits;
    new_cap.bounds.baseBits <- newBaseBits;
    new_cap.format <- if e == 0 then Exp0 else EmbeddedExp;
    -- overwrite new_cap if necessary
    new_cap <- match cap.format
    {
        case Exp0        => if e == 0 then new_cap else UNKNOWN(next_unknown("capability"))
        case EmbeddedExp => new_cap
        case Sealed      => UNKNOWN(next_unknown("capability"))
    };
    new_cap
}

Capability setTag (cap::Capability, tag::bool) =
{
    var new_cap = cap;
    new_cap.tag <- tag;
    new_cap
}

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
    match cap.format
    {
        case Sealed => when not sealed do
        {
            e = cap.bounds.exp;
            lowbits`12 = cap.address<11+e:e>;
            new_cap.format <- if e == 0 then Exp0 else EmbeddedExp;
            new_cap.bounds.topBits<11:0>  <- lowbits;
            new_cap.bounds.baseBits<11:0> <- lowbits
        }
        case _ => when sealed do
        {
            new_cap.format <- Sealed;
            new_cap.bounds.topBits<11:0>  <- 0;
            new_cap.bounds.baseBits<11:0> <- 0;
            new_cap.bounds.otype <- 0
        }
    };
    new_cap
}

Capability setType (cap::Capability, otype::OType) = match cap.format
{
    case Sealed =>
    {
        var new_cap = cap;
        new_cap.bounds.otype <- otype;
        new_cap
    }
    case _ => cap
}

bool isCapAligned (addr::bits(64)) = addr<3:0> == 0

-- representability tests
--------------------------------------------------------------------------------

bool canRepCap( cap::Capability,
                newSealed::bool,
                newOffset::bits(64)) =
{
    sealOk = if newSealed then cap.bounds.topBits<11:0> == 0 and cap.bounds.baseBits<11:0> == 0 else true;
    e::nat         = cap.bounds.exp;              -- exponent
    i::bits(64)    = newOffset - getOffset(cap);  -- increment
    imid::bits(21) = i<e+20:e>;                   -- increment's mid bits (21 bits)
    addr::bits(21) = cap.address<e+20:e>;         -- addr field mid bits (21 bits)
    edge::bits(21) = (cap.bounds.baseBits<20:18> - '001') : 0`18; -- 1/8th of the representable space below the base
    mask::bits(64) = ~[2**(e+21) - 1];
    inRange = if i && mask == mask or i && mask == 0 then true else false;
    inLimits = if i >= 0 then imid <+ (edge - addr - 1)
               else imid >=+ (edge - addr) and edge != addr;
    return ((inRange and inLimits) or e >= 44) and sealOk
}
bool canRepOffset(cap::Capability, newOffset::bits(64)) =
    canRepCap(cap,getSealed(cap),newOffset)
bool canRepSeal(cap::Capability, newSeal::bool) =
    canRepCap(cap,newSeal,getOffset(cap))
bool canRepBounds(cap::Capability, newLength::bits(64)) =
{
    base = getBase(cap);
    offset = getOffset(cap);
    sealed = getSealed(cap);
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base + offset);
    test_cap <- setBounds(test_cap, newLength);
    test_cap <- setOffset(test_cap, 0);
    test_cap <- setSealed(test_cap, sealed);
    if getBase(test_cap) == base + offset and
       getOffset(test_cap) == 0 and
       getLength(test_cap) == newLength and
       getSealed(test_cap) == sealed then
       true else false
}

-- In memory representation
--------------------------------------------------------------------------------

--                     Embedded Exp
-- 127___123_122_112_111_106_105___104___103________________________85_84_________________________64_
-- |        |       |       |   |                                     |                             |
-- | uperms | perms |  res  | 0 |len<19>|                    top<18:0>|                   base<20:0>| Exp0
-- | uperms | perms |  res  | 1 |   0   |             top<18:3>|e<5:3>|            base<20:3>|e<2:0>| EmbeddedExp
-- | uperms | perms |  res  | 1 |   1   |top<18:12>|otype<17:9>|e<5:3>|base<20:12>|otype<8:0>|e<2:0>| Sealed
-- |________|_______|_______|___|_____________________________________|_____________________________|
-- 63_______________________________________________________________________________________________0
-- |                                                                                                |
-- |                                              address                                           |
-- |________________________________________________________________________________________________|

-- reconstructing most significant top bits:
-- top<20:19> = base<20:19> + carry_out + len_correction
--      where
--              carry_out      = 1 if top<18:0> < base <18:0>
--                               0 otherwise
--              len_correction = len<19> if Exp0
--                                  0    otherwise

bits(4) encUPerms (up::UPerms) = &up<3:0>
UPerms decUPerms (up::bits(4)) = UPerms(ZeroExtend(up))

bits(11) encPerms (p::Perms) = &p<10:0>
Perms decPerms (p::bits(11)) = Perms(SignExtend(p))

-- map exp 44 or 0b101100 to 0 representation
bits(6) encExp (e::nat) =
{
    ebits::bits(6) = [e];
    [~[ebits<5>]] : [ebits<4>] : ~ebits<3:2> : ebits<1:0>
}
nat decExp (e::bits(6)) = [~[e<5>] : [e<4>] : ~e<3:2> : e<1:0>]

bits(42) encBounds (format::Format, bounds::Bounds) =
{
    e = encExp(bounds.exp);
    otype = bounds.otype;
    match format
    {
        case Exp0          => '0'  : bounds.len19 : bounds.topBits<18:0> : bounds.baseBits<20:0>
        case EmbeddedExp   => '10' : bounds.topBits<18:3> : e<5:3> : bounds.baseBits<20:3> : e<2:0>
        case Sealed        => '11' : bounds.topBits<18:12> : otype<17:9> : e<5:3> : bounds.baseBits<20:12> : otype<8:0> : e<2:0>
    }
}
Format * Bounds decBounds (bounds :: bits(42)) =
{
    var f::Format;
    var b::Bounds;
    b.otype    <- 0;
    b.exp      <- 0;
    b.len19    <- 0;
    b.topBits  <- 0;
    b.baseBits <- 0;

    f <- if not bounds<41> then Exp0
            else if not bounds<40> then EmbeddedExp
            else Sealed;

    match f
    {
        case Exp0 => {
            b.len19         <- [bounds<40>];
            b.topBits<18:0> <- bounds<39:21>;
            b.baseBits      <- bounds<20:0>
        }
        case EmbeddedExp => {
            b.exp           <- decExp(bounds<23:21> : bounds<2:0>);
            b.topBits<18:0> <- bounds<39:24> : 0`3;
            b.baseBits      <- bounds<20:3> : 0`3
        }
        case Sealed => {
            b.otype         <- bounds<32:24> : bounds<11:3>;
            b.exp           <- decExp(bounds<23:21> : bounds<2:0>);
            b.topBits<18:0> <- bounds<39:33> : 0`12;
            b.baseBits      <- bounds<20:12> : 0`12
        }
    };
    carry_out      = if b.topBits<18:0> < b.baseBits<18:0> then '01' else '00';
    len_correction = match f { case Exp0 => '0':b.len19 case _ => '00'};
    b.topBits<20:19> <- b.baseBits<20:19> + carry_out + len_correction;
    (f,b)
}

CAPRAWBITS capToBits (cap :: Capability) =
    cap.address:encUPerms(cap.uperms):encPerms(cap.perms):cap.reserved:encBounds(cap.format,cap.bounds)

Capability bitsToCap (raw :: CAPRAWBITS) =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.uperms   <- decUPerms(raw<63:60>);
    new_cap.perms    <- decPerms(raw<59:49>);
    new_cap.reserved <- raw<48:42>;
    f,b = decBounds(raw<41:0>);
    new_cap.format   <- f;
    new_cap.bounds   <- b;
    new_cap.address  <- raw<127:64>;
    new_cap
}

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
if dwordAddr<0> then raw<127:64> else raw<63:0>

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
if dwordAddr<0> then
    (old_blob<127:64> && ~mask || data && mask) : old_blob<63:0>
else
    old_blob<127:64> : (old_blob<63:0> && ~mask || data && mask)

-- display and logging utils
--------------------------------------------------------------------------------

string log_cap_format (cap::Capability) = match cap.format
{
    case Exp0        => "Exp0"
    case EmbeddedExp => "EmbeddedExp"
    case Sealed      => "Sealed"
}

string log_cap_bounds (cap::Capability) =
{
    b = cap.bounds;
    "{otype:":hex(b.otype):
    " exp:":[b.exp]:
    " len19:":hex(b.len19):
    " topBits:":hex(b.topBits):
    " baseBits:":hex(b.baseBits):"}"
}

string log_cap_inner_rep (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " uperms:":hex (cap.&uperms):
    " perms:":hex (cap.&perms):
    " reserved:":hex (cap.reserved):
    " format:": log_cap_format(cap):
    " bounds:": log_cap_bounds(cap):
    " address:": hex (cap.address)

string log_cap_write (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " s:":(if getSealed(cap) then "1" else "0"):
    " perms:":hex (cap.&uperms<3:0> : SignExtend(cap.&perms<10:0>)`15): -- TODO report 2 architectural fields
    " type:":hex (getType(cap)):
    " offset:":hex (getOffset(cap)):
    " base:":hex (getBase(cap)):
    " length:":hex (getLength(cap))--:"\n(":log_cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[":hex (pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[":hex (pAddr):"]"
