---------------------------------------------------------------------------
-- CHERI Concentrate
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- types definitions
--------------------------------------------------------------------------------

type OType = bits(18)

record Capability
{
    tag       :: bool     -- 1-bit validity tag
    uperms    :: UPerms   -- 4-bit user permissions
    perms     :: Perms    -- 12-bit permissions
    reserved  :: bits(2)  -- 2-bit reserved
    otype     :: bits(18) -- object type
    internalE :: bool     -- are we using a non zero internal exponent
    E         :: bits(6)  -- the internal exponent
    B         :: bits(14) -- Base bits
    T         :: bits(14) -- Top bits
    address   :: bits(64) -- 64-bit address
}

-- standard capabilities definitions
--------------------------------------------------------------------------------

bits(6)  resetE = 52
bits(14) resetT = 0x1000 -- set the second to last bit (bit index 12)

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag       <- true;
    new_cap.uperms    <- UPerms(~0);
    new_cap.perms     <- Perms(~0);
    new_cap.reserved  <- 0;
    new_cap.internalE <- true;
    new_cap.E         <- resetE;
    new_cap.B         <- 0;
    new_cap.T         <- resetT;
    new_cap.otype     <- ~0;
    new_cap.address   <- 0;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag       <- false;
    new_cap.uperms    <- UPerms(0);
    new_cap.perms     <- Perms(0);
    new_cap.reserved  <- 0;
    new_cap.internalE <- true;
    new_cap.E         <- resetE;
    new_cap.B         <- 0;
    new_cap.T         <- resetT;
    new_cap.otype     <- ~0;
    new_cap.address   <- 0;
    new_cap
}

-- inner helpers
--------------------------------------------------------------------------------

bits(64) * bits(65) getCapBounds(cap::Capability) =
{
  -- Do address, base and top lie in the R aligned region above the one containing R?
  R :: bits(3) = cap.B<13:11> - 0b001;
  aHi :: int = if (cap.address >> ZeroExtend((cap.E + 11)))<2:0> <+ R then 1 else 0;
  bHi :: int = if cap.B<13:11> <+ R then 1 else 0;
  tHi :: int = if cap.T<13:11> <+ R then 1 else 0;
  -- Compute region corrections for top and base relative to a
  correction_base :: int = bHi - aHi;
  correction_top  :: int = tHi - aHi;
  a_top = cap.address >>+ ZeroExtend((cap.E + 14));
  --
  var base :: bits(65) = ([[[a_top]::nat] + correction_base] : cap.B) << ZeroExtend(cap.E);
  var top  :: bits(65) = ([[[a_top]::nat] + correction_top]  : cap.T) << ZeroExtend(cap.E);
  when (base<64> == true) do
    top<64> <- (aHi == 1) and (tHi == 1);
  (base<63:0>, top)
}
-- Interface
--------------------------------------------------------------------------------

bool getTag (cap::Capability) = cap.tag

OType getType (cap::Capability) = cap.otype

UPerms getUPerms (cap::Capability) = cap.uperms

Perms getPerms (cap::Capability) = Perms(SignExtend(cap.&perms<11:0>))

bool getSealed (cap::Capability) = cap.otype != ~0

bits(64) getBase (cap::Capability)   = {b,_ = getCapBounds(cap); b}

bits(64) getTop (cap::Capability)    = {_,t = getCapBounds(cap); t<63:0>}

bits(64) getLength (cap::Capability) =
{
  b,t = getCapBounds(cap);
  len = (t - ZeroExtend(b));
  if len >=+ [2**64] then ~0 else len<63:0>
}

bits(64) * bits(64) getBaseAndLength (cap::Capability) =
(getBase(cap), getLength(cap))

bits(64) getOffset (cap::Capability) = cap.address - getBase(cap)

Capability setOffset (cap::Capability, offset::bits(64)) =
{
    var new_cap = cap;
    new_cap.address <- getBase(cap) + offset;
    new_cap
}

nat idxMSNZ (data::bits(65)) = if data == 0 then 0 else [Log2 (data)]
nat count_leading_zeros(data::bits(65)) = 64 - idxMSNZ(data)
Capability setBounds (cap::Capability, length::bits(64)) =
{
    base   :: bits(65) = ZeroExtend(cap.address);
    top    :: bits(65) = base + ZeroExtend(length);
    newLen :: bits(65) = top - base;
    e  :: nat  = 52 - count_leading_zeros(ZeroExtend(newLen<64:13>));
    ie :: bool = (e != 0) or newLen<12>;

    var Bbits :: bits(14) = [base];
    var Tbits :: bits(14) = [top];
    var lostSignificantBase :: bool = false;
    var lostSignificantTop  :: bool = false;
    var incE :: bool = false;

    when ie do {
      var B_ie :: bits(11) = [base >> [e + 3]];
      var T_ie :: bits(11) = [top  >> [e + 3]];

      maskLo :: bits(65) = ~(~0 << [e + 3]);
      lostSignificantBase <- (base && maskLo) <> 0;
      lostSignificantTop  <- (top  && maskLo) <> 0;

      when lostSignificantTop do T_ie <- T_ie + 1;

      len_ie = T_ie - B_ie;
      when len_ie<10> do {
        incE <- true;

        lostSignificantBase <- lostSignificantBase or B_ie<0>;
        lostSignificantTop  <- lostSignificantTop  or T_ie<0>;

        B_ie <- [base >> [e + 4]];
        incT :: bits(11) = if lostSignificantTop then 0x1 else 0x0;
        T_ie <- [top >> [e + 4]] + incT
      };

      Bbits <- B_ie : 0b000;
      Tbits <- T_ie : 0b000
    };
    var new_cap = cap;
    new_cap.address <- [base];
    new_cap.E <- if incE then [e + 1] else [e];
    new_cap.B <- Bbits;
    new_cap.T <- Tbits;
    new_cap.internalE <- ie;
    --exact = not(lostSignificantBase or lostSignificantTop);
    --(exact, newCap)
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
    when not sealed do new_cap.otype <- ~0;
    new_cap
}

Capability setType (cap::Capability, otype::OType) =
{
  var new_cap = cap;
  new_cap.otype <- otype;
  new_cap
}

bool isCapAligned (addr::bits(64)) = addr<3:0> == 0

-- representability tests
--------------------------------------------------------------------------------

bool canRepCap( cap::Capability,
                newSealed::bool,
                newOffset::bits(64)) = true
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

CAPRAWBITS rawCapBits (cap :: Capability) =
{
  uprms :: bits(4)  = cap.&uperms<3:0>;
  prms  :: bits(12) = cap.&perms<11:0>;
  ie    :: bits(1)  = if cap.internalE then '1' else '0';
  base  = if cap.internalE then cap.B<13:3> : cap.E<2:0> else cap.B;
  top   = if cap.internalE then cap.T<12:3> : cap.E<5:3> else cap.T<12:0>;
  cap.address:uprms:prms:cap.reserved:cap.otype:ie:base:top
}

CAPRAWBITS capToBits (cap :: Capability) = rawCapBits(nullCap) ?? rawCapBits(cap)

Capability bitsToCap (raw :: CAPRAWBITS) =
{
  newRaw = rawCapBits(nullCap) ?? raw;
  var new_cap = nullCap;
    new_cap.uperms    <- UPerms(ZeroExtend(newRaw<63:60>));
    new_cap.perms     <- Perms(SignExtend(newRaw<59:48>));
    new_cap.reserved  <- newRaw<47:46>;
    new_cap.otype     <- newRaw<45:28>;
    new_cap.internalE <- newRaw<27>;
    new_cap.E         <- if new_cap.internalE then newRaw<15:13> : newRaw<2:0> else 0;
    new_cap.B         <- if new_cap.internalE then newRaw<26:16> : '000' else newRaw<26:13>;
    new_cap.T         <- if new_cap.internalE then '1' : newRaw<12:3> : '000' else '1':newRaw<12:0>;
    new_cap.address   <- newRaw<127:64>;
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

string log_cap_bounds (cap::Capability) =
{
    "meh" -- TODO
}

string log_cap_inner_rep (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " uperms:":hex (cap.&uperms):
    " perms:":hex (cap.&perms):
    " reserved:":hex (cap.reserved):
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
