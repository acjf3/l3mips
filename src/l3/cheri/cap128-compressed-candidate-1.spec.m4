---------------------------------------------------------------------------
-- CHERI types for 128-bits candidate 1
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-params.m4')dnl

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

register Perms :: bits (23)
{
    22-15 : soft
       14 : Access_KR2C
       13 : Access_KR1C
       12 : Access_KCC
       11 : Access_KDC
       10 : Access_EPCC
        9 : Reserved
        8 : Permit_Set_Type
        7 : Permit_Seal
        6 : Permit_Store_Local_Capability
        5 : Permit_Store_Capability
        4 : Permit_Load_Capability
        3 : Permit_Store
        2 : Permit_Load
        1 : Permit_Execute
        0 : Global
}

type OType = bits(16)

register TypedPointer :: bits (64)
{
    63-59 : seg         -- top 5 bits of virtual address
    58-43 : otype       -- the type of the sealed object
     42-0 : vaddr       -- the 40 bits of the virtual address
}

-- in memory representation of compressed 128 bits capabilities
register Capability :: bits (129)
{
        128 : tag       -- 1 tag bit
    127-105 : perms     -- 23 permission bits
        104 : base_eq_pointer
        103 : unused    -- unused
     102-97 : exp       -- 6 exponent bits
      96-81 : toTop     -- 16 bits signed mantis
      80-65 : toBottom  -- 16 bits signed mantis
         64 : sealed    -- 1 sealed bit
       63-0 : pointer   -- 64 bits pointer / compressed address + type when sealed
}

---------------------------------
-- capability helper functions --
--------------------------------------------------------------------------------

bits(64) getPtr (cap::Capability) =
if not cap.sealed then
    cap.pointer
else
{
    var addr = cap.pointer;
    addr<58:0> <- SignExtend(TypedPointer(cap.pointer).vaddr);
    addr
}
bits(65) innerGetTop (cap::Capability) =
{
    top::bits(66) = (ZeroExtend(getPtr(cap)) + (SignExtend(cap.toTop) << [cap.exp])) && (~0<<[cap.exp]);
    zero::bits(64) = 0;
    if top <64> then '1':zero else top<64:0>
}
bits(65) innerGetBase (cap::Capability) =
{
    var ret::bits(66) = ZeroExtend(getPtr(cap));
    when not cap.base_eq_pointer do
        ret <- (ret + (SignExtend(cap.toBottom) << [cap.exp])) && (~0<<[cap.exp]);
    (ret<64:0>)
}

nat innerZeroCount (data::bool list, acc::nat) = match data
{
    case Nil => acc
    case Cons(hd, tl) => if hd then acc else innerZeroCount (tl, acc + 1)
}
nat countLeadingZeros (data::bits(64)) = innerZeroCount ([data], 0)

Capability updatePtr (cap::Capability, ptr::bits(64)) =
{
    var new_cap = cap;
    if cap.sealed then
    {
        TypedPointer(new_cap.pointer).vaddr <- ptr<42:0>;
        TypedPointer(new_cap.pointer).seg   <- ptr<63:59>
    }
    else new_cap.pointer <- ptr;
    new_cap
}

Capability updateBounds (cap::Capability, ptr::bits(64)) =
{
    ptrDiff     = ((ptr >>+ [cap.exp]) - (getPtr(cap) >>+ [cap.exp]))<15:0>;
    newToTop    = cap.toTop    - ptrDiff;
    newToBottom = cap.toBottom - ptrDiff;

    var new_cap = cap;
    new_cap.toTop    <- newToTop;
    new_cap.toBottom <- newToBottom;
    new_cap
}

--------------------------------------
-- capability "typeclass" functions --
--------------------------------------------------------------------------------

bool allow_system_reg_access(p::Perms, r::reg) =
(  r == 31 and not p.Access_EPCC
or r == 30 and not p.Access_KDC
or r == 29 and not p.Access_KCC
or r == 27 and not p.Access_KR1C
or r == 28 and not p.Access_KR2C )

bool isCapAligned    (addr::bits(64))  = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) = &cap<63:0> : &cap<127:64> -- XXX swap dwords to match CHERI bluespec implementation

Capability bitsToCap (raw :: CAPRAWBITS) = Capability('0' : raw<63:0> : raw<127:64>) -- XXX swap dwords to match CHERI bluespec implementation

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
if dwordAddr<0> then raw<127:64> else raw<63:0>

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
if dwordAddr<0> then
    (old_blob<127:64> && ~mask || data && mask) : old_blob<63:0>
else
    old_blob<127:64> : (old_blob<63:0> && ~mask || data && mask)

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag <- true;
    new_cap.sealed <- false;
    new_cap.perms <- ~0;
    new_cap.unused <- false;
    new_cap.base_eq_pointer <- true;
    new_cap.exp <- 0x32; -- leftshift by 50
    new_cap.pointer <- 0;
    new_cap.toBottom <- 0;
    new_cap.toTop <- 0x4000;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag <- false;
    new_cap.sealed <- false;
    new_cap.perms <- 0;
    new_cap.unused <- false;
    new_cap.base_eq_pointer <- false;
    new_cap.exp <- 0;
    new_cap.pointer <- 0;
    new_cap.toBottom <- 0;
    new_cap.toTop <- 0;
    new_cap
}

------------------------------------
-- capability "typeclass" getters --
--------------------------------------------------------------------------------

bool     getTag    (cap::Capability) = cap.tag
OType    getType   (cap::Capability) = if cap.sealed then TypedPointer(cap.pointer).otype else 0
Perms    getPerms  (cap::Capability) = Perms(cap.perms)
bool     getSealed (cap::Capability) = cap.sealed
bits(64) getBase (cap::Capability) = innerGetBase(cap)<63:0>
bits(64) getOffset (cap::Capability) = getPtr(cap) - getBase(cap)
bits(64) getLength (cap::Capability) =
{
    len = innerGetTop(cap) - innerGetBase(cap);
    if len<64> then ~0 else len<63:0>
}

------------------------------------
-- capability "typeclass" setters --
--------------------------------------------------------------------------------

Capability setTag    (cap::Capability, tag::bool)        = {var new_cap = cap; new_cap.tag      <- tag; new_cap}
Capability setType   (cap::Capability, otype::OType)     =
{
    var new_cap = cap;
    when cap.sealed do
        TypedPointer(new_cap.pointer).otype <- otype;
    new_cap
}
Capability setPerms  (cap::Capability, perms::Perms)     = {var new_cap = cap; new_cap.perms    <- &perms; new_cap}
Capability setSealed (cap::Capability, sealed::bool) =
{
    var new_cap = cap;
    ptr = cap.pointer<63:59> : SignExtend(TypedPointer(cap.pointer).vaddr);
    new_cap <- updateBounds (new_cap, ptr);
    new_cap.pointer <- ptr;
    new_cap.sealed <- sealed;
    new_cap
}
Capability setOffset (cap::Capability, offset::bits(64)) =
{
    -- XXX experimental :
    oldbase = innerGetBase(cap);
    oldtop = innerGetTop(cap);
    ---------------------

    var new_cap = cap;
    newPtr      = getBase(cap) + offset;
    new_cap <- updateBounds(new_cap, newPtr);
    new_cap <- updatePtr(new_cap, newPtr);

    new_cap.base_eq_pointer <- if offset == 0 then true else false;

    -- XXX experimental :
    newbase = innerGetBase(new_cap);
    newtop = innerGetTop(new_cap);
    when (oldbase <> newbase) or (oldtop <> newtop) do
    {
        new_cap.exp <- 0x32; -- exp of 50
        dist = (offset >>+ 50)<14:0>;
        new_cap.toBottom <- SignExtend(-dist); -- force base to 0
        new_cap.toTop <- SignExtend(-(dist-1)); -- force top to 1 (length of 0)
        new_cap.pointer <- offset; -- force offset to provided offset
        new_cap.tag <- false
    };
    ---------------------

    new_cap
}

Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    -- set length (pick best representation)
    zeros  = countLeadingZeros (length);
    newExp::nat = if (zeros > 50) then 0 else 50 - zeros; -- 50 is actually 65 - 15, 15 being the mantissa size minus 1 for the sign bit
    new_cap.exp <- [newExp];
    new_cap.toTop <- ZeroExtend((length >>+ newExp)<14:0>);
    when length && ~(~0 << newExp) <> 0 do new_cap.toTop <- new_cap.toTop + 1;
    -- set base (and offset of 0)
    new_cap.toBottom <- 0;
    -- return initialized capability
    new_cap
}

---------------
-- log utils --
--------------------------------------------------------------------------------

string hex16 (x::bits(16)) = ToLower (PadLeft (#"0", 4, [x]))
string hex23 (x::bits(23)) = ToLower (PadLeft (#"0", 6, [x]))

{-
string cap_inner_rep (cap::Capability) =
    "u:":(if cap.tag then "1" else "0"):
    " perms:0x":hex23(cap.perms):
    " base_eq_ptr:":(if cap.base_eq_pointer then "1" else "0"):
    " exp:":[[cap.exp]::nat]:
    " toTop:0x":hex16(cap.toTop):
    " toBottom:0x":hex16(cap.toBottom):
    " sealed:":(if cap.sealed then "1" else "0"):
    " pointer:0x":hex64(cap.pointer)
-}

string log_cap_write (cap::Capability) =
    "u:":(if getSealed(cap) then "1" else "0"):
    " perms:0x":hex23(&getPerms(cap)):
    " type:0x":hex16(getType(cap)):
    " offset:0x":hex64(getOffset(cap)):
    " base:0x":hex64(getBase(cap)):
    " length:0x":hex64(getLength(cap))--:"\\n(":cap_inner_rep(cap):")"

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
