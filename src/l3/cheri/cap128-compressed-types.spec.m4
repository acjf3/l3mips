---------------------------------------------------------------------------
-- CHERI types for 128-bits precice capability
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-params.m4')dnl

-- types definitions

register Perms :: bits (23)
{
    22-15 : soft
       14 : Access_KR2C
       13 : Access_KR1C
       12 : Access_KCC
       11 : Access_KDC
       10 : Access_EPCC
        9 : base_eq_pointer -- XXX taken from the CapCop128.bsv ... should change
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

register VirtAddress :: bits (45)
{
    44-40 : seg
     39-0 : address
}

type OType = bits(16)

register TypedPointer :: bits (64)
{
    63-61 : reserved    -- XXX what's going on here ? wasted bits ? (from CapCop128.bsv)
    60-45 : otype       -- the type of the sealed object
     44-0 : shortAddr   -- of type VirtAddr
}

-- conceptually, sealed + pointer is equivalent to :
-- construct PointerType {Full :: bits(64), Typed :: TypedPointer}

-- in memory representation of compressed 128 bits capabilities
register Capability :: bits (129)
{
        128 : tag       -- 1 tag bit
    127-105 : perms     -- 23 permission bits
     104-99 : exp       -- 6 exponent bits
      98-82 : toTop     -- 17 bits signed mantis
      81-65 : toBottom  -- 17 bits signed mantis
         64 : sealed    -- 1 sealed bit
       63-0 : pointer   -- 64 bits pointer / compressed address + type when sealed
}

-- Capability API

-- XXX implemented according to CapCop128.bsv

-------------
-- getters --
-------------

bool     getTag    (cap::Capability) = cap.tag
OType    getType   (cap::Capability) = if cap.sealed then TypedPointer(cap.pointer).otype else 0
Perms    getPerms  (cap::Capability) = Perms(cap.perms)
bool     getSealed (cap::Capability) = cap.sealed
bits(64) getPtr (cap::Capability) =
if not cap.sealed then
    cap.pointer
else
{
    var addr = 0;
    vaddr = VirtAddress(TypedPointer(cap.pointer).shortAddr);
    addr<57:0> <- SignExtend(vaddr.address); -- XXX help ? SignExtend an address ?
    -- XXX bit 58 always 0 ?
    addr<63:59> <- vaddr.seg;
    addr
}
bits(64) getBase (cap::Capability) =
{
    var ret = getPtr(cap);
    when not getPerms(cap).base_eq_pointer do
    {
        mask = (-1) << [cap.exp];
        ret <- ret + (SignExtend(cap.toBottom) << [cap.exp]);
        ret <- ret && mask -- Ensure the base is aligned
    };
    ret
}
bits(64) getOffset (cap::Capability) = getPtr(cap) - getBase(cap)

bits(64) getLength (cap::Capability) =
{
    var ret = ZeroExtend(cap.toTop - cap.toBottom) + 1;
    ret <- (ret << [cap.exp]) - 1;
    ret
}

-------------
-- setters --
-------------

nat innerZeroCount (data::bool list, acc::nat) =
if Head (data) == true then acc else innerZeroCount (Tail (data), acc + 1)
nat countLeadingZeros (data::bits(64)) = innerZeroCount ([data], 0)

Capability setTag    (cap::Capability, tag::bool)        = {var new_cap = cap; new_cap.tag      <- tag; new_cap}
Capability setType   (cap::Capability, otype::OType)     = {var new_cap = cap; new_cap.pointer<60:45> <- otype;  new_cap}
Capability setPerms  (cap::Capability, perms::Perms)     = {var new_cap = cap; new_cap.perms    <- &perms; new_cap}
Capability setSealed (cap::Capability, sealed::bool)     = {var new_cap = cap; new_cap.sealed   <- sealed; new_cap}
Capability setOffset (cap::Capability, offset::bits(64)) =
{
    var new_cap = cap;
    newPtr      = getBase(cap) + offset;
    ptrDiff     = ((newPtr - getPtr(cap)) >> [cap.exp])<16:0>;
    newToTop    = cap.toTop - ptrDiff;
    newToBottom = cap.toBottom - ptrDiff;

    new_cap.toTop    <- newToTop;
    new_cap.toBottom <- newToBottom;
    new_cap.pointer  <- newPtr;

    Perms(new_cap.perms).base_eq_pointer <- false;
    new_cap
}

Capability setBase   (cap::Capability, base::bits(64)) =
{
    var new_cap = cap;
    if Perms(new_cap.perms).base_eq_pointer then
    {
        new_cap.pointer <- base
    }
    else
    {
        newToBottom = ((base - getBase(cap)) >> [cap.exp])<16:0> + cap.toBottom;
        new_cap.toBottom <- newToBottom
    };
    new_cap
}

Capability setLength (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    if (getPerms(cap).base_eq_pointer) then -- Normalise if the base is precise
    {
        zeros  = countLeadingZeros (length);
        newExp = if (zeros > 48) then 0 else 48 - zeros;
        new_cap.exp      <- [newExp];
        new_cap.toBottom <- 0;
        new_cap.toTop    <- (length >> newExp)<16:0>
    }
    else -- Otherwise, don't normalise
        new_cap.toTop <- ((length + SignExtend(cap.toBottom)<<[cap.exp])>>[cap.exp])<16:0>;
    new_cap
}

bool isCapAligned    (addr::bits(64))  = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) = &cap<127:0> -- XXX might want to reverse the 2 dwords to be consistent with the 256 bits implementation

Capability bitsToCap (raw :: CAPRAWBITS) = Capability('0' : raw) -- XXX same here ?

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
    new_cap.exp <- 0x30; -- 48 leftshift 48
    new_cap.pointer <- 0;
    new_cap.toBottom <- 0;
    new_cap.toTop <- 0x0FFFF;
    new_cap
}

-- log utils --

string hex16 (x::bits(16)) = PadLeft (#"0", 4, [x])
string hex23 (x::bits(23)) = PadLeft (#"0", 6, [x])
string hex40 (x::bits(40)) = PadLeft (#"0", 10, [x])

string log_cap_write (cap::Capability) =
    "u:":(if getSealed(cap) then "1" else "0"):
    " perms:0x":hex23(&getPerms(cap)):
    " type:0x":hex16(getType(cap)):
    " offset:0x":hex64(getOffset(cap)):
    " base:0x":hex64(getBase(cap)):
    " length:0x":hex64(getLength(cap))

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
