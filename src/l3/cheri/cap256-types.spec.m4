---------------------------------------------------------------------------
-- CHERI types for 256-bits precice capability
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-params.m4')dnl

-- types definitions

register Perms :: bits (31)
{
    30-15 : soft
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

register Capability :: bits (257)
{
        256 : tag       -- 1 tag bit
    255-248 : reserved  -- 8 Reserved bits
    247-224 : otype     -- 24 type bits
    223-193 : perms     -- 31 permission bits
        192 : sealed    -- 1 sealed bit
    191-128 : offset    -- 64 offset bits
     127-64 : base      -- 64 base bits
       63-0 : length    -- 64 length bits
}

-- Capability API

bool     getTag    (cap::Capability) = cap.tag
bits(24) getType   (cap::Capability) = cap.otype -- 16 bits in 128-bits mode
Perms    getPerms  (cap::Capability) = Perms(cap.perms) -- 8 bits in 128-bits mode
bool     getSealed (cap::Capability) = cap.sealed
bits(64) getOffset (cap::Capability) = cap.offset
bits(64) getBase   (cap::Capability) = cap.base
bits(64) getLength (cap::Capability) = cap.length

Capability setTag    (cap::Capability, tag::bool)        = {var new_cap = cap; new_cap.tag    <- tag;    new_cap}
Capability setType   (cap::Capability, otype::bits(24))  = {var new_cap = cap; new_cap.otype  <- otype;  new_cap}
Capability setPerms  (cap::Capability, perms::Perms)     = {var new_cap = cap; new_cap.perms  <- &perms; new_cap}
Capability setSealed (cap::Capability, sealed::bool)     = {var new_cap = cap; new_cap.sealed <- sealed; new_cap}
Capability setOffset (cap::Capability, offset::bits(64)) = {var new_cap = cap; new_cap.offset <- offset; new_cap}
Capability setBase   (cap::Capability, base::bits(64))   = {var new_cap = cap; new_cap.base   <- base;   new_cap}
Capability setLength (cap::Capability, length::bits(64)) = {var new_cap = cap; new_cap.length <- length; new_cap}
Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    new_cap.base   <- cap.base + cap.offset;
    new_cap.length <- length;
    new_cap.offset <- 0;
    new_cap
}

bool isCapAligned  (addr::bits(64))  = addr<4:0> == 0

CAPRAWBITS capToBits (cap :: Capability) =
    &cap<63:0> : &cap<127:64> : &cap<191:128> : &cap<255:192>

Capability bitsToCap (raw :: CAPRAWBITS) =
    Capability('0' : raw<63:0> : raw<127:64> : raw<191:128> : raw<255:192>)

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
match dwordAddr<1:0>
{
    case '00' => raw<63:0>
    case '01' => raw<127:64>
    case '10' => raw<191:128>
    case '11' => raw<255:192>
}

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
match dwordAddr<1:0>
{
    case '00' => old_blob<255:64>  : (old_blob<63:0>    && ~mask || data && mask)
    case '01' => old_blob<255:128> : (old_blob<127:64>  && ~mask || data && mask) : old_blob<63:0>
    case '10' => old_blob<255:192> : (old_blob<191:128> && ~mask || data && mask) : old_blob<127:0>
    case '11' => (old_blob<255:192> && ~mask || data && mask) : old_blob<191:0>
}

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag <- true;
    new_cap.sealed <- false;
    new_cap.offset <- 0;
    new_cap.base <- 0;
    new_cap.length <- ~0;
    new_cap.otype <- 0;
    new_cap.perms <- ~0;
    new_cap.reserved <- 0;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag <- false;
    new_cap.sealed <- false;
    new_cap.offset <- 0;
    new_cap.base <- 0;
    new_cap.length <- 0;
    new_cap.otype <- 0;
    new_cap.perms <- 0;
    new_cap.reserved <- 0;
    new_cap
}

-- log utils --

string hex24 (x::bits(24)) = ToLower (PadLeft (#"0", 6, [x]))
string hex31 (x::bits(31)) = ToLower (PadLeft (#"0", 8, [x]))

string log_cap_write (cap::Capability) =
    "u:":(if getSealed(cap) then "1" else "0"):
    " perms:0x":hex31(&getPerms(cap)):
    " type:0x":hex24(getType(cap)):
    " offset:0x":hex64(getOffset(cap)):
    " base:0x":hex64(getBase(cap)):
    " length:0x":hex64(getLength(cap))

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
