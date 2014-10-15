---------------------------------------------------------------------------
-- CHERI types
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

register CapCause :: bits (16)
{
    15-8 : ExcCode  -- 8 bits exception code
     7-0 : RegNum   -- 8 bits register number
}

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

nat capByteWidth = 32   -- 256bits wide capabilities

bool isCapAligned  (addr::bits(64))  = addr<4:0> == 0

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