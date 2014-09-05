---------------------------------------------------------------------------
-- CHERI related state elements
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------
-- Capability Coprocessor types
--------------------------------

register Perms :: bits (31)
{
    30-15   : soft
    14-14   : access_CR28
    13-13   : access_CR27
    12-12   : access_CR29
    11-11   : access_CR30
    10-10   : access_CR31
    9-9     : reserved
    8-8     : permit_set_type
    7-7     : permit_seal
    6-6     : permit_store_ephemeral_cap
    5-5     : permit_store_cap
    4-4     : permit_load_cap
    3-3     : permit_store
    2-2     : permit_load
    1-1     : permit_execute
    0-0     : non_ephemeral
}

register Capability :: bits (257)
{
    256-256 : tag       -- 1 tag bit
    255-248 : reserved  -- 8 Reserved bits
    247-224 : otype     -- 24 type bits
    223-193 : perms     -- 31 permission bits
    192-192 : sealed    -- 1 sealed bit
    191-128 : offset    -- 64 offset bits
    127-64  : base      -- 64 base bits
    63-0    : length    -- 64 length bits
}

--------------------------------
-- Capability coprocessor state
--------------------------------

type CapRegFile = reg -> Capability

declare
{
  c_pcc     :: id -> Capability    -- program counter capability
  c_capr    :: id -> CapRegFile    -- capability register file
}

component CAPR (n::reg) :: Capability
{
   value = { m = c_capr(procID); m(n) }
   assign value = { var m = c_capr(procID)
                  ; m(n) <- value
                  ; c_capr(procID) <- m }
}

component RCC :: Capability
{
   value = CAPR(24)
   assign value = CAPR(24) <- value
}

component IDC :: Capability
{
   value = CAPR(26)
   assign value = CAPR(26) <- value
}

component KR1C :: Capability
{
   value = CAPR(27)
   assign value = CAPR(27) <- value
}

component KR2C :: Capability
{
   value = CAPR(28)
   assign value = CAPR(28) <- value
}

component KCC :: Capability
{
   value = CAPR(29)
   assign value = CAPR(29) <- value
}

component KDC :: Capability
{
   value = CAPR(30)
   assign value = CAPR(30) <- value
}

component EPCC :: Capability
{
   value = CAPR(31)
   assign value = CAPR(31) <- value
}
