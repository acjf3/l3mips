---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
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
    256-256 : isCap     -- 1 tag bit
    255-248 : reserved  -- 8 Reserved bits
    247-224 : otype     -- 24 type bits
    223-193 : perms     -- 31 permission bits
    192-192 : unsealed  -- 1 unsealed bit
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
  c_capr    :: id -> CapRegFile    -- capability register file
  c_pcc     :: id -> Capability    -- program counter capability
}

component PCC :: Capability
{
   value = c_pcc(procID)
   assign value = c_pcc(procID) <- value
}

component CAPR (n::reg) :: Capability
{
   value = { m = c_capr(procID); m(n) }
   assign value = { var m = c_capr(procID)
                  ; m(n) <- value
                  ; c_capr(procID) <- m }
}

---------------------------------------
-- Capability coprocessor instructions
---------------------------------------

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERI > CGet > CGetBase (rd::reg, cb::reg) =
   GPR(rd) <- CAPR(cb).base

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERI > UnknownCapInstruction =
   SignalException (ResI)

define LWC2 > CLoad (v::bits(26)) = ()
define LDC2 > CLoadCap(v::bits(26)) = ()
define SWC2 > CStore(v::bits(26)) = ()
define SDC2 > CStoreCap(v::bits(26)) = ()
