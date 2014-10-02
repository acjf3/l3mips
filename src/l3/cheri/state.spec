---------------------------------------------------------------------------
-- CHERI related basic types and components
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------
-- Capability Coprocessor types
--------------------------------

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

register CapCause :: bits (16)
{
    15-8 : ExcCode  -- 8 bits exception code
     7-0 : RegNum   -- 8 bits register number
}

string hex40(x :: bits(40)) =
  pad(10, [x]):(if x == 0 then "" else [x])

string hex31(x :: bits(31)) =
  pad(8, [x]):(if x == 0 then "" else [x])

string hex24(x :: bits(24)) =
  pad(6, [x]):(if x == 0 then "" else [x])

string cap_write (cap::Capability) = 
    "u:":(if cap.sealed then "1" else "0"):
    " perms:0x":hex31(cap.perms):
    " type:0x":hex24(cap.otype):
    " offset:0x":hex64(cap.offset):
    " base:0x":hex64(cap.base):
    " length:0x":hex64(cap.length)

string cpp_write (cap::Capability) = "PCC <- ":cap_write(cap)
string creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":cap_write(cap)
string store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":cap_write(cap)

--------------------------------
-- Capability coprocessor state
--------------------------------
declare
{
    c_BranchDelayPCC    :: id -> (bits(64) * Capability) option
    c_BranchToPCC       :: id -> (bits(64) * Capability) option
}

component BranchDelayPCC :: (bits(64) * Capability) option
{
   value = c_BranchDelayPCC(procID)
   assign value = c_BranchDelayPCC(procID) <- value
}

component BranchToPCC :: (bits(64) * Capability) option
{
   value = c_BranchToPCC(procID)
   assign value = c_BranchToPCC(procID) <- value
}

type CapRegFile = reg -> Capability

declare
{
    c_capcause:: id -> CapCause      -- capability exception cause register
    c_pcc     :: id -> Capability    -- program counter capability
    c_capr    :: id -> CapRegFile    -- capability register file
}

component capcause :: CapCause
{
   value = c_capcause(procID)
   assign value = c_capcause(procID) <- value
}

component PCC :: Capability
{
   value = c_pcc(procID)
   assign value = { c_pcc(procID) <- value
                    ; mark (2, cpp_write (value))}

}

component CAPR (n::reg) :: Capability
{
   value = { m = c_capr(procID); m(n) }
   assign value = { var m = c_capr(procID)
                  ; m(n) <- value
                  ; c_capr(procID) <- m
                  ; mark (2, creg_write (n, value))}
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

unit dumpCRegs () = 
{
    mark(0, "======   Registers   ======")
  ; mark(0, "Core = ":[[procID]::nat])
  ; mark(0, "DEBUG CAP PCC ":cap_write(PCC))
  ; for i in 0 .. 31 do
      mark(0, "DEBUG CAP REG          ":(if i<10 then " " else ""):[[i]::nat]:" ":cap_write(CAPR([i])))
}
