---------------------------------------------------------------------------
-- CHERI state components
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

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

unit dumpCRegs () =
{
    mark_log (0, "======   Registers   ======")
  ; mark_log (0, "Core = " : [[procID]::nat])
  ; mark_log (0, "DEBUG CAP PCC " : log_cap_write(c_pcc(procID)))
  ; m = c_capr(procID)
  ; for i in 0 .. 31 do
    mark_log (0, "DEBUG CAP REG          " :
        (if i<10 then " " else "") : [[i]::nat] :
        " " : log_cap_write(m([i])))
}

component capcause :: CapCause
{
   value = c_capcause(procID)
   assign value = c_capcause(procID) <- value
}

component PCC :: Capability
{
    value = c_pcc(procID)
    assign value =
    {
        c_pcc(procID) <- value;
        mark_log (2, log_cpp_write (value))
    }
}

component CAPR (n::reg) :: Capability
{
    value = { m = c_capr(procID); m(n) }
    assign value =
    {
        var m = c_capr(procID);
        m(n) <- value;
        c_capr(procID) <- m;
        mark_log (2, log_creg_write (n, value))
    }
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

type CapAddr = bits(35)
