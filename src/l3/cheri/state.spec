---------------------------------------------------------------------------
-- CHERI state components
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

type CapRegFile = reg -> Capability

register CapCause :: bits (16)
{
    15-8 : ExcCode  -- 8 bits exception code
     7-0 : RegNum   -- 8 bits register number
}

declare
{
   -- State of all cores
   all_BranchDelayPCC   :: id -> (bits(64) * Capability) option
   all_BranchToPCC      :: id -> (bits(64) * Capability) option
   all_capcause         :: id -> CapCause    -- capability exception cause register
   all_pcc              :: id -> Capability  -- program counter capability
   all_capr             :: id -> CapRegFile  -- capability register file
   all_scapr            :: id -> CapRegFile  -- special capability register file

   -- State of current core
   BranchDelayPCC    :: (bits(64) * Capability) option
   BranchToPCC       :: (bits(64) * Capability) option
   capcause          :: CapCause          -- capability exception cause register
   c_pcc             :: Capability        -- program counter capability
   c_capr            :: CapRegFile        -- capability register file
   c_scapr           :: CapRegFile        -- special capability register file
}

unit switchCoreCAP (i::id) =
{
   all_BranchDelayPCC (procID) <- BranchDelayPCC;
   all_BranchToPCC (procID) <- BranchToPCC;
   all_capcause (procID) <- capcause;
   all_pcc (procID) <- c_pcc;
   all_capr (procID) <- c_capr;
   all_scapr (procID) <- c_scapr;
   BranchDelayPCC <- all_BranchDelayPCC (i);
   BranchToPCC <- all_BranchToPCC (i);
   capcause <- all_capcause (i);
   c_pcc <- all_pcc (i);
   c_capr <- all_capr (i);
   c_scapr <- all_scapr (i)
}

unit dumpCRegs () =
{
    mark_log (0, "======   Registers   ======")
  ; mark_log (0, "Core = " : [[procID]::nat])
  ; mark_log (0, "DEBUG CAP PCC   \t" : log_cap_write(c_pcc))
  ; for i in 0 .. 31 do
    mark_log (0, "DEBUG CAP REG " :
        (if i<10 then " " else "") : [[i]::nat] :
        "\t" : log_cap_write(c_capr([i])))
  ; for i in 0 .. 31 do
    mark_log (0, "DEBUG CAP HWREG " :
        (if i<10 then " " else "") : [[i]::nat] :
        "\t" : log_cap_write(c_scapr([i])))
}

component CAPR (n::reg) :: Capability
{
    value = if n == 0 then nullCap else c_capr(n)
    assign value =
    {
        c_capr(n) <- value;
        mark_log (2, log_creg_write (n, value))
    }
}

component SCAPR (n::reg) :: Capability
{
    value = c_scapr(n)
    assign value =
    {
        c_scapr(n) <- value;
        mark_log (2, "Special ":log_creg_write (n, value))
    }
}

component PCC :: Capability
{
    value = c_pcc
    assign value =
    {
        c_pcc <- value;
        mark_log (2, log_cpp_write (value))
    }
}

component RCC :: Capability
{
    value = CAPR(17)
    assign value = CAPR(17) <- value
}

component IDC :: Capability
{
    value = CAPR(26)
    assign value = CAPR(26) <- value
}

component DDC :: Capability
{
    value = SCAPR(0)
    assign value = SCAPR(0) <- value
}

component TLSC :: Capability
{
    value = SCAPR(1)
    assign value = SCAPR(1) <- value
}

component PTLSC :: Capability
{
    value = SCAPR(8)
    assign value = SCAPR(8) <- value
}

component KR1C :: Capability
{
    value = SCAPR(22)
    assign value = SCAPR(22) <- value
}

component KR2C :: Capability
{
    value = SCAPR(23)
    assign value = SCAPR(23) <- value
}

component KCC :: Capability
{
    value = SCAPR(29)
    assign value = SCAPR(29) <- value
}

component KDC :: Capability
{
    value = SCAPR(30)
    assign value = SCAPR(30) <- value
}

component EPCC :: Capability
{
    value = SCAPR(31)
    assign value = SCAPR(31) <- value
}
