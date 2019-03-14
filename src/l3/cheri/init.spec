---------------------------------------------------------------------------
-- CHERI capability coprocessor initialisation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

unit COP2Init () =
{
    hasCP2 <- true;
    CP0.Config1.C2 <- true;
    var defaultCapCause :: CapCause;
    defaultCapCause.ExcCode <- 0;
    defaultCapCause.RegNum <- 0;
    capcause <- defaultCapCause;
    for i in 0 .. 31 do CAPR([i]) <- nullCap;
    for i in 0 .. 31 do SCAPR([i]) <- nullCap;
    PCC <- defaultCap;
    KCC <- defaultCap;
    EPCC <- defaultCap;
    DDC <- defaultCap
}
