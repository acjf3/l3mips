---------------------------------------------------------------------------
-- CHERI capability coprocessor initialisation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool hasCP2 = true

unit COP2Init () =
{
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
