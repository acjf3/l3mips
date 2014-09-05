---------------------------------------------------------------------------
-- CHERI capability coprocessor initialisation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

unit COP2Init () = ()
{-
    capcause <- 0;
    var defaultCap :: Capability;
    defaultCap.tag <- 1;
    defaultCap.sealed <- 0;
    defaultCap.offset <- 0;
    defaultCap.base <- 0;
    defaultCap.lenght <- ~0;
    defaultCap.otype <- 0;
    defaultCap.perms <- ~0;
    defaultCap.reserved <- ~0;
    PCC <- defaultCap;
    for i in 0 .. 31 do CAPR([i]) <- defaultCap;
-}
