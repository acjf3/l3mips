---------------------------------------------------------------------------
-- Coprocessor 1 initialisation
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

bool hasCP1 = true

unit COP1Init() =
{
    fcsr.FS <- false;
    fcsr.FCC <- 0;
    fcsr.ABS2008 <- false;
    fcsr.NAN2008 <- true;
    fcsr.CauseE <- false;
    fcsr.CauseV <- false;
    fcsr.CauseZ <- false;
    fcsr.CauseO <- false;
    fcsr.CauseU <- false;
    fcsr.CauseI <- false;
    fcsr.EnableV <- false;
    fcsr.EnableZ <- false;
    fcsr.EnableO <- false;
    fcsr.EnableU <- false;
    fcsr.EnableI <- false;
    fcsr.FlagV <- false;
    fcsr.FlagZ <- false;
    fcsr.FlagO <- false;
    fcsr.FlagU <- false;
    fcsr.FlagI <- false;
    fcsr.RM <- 0;

    fir.F64 <- true;
    fir.L <- true;
    fir.W <- true;
    fir.ASE <- false;
    fir.PS <- false;
    fir.D <- true;
    fir.S <- true;
    fir.PrID <- 0;
    fir.Rev <- 0
}
