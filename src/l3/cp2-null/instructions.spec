---------------------------------------------------------------------------
-- Place holders for coprocessor 2 instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

unit CP2Unusable = when not CP0.Status.CU2 do SignalCP2UnusableException

define COP2 () = CP2Unusable
define LWC2 () = CP2Unusable
define LDC2 () = CP2Unusable
define SWC2 () = CP2Unusable
define SDC2 () = CP2Unusable
