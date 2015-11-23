unit CP1Unusable = when not CP0.Status.CU1 do SignalCP1UnusableException

define COP1 () = CP1Unusable

