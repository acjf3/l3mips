---------------------------------------------------------------------------
-- Internal state of the floating point unit.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

register FCSR :: word
{
    24    : FS       -- Flush denormalized results to zero (true)
    23    : C        -- Condition code
    19    : ABS2008  -- ABS and NEG conform to IEEE 754:2008 (false)
    18    : NAN2008  -- NaN values conform to IEEE 754:2008 (false)
    17    : CauseE   -- Unimplemented operation
    16    : CauseV   -- Invalid operation
    15    : CauseZ   -- Divide by zero
    14    : CauseO   -- Overflow
    13    : CauseU   -- Underflow
    12    : CauseI   -- Inexact
    11    : EnableV
    10    : EnableZ
    9     : EnableO
    8     : EnableU
    7     : EnableI
    6     : FlagV
    5     : FlagZ
    4     : FlagO
    3     : FlagU
    2     : FlagI
    1-0   : RM       -- Rounding mode (0, round to nearest tie even)
}


declare
{
    FGR :: reg -> bits(64)
    fcsr :: FCSR
}
