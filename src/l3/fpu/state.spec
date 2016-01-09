---------------------------------------------------------------------------
-- Internal state of the floating point unit.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

register FCSR :: word
{
    31-25,23: FCC    -- Condition codes
    24    : FS       -- Flush denormalized results to zero
    19    : ABS2008  -- ABS and NEG conform to IEEE 754:2008
    18    : NAN2008  -- NaN values conform to IEEE 754:2008
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

register FIR :: word
{
    22    : F64      -- Floating point registers are 64 bit
    21    : L        -- 64-bit fixed point type is implemented
    20    : W        -- 32-bit fixed point type is implemented
    19    : ASE      -- 3D ASE is implemented
    18    : PS       -- Paired single type is implemented
    17    : D        -- Double precision floating point is implemented
    16    : S        -- Single precision floating point is implemented
    15-8  : PrID     -- Processor ID
    7-0   : Rev      -- Revision
}

declare
{
    fgr :: reg -> bits(64)
    fcsr :: FCSR
    fir :: FIR
}

component FGR(n::reg)::dword
{
    value = fgr(n)
    assign value =
    {
        fgr(n) <- value;
        when 2 <= trace_level do mark_log (2, log_w_fgr (n, value))
    }
}
