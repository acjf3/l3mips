---------------------------------------------------------------------------
-- Model of the floating point instructions in the MIPS-III ISA.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- This model leaves out several features of the MIPS floating point ISA:
--
-- (a) Enabling floating point traps is not supported. (BERI doesnt
--     implement this either).
-- (b) The only supported rounding mode is round to nearest, tie to even.
--     (BERI doesnt implement the other rounding modes either).
-- (c) In the MIPS ISA, the quiet/signalling bit of NaN values has the
--     opposite sense to the IEEE 754:2008 standard. This isnt included
--     in this model.
-- (d) In the MIPS ISA, denormalized results are flushed to zero. This isnt
--     included in this model.
---------------------------------------------------------------------------

-----------------------------------
-- ABS.D fd, fs
-----------------------------------
define COP1 > ABS_D (fd::reg, fs::reg) =
    FGR(fd) <- FP64_Abs1985(FGR(fs))

-----------------------------------
-- ADD.D fd, fs, ft
-----------------------------------
define COP1 > ADD_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Add(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- CEIL.L.D fd, fs
-----------------------------------
define COP1 > CEIL_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardPositive, FGR(fs))
    {
        case Some(x) => [x]`64
        case None => -1`64
    }

-----------------------------------
-- CVT.D fd, fs
-----------------------------------
define COP1 > CVT_D_L(fd::reg, fs::reg) =
    FGR(fd) <- FP64_FromInt(roundTiesToEven, [FGR(fs)]::int)

-----------------------------------
-- DIV.D fd, fs, ft
-----------------------------------
define COP1 > DIV_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Div(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- FLOOR.L.D fd, fs
-----------------------------------
define COP1 > FLOOR_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardNegative, FGR(fs))
    {
        case Some(x) => [x]`64
        case None => -1`64
    }

-----------------------------------
-- MOV.D fd, fs
-----------------------------------
define COP1 > MOV_D (fd::reg, fs::reg) =
    FGR(fd) <- FGR(fs)

-----------------------------------
-- MUL.D fd, fs, ft
-----------------------------------
define COP1 > MUL_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Mul(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- NEG.D fd, fs
-----------------------------------
define COP1 > NEG_D (fd::reg, fs::reg) =
    FGR(fd) <- FP64_Neg1985(FGR(fs))

-----------------------------------
-- ROUND.L.D fd, fs
-----------------------------------
define COP1 > ROUND_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTiesToEven, FGR(fs))
    {
        case Some(x) => [x]`64
        case None => -1`64
    }

-----------------------------------
-- SUB.D fd, fs, ft
-----------------------------------
define COP1 > SUB_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Sub(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- TRUNC.L.D fd, fs
-----------------------------------
define COP1 > TRUNC_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardZero, FGR(fs))
    {
        case Some(x) => [x]`64
        case None => 0x7fffffffffffffff`64
    }

-----------------------------------
-- DMFC1 rt, fs
-----------------------------------
define COP1 > DMFC1 (rt::reg, fs::reg) =
    GPR(rt) <- FGR(fs)


-----------------------------------
-- DMTC1 rt, fs
-----------------------------------
define COP1 > DMTC1 (rt::reg, fs::reg) =
    FGR(fs) <- GPR(rt)

-----------------------------------
-- MFC1 rt, fs
-----------------------------------
define COP1 > MFC1 (rt::reg, fs::reg) =
    nothing

-----------------------------------
-- MTC1 rt, fs
-----------------------------------
define COP1 > MTC1 (rt::reg, fs::reg) =
    nothing

-------------------------------------------------------------
-- Unknown Floating Point Instruction.
-------------------------------------------------------------

define COP1 > UnknownFPInstruction = 
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        SignalException(ResI)

