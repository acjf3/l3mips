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


---------------------------------------------------------------------------
-- The MIPS rule for casting a finite IEEE float to a fixed point integer
-- is that out of range values become MAXINT.
--
-- The Java language rule is that positive out of range values become
-- MAXINT, negative out of range values become MININT.
-- 
-- Later versions of the MIPS ISA provide Java-compatible behaviour if
-- fcsr.NAN2008 is set.
---------------------------------------------------------------------------

word IntToWordMIPS(v::int) =
    if v > 0x7fffffff then
        0x7fffffff`32
    else if v < -0x80000000 then
        0x7fffffff`32
    else
        [v]`word

dword IntToDWordMIPS(v::int) =
    if v > 0x7fffffffffffffff then
        0x7fffffffffffffff`64
    else if v < -0x8000000000000000 then
        0x7fffffffffffffff`64
    else
        [v]`dword

-----------------------------------
-- Unordered floating point comparison
-----------------------------------
bool FP64_Unordered(a::dword, b::dword) =
    FP64_IsNan(a) or FP64_IsNan(b)

-----------------------------------
-- ABS.D fd, fs
-----------------------------------
define COP1 > ABS_D (fd::reg, fs::reg) =
    if fcsr.ABS2008 then
        FGR(fd) <- FP64_Abs(FGR(fs))
    else
        FGR(fd) <- FP64_Abs1985(FGR(fs))

-----------------------------------
-- ADD.D fd, fs, ft
-----------------------------------
define COP1 > ADD_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Add(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- C.F.D fd, fs
-----------------------------------
define COP1 > C_F_D(ft::reg, fs::reg) =
    fcsr.C <- false

-----------------------------------
-- C.UN.D fd, fs
-----------------------------------
define COP1 > C_UN_D(ft::reg, fs::reg) =
    fcsr.C <- FP64_Unordered(FGR(ft), FGR(fs))

-----------------------------------
-- C.EQ.D fd, fs
-----------------------------------
define COP1 > C_EQ_D(ft::reg, fs::reg) =
    fcsr.C <- FP64_Equal(FGR(ft), FGR(fs))

-----------------------------------
-- C.UEQ.D fd, fs
-----------------------------------
define COP1 > C_UEQ_D(ft::reg, fs::reg) =
    fcsr.C <- FP64_Equal(FGR(ft), FGR(fs)) or FP64_Unordered(FGR(ft), FGR(fs))

-----------------------------------
-- C.OLT.D fd, fs
-----------------------------------
define COP1 > C_OLT_D(ft::reg, fs::reg) =
    fcsr.C <- FP64_LessThan(FGR(ft), FGR(fs))

-----------------------------------
-- C.ULT.D fd, fs
-----------------------------------
define COP1 > C_ULT_D(ft::reg, fs::reg) =
    fcsr.C <- not FP64_GreaterEqual(FGR(ft), FGR(fs))

-----------------------------------
-- C.OLE.D fd, fs
-----------------------------------
define COP1 > C_OLE_D(ft::reg, fs::reg) =
    fcsr.C <- FP64_LessEqual(FGR(ft), FGR(fs))

-----------------------------------
-- C.ULE.D fd, fs
-----------------------------------
define COP1 > C_ULE_D(ft::reg, fs::reg) =
    fcsr.C <- not FP64_GreaterThan(FGR(ft), FGR(fs))

-----------------------------------
-- C.EQ.S fd, fs
-----------------------------------
define COP1 > C_EQ_S(ft::reg, fs::reg) =
    nothing -- XXX: TO DO

-----------------------------------
-- CEIL.L.D fd, fs
-----------------------------------
define COP1 > CEIL_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardPositive, FGR(fs))
    {
        case Some(x) => IntToDWordMIPS(x)
        case None => 0x7fffffffffffffff`64
    }

-----------------------------------
-- CEIL.W.D fd, fs
-----------------------------------
define COP1 > CEIL_W_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardPositive, FGR(fs))
    {
        case Some(x) => SignExtend(IntToWordMIPS(x))
        case None => 0x7fffffff`64
    }

-----------------------------------
-- CVT.D.L fd, fs
-----------------------------------
define COP1 > CVT_D_L(fd::reg, fs::reg) =
    FGR(fd) <- FP64_FromInt(roundTiesToEven, [FGR(fs)]::int)

-----------------------------------
-- CVT.D.S fd, fs
-----------------------------------
define COP1 > CVT_D_S(fd::reg, fs::reg) =
    FGR(fd) <- FP32_ToFP64(FGR(fs)<31:0>)

-----------------------------------
-- CVT.D.W fd, fs
-----------------------------------
define COP1 > CVT_D_W(fd::reg, fs::reg) =
{
    when NotWordValue (FGR(fs))
        do #UNPREDICTABLE("CVT.D.W: NotWordValue");

    FGR(fd) <- FP64_FromInt(roundTiesToEven, [FGR(fs)<31:0>]::int)
}

-----------------------------------
-- CVT.S.D fd, fs
-----------------------------------
define COP1 > CVT_S_D(fd::reg, fs::reg) =
    FGR(fd) <- SignExtend(FP64_ToFP32(roundTiesToEven, FGR(fs)))

-----------------------------------
-- CVT.S.W fd, fs
-----------------------------------
define COP1 > CVT_S_W(fd::reg, fs::reg) =
    nothing -- XXX: TO DO

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
        case Some(x) => IntToDWordMIPS(x)
        case None => 0x7fffffffffffffff`64
    }

-----------------------------------
-- FLOOR.W.D fd, fs
-----------------------------------
define COP1 > FLOOR_W_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardNegative, FGR(fs))
    {
        case Some(x) => SignExtend(IntToWordMIPS(x))
        case None => 0x7fffffff`64
    }

-----------------------------------
-- MOV.D fd, fs
-----------------------------------
define COP1 > MOV_D (fd::reg, fs::reg) =
    FGR(fd) <- FGR(fs)

-----------------------------------
-- MOVF.D fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVF_D(fd::reg, fs::reg) =
    if not fcsr.C then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVN.D fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVN_D(fd::reg, fs::reg, rt::reg) =
    if GPR(rt) <> 0 then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVT.D fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVT_D(fd::reg, fs::reg) =
    if fcsr.C then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVZ.D fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVZ_D(fd::reg, fs::reg, rt::reg) =
    if GPR(rt) == 0 then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MUL.D fd, fs, ft
-----------------------------------
define COP1 > MUL_D (fd::reg, fs::reg, ft::reg) =
    FGR(fd) <- FP64_Mul(roundTiesToEven, FGR(fs), FGR(ft))

-----------------------------------
-- NEG.D fd, fs
-----------------------------------
define COP1 > NEG_D (fd::reg, fs::reg) =
    if fcsr.ABS2008 then
        FGR(fd) <- FP64_Neg(FGR(fs))
    else
        FGR(fd) <- FP64_Neg1985(FGR(fs))

-----------------------------------
-- ROUND.L.D fd, fs
-----------------------------------
define COP1 > ROUND_L_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTiesToEven, FGR(fs))
    {
        case Some(x) => IntToDWordMIPS(x)
        case None => 0x7fffffffffffffff`64
    }

-----------------------------------
-- ROUND.W.D fd, fs
-----------------------------------
define COP1 > ROUND_W_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTiesToEven, FGR(fs))
    {
        case Some(x) => SignExtend(IntToWordMIPS(x))
        case None => 0x7fffffff`64
    }

-----------------------------------
-- SQRT.D fd, fs, ft
-----------------------------------
define COP1 > SQRT_D (fd::reg, fs::reg) =
    FGR(fd) <- FP64_Sqrt(roundTiesToEven, FGR(fs))

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
        case Some(x) => IntToDWordMIPS(x)
        case None => 0x7fffffffffffffff`64
    }

-----------------------------------
-- TRUNC.W.D fd, fs
-----------------------------------
define COP1 > TRUNC_W_D(fd::reg, fs::reg) =
    FGR(fd) <- match FP64_ToInt(roundTowardZero, FGR(fs))
    {
        case Some(x) => SignExtend(IntToWordMIPS(x))
        case None => 0x7fffffff`64
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
    -- In the MIPS ISA, the upper 32 bits of FGR(fs) are UNDEFINED.
    GPR(rt) <- SignExtend(FGR(fs)<31:0>)

-----------------------------------
-- MTC1 rt, fs
-----------------------------------
define COP1 > MTC1 (rt::reg, fs::reg) =
    FGR(fs) <- SignExtend(GPR(rt)<31:0>)

-----------------------------------
-- CFC1 rt, fs
-----------------------------------
define COP1 > CFC1(rt::reg, fs::reg) =
    GPR(rt) <- match fs
    {
        -- Floating Point Condition Codes Register (not in MIPS III)
        case 25 => ZeroExtend([fcsr.C]`1)
        case 31 => SignExtend(&fcsr)
        case _  => #UNPREDICTABLE("Unsupported floating point control register")
    }

-----------------------------------
-- CTC1 rt, fs
-----------------------------------
define COP1 > CTC1(rt:: reg, fs::reg) = 
    match (fs)
    {
        case 31 => &fcsr <- GPR(rt)<31:0>
        case _  => #UNPREDICTABLE("Unsupported floating point control register")
    }

-------------------------------------------------------------
-- Unknown Floating Point Instruction.
-------------------------------------------------------------
define COP1 > UnknownFPInstruction = 
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        SignalException(ResI)

