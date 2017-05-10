---------------------------------------------------------------------------
-- Model of the floating point instructions in the MIPS-III ISA.
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- This model leaves out several features of the MIPS floating point ISA:
--
-- (a) Enabling floating point traps is not supported. (BERI doesnt
--     implement this either).
-- (b) In the MIPS ISA, the quiet/signalling bit of NaN values has the
--     opposite sense to the IEEE 754:2008 standard. This isnt included
--     in this model.
-- (c) In the MIPS ISA, denormalized results are flushed to zero. This isnt
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
-- Post-processing after a floating point operation, including flushing
-- denormalized results to zero.
-----------------------------------
word PostOpF32(v::word) =
    if fcsr.FS and FP32_IsSubnormal(v) then
        0`word
    else
        v

dword PostOpF64(v::dword) =
    if fcsr.FS and FP64_IsSubnormal(v) then
        0`dword
    else
        v

-----------------------------------
-- Unordered floating point comparison
-----------------------------------
bool FP64_Unordered(a::dword, b::dword) =
    FP64_IsNan(a) or FP64_IsNan(b)

bool FP32_Unordered(a::word, b::word) =
    FP32_IsNan(a) or FP32_IsNan(b)

-----------------------------------
-- Current rounding mode
-----------------------------------
ieee_rounding Rounding_Mode =
    match fcsr.RM
    {
        case 0 => roundTiesToEven
        case 1 => roundTowardZero
        case 2 => roundTowardPositive
        case 3 => roundTowardNegative
    }

-----------------------------------
-- ABS.D fd, fs
-----------------------------------
define COP1 > ABS_D (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.ABS2008 then
        -- The IEEE 754:2008 version of ABS is non-arithmetic, and
        -- so should not do post-processing such as flush to zero.
        FGR(fd) <- FP64_Abs(FGR(fs))
    else
        FGR(fd) <- PostOpF64(FP64_Abs1985(FGR(fs)))

-----------------------------------
-- ABS.S fd, fs
-----------------------------------
define COP1 > ABS_S (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.ABS2008 then
        -- The IEEE 754:2008 version of ABS is non-arithmetic, and
        -- so should not do post-processing such as flush to zero.
        FGR(fd) <- SignExtend(FP32_Abs(FGR(fs)<31:0>))
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Abs1985(FGR(fs)<31:0>)))

-----------------------------------
-- ADD.D fd, fs, ft
-----------------------------------
define COP1 > ADD_D (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Add(Rounding_Mode, FGR(fs), FGR(ft)))

-----------------------------------
-- ADD.S fd, fs, ft
-----------------------------------
define COP1 > ADD_S (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Add(Rounding_Mode,
            FGR(fs)<31:0>, FGR(ft)<31:0>)))

-----------------------------------
-- BC1F offset
-----------------------------------
define COP1 > BC1F(i::bits(16), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if not fcsr.FCC<[cc]> then
        BranchTo <- Some (PC + 4 + SignExtend (i) << 2)
    else
        CheckBranch

-----------------------------------
-- BC1FL offset
-----------------------------------
define COP1 > BC1FL(i::bits(16), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if not fcsr.FCC<[cc]> then
        BranchTo <- Some (PC + 4 + SignExtend (i) << 2)
    else
    {
        CheckBranch;
        PC <- PC + 4
    }

-----------------------------------
-- BC1T offset
-----------------------------------
define COP1 > BC1T(i::bits(16), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.FCC<[cc]> then
        BranchTo <- Some (PC + 4 + SignExtend (i) << 2)
    else
        CheckBranch

-----------------------------------
-- BC1TL offset
-----------------------------------
define COP1 > BC1TL(i::bits(16), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.FCC<[cc]> then
        BranchTo <- Some (PC + 4 + SignExtend (i) << 2)
    else
    {
        CheckBranch;
        PC <- PC + 4
    }

--------------------------------
-- C.cond.D fs, ft
-----------------------------------
define COP1 > C_cond_D(fs::reg, ft::reg, cnd::bits(3), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        fcsr.FCC<[cc]> <-
        match cnd
        {
         case 0 {- F   -} => false
         case 1 {- UN  -} => FP64_Unordered(FGR(fs), FGR(ft))
         case 2 {- EQ  -} => FP64_Equal(FGR(fs), FGR(ft))
         case 3 {- UEQ -} => FP64_Equal(FGR(fs), FGR(ft)) or
                             FP64_Unordered(FGR(fs), FGR(ft))
         case 4 {- OLT -} => FP64_LessThan(FGR(fs), FGR(ft))
         case 5 {- ULT -} => not FP64_GreaterEqual(FGR(fs), FGR(ft))
         case 6 {- OLE -} => FP64_LessEqual(FGR(fs), FGR(ft))
         case 7 {- ULE -} => not FP64_GreaterThan(FGR(fs), FGR(ft))
        }

--------------------------------
-- C.cond.S fs, ft
-----------------------------------
define COP1 > C_cond_S(fs::reg, ft::reg, cnd::bits(3), cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        fcsr.FCC<[cc]> <-
        match cnd
        {
         case 0 {- F   -} => false
         case 1 {- UN  -} => FP32_Unordered(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 2 {- EQ  -} => FP32_Equal(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 3 {- UEQ -} => FP32_Equal(FGR(fs)<31:0>, FGR(ft)<31:0>) or
                             FP32_Unordered(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 4 {- OLT -} => FP32_LessThan(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 5 {- ULT -} => not FP32_GreaterEqual(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 6 {- OLE -} => FP32_LessEqual(FGR(fs)<31:0>, FGR(ft)<31:0>)
         case 7 {- ULE -} => not FP32_GreaterThan(FGR(fs)<31:0>, FGR(ft)<31:0>)
        }

-----------------------------------
-- CEIL.L.D fd, fs
-----------------------------------
define COP1 > CEIL_L_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardPositive, FGR(fs))
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- CEIL.L.S fd, fs
-----------------------------------
define COP1 > CEIL_L_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardPositive, FGR(fs)<31:0>)
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- CEIL.W.D fd, fs
-----------------------------------
define COP1 > CEIL_W_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardPositive, FGR(fs))
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- CEIL.W.S fd, fs
-----------------------------------
define COP1 > CEIL_W_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardPositive, FGR(fs)<31:0>)
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- CVT.D.L fd, fs
-----------------------------------
define COP1 > CVT_D_L(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- FP64_FromInt(Rounding_Mode, [FGR(fs)]::int)

-----------------------------------
-- CVT.D.S fd, fs
-----------------------------------
define COP1 > CVT_D_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- FP32_ToFP64(FGR(fs)<31:0>)

-----------------------------------
-- CVT.D.W fd, fs
-----------------------------------
define COP1 > CVT_D_W(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        when NotWordValue (FGR(fs))
            do #UNPREDICTABLE("CVT.D.W: NotWordValue");

        FGR(fd) <- FP64_FromInt(Rounding_Mode, [FGR(fs)<31:0>]::int)
    }

-----------------------------------
-- CVT.L.D fd, fs
-----------------------------------
define COP1 > CVT_L_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(Rounding_Mode, FGR(fs))
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- CVT.L.S fd, fs
-----------------------------------
define COP1 > CVT_L_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(Rounding_Mode, FGR(fs)<31:0>)
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- CVT.S.D fd, fs
-----------------------------------
define COP1 > CVT_S_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(FP64_ToFP32(Rounding_Mode, FGR(fs)))

-----------------------------------
-- CVT.S.L fd, fs
-----------------------------------
define COP1 > CVT_S_L(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(FP32_FromInt(Rounding_Mode, [FGR(fs)]::int))

-----------------------------------
-- CVT.S.W fd, fs
-----------------------------------
define COP1 > CVT_S_W(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        when NotWordValue (FGR(fs))
            do #UNPREDICTABLE("CVT.S.W: NotWordValue");

        FGR(fd) <- SignExtend(FP32_FromInt(Rounding_Mode,
            [FGR(fs)<31:0>]::int))
    }

-----------------------------------
-- CVT.W.D fd, fs
-----------------------------------
define COP1 > CVT_W_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(Rounding_Mode, FGR(fs))
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- CVT.W.S fd, fs
-----------------------------------
define COP1 > CVT_W_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(Rounding_Mode, FGR(fs)<31:0>)
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- DIV.D fd, fs, ft
-----------------------------------
define COP1 > DIV_D (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Div(Rounding_Mode, FGR(fs), FGR(ft)))

-----------------------------------
-- DIV.S fd, fs, ft
-----------------------------------
define COP1 > DIV_S (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Div(Rounding_Mode,
            FGR(fs)<31:0>, FGR(ft)<31:0>)))

-----------------------------------
-- FLOOR.L.D fd, fs
-----------------------------------
define COP1 > FLOOR_L_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardNegative, FGR(fs))
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- FLOOR.L.S fd, fs
-----------------------------------
define COP1 > FLOOR_L_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardNegative, FGR(fs)<31:0>)
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- FLOOR.W.D fd, fs
-----------------------------------
define COP1 > FLOOR_W_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardNegative, FGR(fs))
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- FLOOR.W.S fd, fs
-----------------------------------
define COP1 > FLOOR_W_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardNegative, FGR(fs)<31:0>)
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- LDC1 ft, offset(base)
-----------------------------------
define COP1 > LDC1 (ft::reg, offset::bits(16), base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(SignExtend (offset) + GPR(base));
        memdoubleword =
            LoadMemory (DOUBLEWORD, DOUBLEWORD, true, vAddr, false);
        when not exceptionSignalled do FGR(ft) <- memdoubleword
    }

-----------------------------------
-- LDXC1 fd, index(base)
-----------------------------------
define COP1 > LDXC1 (fd::reg, index::reg, base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(GPR(index) + GPR(base));
        memdoubleword =
            LoadMemory (DOUBLEWORD, DOUBLEWORD, true, vAddr, false);
        when not exceptionSignalled do FGR(fd) <- memdoubleword
    }

-----------------------------------
-- LWC1 ft, offset(base)
-----------------------------------
define COP1 > LWC1 (ft::reg, offset::bits(16), base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(SignExtend (offset) + GPR(base));
        memdoubleword = LoadMemory (WORD, WORD, true, vAddr, false);
        when not exceptionSignalled do
        {
            byte = vAddr<2:0> ?? (BigEndianCPU : '00');
            memword`32 = memdoubleword <31 + 8 * [byte] : 8 * [byte]>;
            -- The upper 32 bits are UNDEFINED in the MIPS ISA
            FGR(ft) <- SignExtend(memword)
        }
    }

-----------------------------------
-- LWXC1 ft, index(base)
-----------------------------------
define COP1 > LWXC1 (ft::reg, index::reg, base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(GPR(index) + GPR(base));
        memdoubleword = LoadMemory (WORD, WORD, true, vAddr, false);
        when not exceptionSignalled do
        {
            byte = vAddr<2:0> ?? (BigEndianCPU : '00');
            memword`32 = memdoubleword <31 + 8 * [byte] : 8 * [byte]>;
            -- The upper 32 bits are UNDEFINED in the MIPS ISA
            FGR(ft) <- SignExtend(memword)
        }
    }

-----------------------------------
-- MADD.D fd, fr, fs, ft (MIPS IV)
-----------------------------------
define COP1 > MADD_D (fd::reg, fr::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Add(Rounding_Mode,
            PostOpF64(FP64_Mul(Rounding_Mode, FGR(fs), FGR(ft))), FGR(fr)))

-----------------------------------
-- MADD.S fd, fr, fs, ft (MIPS IV)
-----------------------------------
define COP1 > MADD_S (fd::reg, fr::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Add(Rounding_Mode,
            PostOpF32(FP32_Mul(Rounding_Mode, FGR(fs)<31:0>,
                FGR(ft)<31:0>)), FGR(fr)<31:0>)))

-----------------------------------
-- MOV.D fd, fs
-----------------------------------
define COP1 > MOV_D (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- FGR(fs)

-----------------------------------
-- MOV.S fd, fs
-----------------------------------
define COP1 > MOV_S (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        -- In the MIPS ISA, the upper bits of the result are UNDEFINED
        FGR(fd) <- SignExtend(FGR(fs)<31:0>)

-----------------------------------
-- MOVF rd, rs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVF(rd::reg, rs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if not fcsr.FCC<[cc]::nat> then
        GPR(rd) <- GPR(rs)
    else
        nothing

-----------------------------------
-- MOVF.D fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVF_D(fd::reg, fs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if not fcsr.FCC<[cc]::nat> then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVF.S fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVF_S(fd::reg, fs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if not fcsr.FCC<[cc]::nat> then
        FGR(fd) <- SignExtend(FGR(fs)<31:0>)
    else
        nothing

-----------------------------------
-- MOVN.D fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVN_D(fd::reg, fs::reg, rt::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if GPR(rt) <> 0 then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVN.S fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVN_S(fd::reg, fs::reg, rt::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if GPR(rt) <> 0 then
        FGR(fd) <- SignExtend(FGR(fs)<31:0>)
    else
        nothing

-----------------------------------
-- MOVT rd, rs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVT(rd::reg, rs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.FCC<[cc]::nat> then
        GPR(rd) <- GPR(rs)
    else
        nothing

-----------------------------------
-- MOVT.D fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVT_D(fd::reg, fs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.FCC<[cc]::nat> then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVT.S fd, fs, cc (MIPS IV)
-----------------------------------
define COP1 > MOVT_S(fd::reg, fs::reg, cc::bits(3)) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.FCC<[cc]::nat> then
        FGR(fd) <- SignExtend(FGR(fs)<31:0>)
    else
        nothing

-----------------------------------
-- MOVZ.D fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVZ_D(fd::reg, fs::reg, rt::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if GPR(rt) == 0 then
        FGR(fd) <- FGR(fs)
    else
        nothing

-----------------------------------
-- MOVZ.S fd, fs, rt (MIPS IV)
-----------------------------------
define COP1 > MOVZ_S(fd::reg, fs::reg, rt::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if GPR(rt) == 0 then
        FGR(fd) <- SignExtend(FGR(fs)<31:0>)
    else
        nothing

-----------------------------------
-- MSUB.D fd, fr, fs, ft (MIPS IV)
-----------------------------------
define COP1 > MSUB_D (fd::reg, fr::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else FGR(fd) <- PostOpF64(FP64_Sub(Rounding_Mode,
        PostOpF64(FP64_Mul(Rounding_Mode, FGR(fs), FGR(ft))), FGR(fr)))

-----------------------------------
-- MSUB.S fd, fr, fs, ft (MIPS IV)
-----------------------------------
define COP1 > MSUB_S (fd::reg, fr::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Sub(Rounding_Mode,
            PostOpF32(FP32_Mul(Rounding_Mode, FGR(fs)<31:0>,
            FGR(ft)<31:0>)), FGR(fr)<31:0>)))

-----------------------------------
-- MUL.D fd, fs, ft
-----------------------------------
define COP1 > MUL_D (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Mul(Rounding_Mode, FGR(fs), FGR(ft)))

-----------------------------------
-- MUL.S fd, fs, ft
-----------------------------------
define COP1 > MUL_S (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Mul(Rounding_Mode,
            FGR(fs)<31:0>, FGR(ft)<31:0>)))

-----------------------------------
-- NEG.D fd, fs
-----------------------------------
define COP1 > NEG_D (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.ABS2008 then
        -- The IEEE 754:2008 version of NEG is non-arithmetic, and so
        -- should not do post-processing such as flush to zero.
        FGR(fd) <- FP64_Neg(FGR(fs))
    else
        FGR(fd) <- PostOpF64(FP64_Neg1985(FGR(fs)))

-----------------------------------
-- NEG.S fd, fs
-----------------------------------
define COP1 > NEG_S (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else if fcsr.ABS2008 then
        -- The IEEE 754:2008 version of NEG is non-arithmetic, and so
        -- should not do post-processing such as flush to zero.
        FGR(fd) <- SignExtend(FP32_Neg(FGR(fs)<31:0>))
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Neg1985(FGR(fs)<31:0>)))

-----------------------------------
-- ROUND.L.D fd, fs
-----------------------------------
define COP1 > ROUND_L_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTiesToEven, FGR(fs))
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- ROUND.L.S fd, fs
-----------------------------------
define COP1 > ROUND_L_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTiesToEven, FGR(fs)<31:0>)
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- ROUND.W.D fd, fs
-----------------------------------
define COP1 > ROUND_W_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTiesToEven, FGR(fs))
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- ROUND.W.S fd, fs
-----------------------------------
define COP1 > ROUND_W_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTiesToEven, FGR(fs)<31:0>)
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- SDC1 ft, offset(base)
-----------------------------------
define COP1 > SDC1 (ft::reg, offset::bits(16), base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(SignExtend (offset) + GPR(base));
        datadoubleword = FGR(ft);
        _ = StoreMemory(DOUBLEWORD, DOUBLEWORD, true, datadoubleword, vAddr, false)
    }

-----------------------------------
-- SDXC1 fs, index(base)
-----------------------------------
define COP1 > SDXC1 (fs::reg, index::reg, base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(GPR(index) + GPR(base));
        datadoubleword = FGR(fs);
        _ = StoreMemory(DOUBLEWORD, DOUBLEWORD, true, datadoubleword, vAddr, false)
    }

-----------------------------------
-- SQRT.D fd, fs, ft
-----------------------------------
define COP1 > SQRT_D (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Sqrt(Rounding_Mode, FGR(fs)))

-----------------------------------
-- SQRT.S fd, fs, ft
-----------------------------------
define COP1 > SQRT_S (fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Sqrt(Rounding_Mode,
            FGR(fs)<31:0>)))

-----------------------------------
-- SUB.D fd, fs, ft
-----------------------------------
define COP1 > SUB_D (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- PostOpF64(FP64_Sub(Rounding_Mode, FGR(fs), FGR(ft)))

-----------------------------------
-- SUB.S fd, fs, ft
-----------------------------------
define COP1 > SUB_S (fd::reg, fs::reg, ft::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- SignExtend(PostOpF32(FP32_Sub(Rounding_Mode,
            FGR(fs)<31:0>, FGR(ft)<31:0>)))

-----------------------------------
-- SWC1 ft, offset(base)
-----------------------------------
define COP1 > SWC1 (ft::reg, offset::bits(16), base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(SignExtend (offset) + GPR(base));
        bytesel = vAddr<2:0> ?? (BigEndianCPU : '00');
        datadoubleword = FGR(ft) << (0n8 * [bytesel]);
        _ = StoreMemory (WORD, WORD, true, datadoubleword, vAddr, false)
    }

-----------------------------------
-- SWXC1 ft, index(base)
-----------------------------------
define COP1 > SWXC1 (ft::reg, index::reg, base::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
    {
        vAddr = getVirtualAddress(GPR (index) + GPR(base));
        bytesel = vAddr<2:0> ?? (BigEndianCPU : '00');
        datadoubleword = FGR(ft) << (0n8 * [bytesel]);
        _ = StoreMemory (WORD, WORD, true, datadoubleword, vAddr, false)
    }

-----------------------------------
-- TRUNC.L.D fd, fs
-----------------------------------
define COP1 > TRUNC_L_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardZero, FGR(fs))
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- TRUNC.L.S fd, fs
-----------------------------------
define COP1 > TRUNC_L_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardZero, FGR(fs)<31:0>)
        {
            case Some(x) => IntToDWordMIPS(x)
            case None => 0x7fffffffffffffff`64
        }

-----------------------------------
-- TRUNC.W.D fd, fs
-----------------------------------
define COP1 > TRUNC_W_D(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP64_ToInt(roundTowardZero, FGR(fs))
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- TRUNC.W.S fd, fs
-----------------------------------
define COP1 > TRUNC_W_S(fd::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fd) <- match FP32_ToInt(roundTowardZero, FGR(fs)<31:0>)
        {
            case Some(x) => SignExtend(IntToWordMIPS(x))
            case None => 0x7fffffff`64
        }

-----------------------------------
-- DMFC1 rt, fs
-----------------------------------
define COP1 > DMFC1 (rt::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        GPR(rt) <- FGR(fs)

-----------------------------------
-- DMTC1 rt, fs
-----------------------------------
define COP1 > DMTC1 (rt::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fs) <- GPR(rt)

-----------------------------------
-- MFC1 rt, fs
-----------------------------------
define COP1 > MFC1 (rt::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        -- In the MIPS ISA, the upper 32 bits of FGR(fs) are UNDEFINED.
        GPR(rt) <- SignExtend(FGR(fs)<31:0>)

-----------------------------------
-- MTC1 rt, fs
-----------------------------------
define COP1 > MTC1 (rt::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        FGR(fs) <- SignExtend(GPR(rt)<31:0>)

-----------------------------------
-- CFC1 rt, fs
-----------------------------------
define COP1 > CFC1(rt::reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        GPR(rt) <- match fs
        {
            -- Floating Point Condition Codes Register (not in MIPS III)
            case 0  => SignExtend(&fir)
            case 25 => ZeroExtend(fcsr.FCC)
            case 31 => SignExtend(&fcsr)
            case _  => #UNPREDICTABLE("Unsupported floating point control register")
        }

-----------------------------------
-- CTC1 rt, fs
-----------------------------------
define COP1 > CTC1(rt:: reg, fs::reg) =
    if not CP0.Status.CU1 then
        SignalCP1UnusableException
    else
        match (fs)
        {
            -- Floating Point Implementation Register
            -- This is a read-only register; spec is ambiguious about
            -- whether a write to it is a NOP or is UNPREDICTABLE.
            case 0 => nothing
            -- Floating Point Condition Codes Register (not in MIPS III)
            case 25 => fcsr.FCC <- GPR(rt)<7:0>
            case 31 =>
            {
                &fcsr <- GPR(rt)<31:0>;

                -- ABS2008 is read-only in some versions of the MIPS ISA,
                -- read-write in others. We treat it as read-write.

                -- NAN2008 is read-only in some versions of the MIPS ISA,
                -- read-write in others. We don't implement the legacy NaN
                -- encoding, so we treat it as read-only.
                fcsr.NAN2008 <- true
            }
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

