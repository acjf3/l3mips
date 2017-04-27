---------------------------------------------------------------------------
-- Disassembler for MIPS ISA floating point instructions
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

string COP1InstructionToString (i::instruction) =
    match i
    {
        case COP1(j) =>
            match j
            {
                case ABS_D(fd, fs) => op2fpr("abs.d", fd, fs)
                case ABS_S(fd, fs) => op2fpr("abs.s", fd, fs)
                case ADD_D(fd, fs, ft) => op3fpr("add.d", fd, fs, ft)
                case ADD_S(fd, fs, ft) => op3fpr("add.s", fd, fs, ft)
                case BC1F(i) => op1i("bc1f", i)
                case BC1FL(i) => op1i("bc1fl", i)
                case BC1T(i) => op1i("bc1t", i)
                case BC1TL(i) => op1i("bc1tl", i)
                case C_F_D(fs, ft) => op2fpr("c.f.d", fs, ft)
                case C_F_S(fs, ft) => op2fpr("c.f.s", fs, ft)
                case C_UN_D(fs, ft) => op2fpr("c.un.d", fs, ft)
                case C_UN_S(fs, ft) => op2fpr("c.un.s", fs, ft)
                case C_EQ_D(fs, ft) => op2fpr("c.eq.d", fs, ft)
                case C_EQ_S(fs, ft) => op2fpr("c.eq.s", fs, ft)
                case C_UEQ_D(fs, ft) => op2fpr("c.ueq.d", fs, ft)
                case C_UEQ_S(fs, ft) => op2fpr("c.ueq.s", fs, ft)
                case C_OLT_D(fs, ft) => op2fpr("c.olt.d", fs, ft)
                case C_OLT_S(fs, ft) => op2fpr("c.olt.s", fs, ft)
                case C_ULT_D(fs, ft) => op2fpr("c.ult.d", fs, ft)
                case C_ULT_S(fs, ft) => op2fpr("c.ult.s", fs, ft)
                case C_OLE_D(fs, ft) => op2fpr("c.ole.d", fs, ft)
                case C_OLE_S(fs, ft) => op2fpr("c.ole.s", fs, ft)
                case C_ULE_D(fs, ft) => op2fpr("c.ule.d", fs, ft)
                case C_ULE_S(fs, ft) => op2fpr("c.ule.s", fs, ft)
                case CEIL_L_D(fd, fs) => op2fpr("ceil.l.d", fd, fs)
                case CEIL_L_S(fd, fs) => op2fpr("ceil.l.s", fd, fs)
                case CEIL_W_D(fd, fs) => op2fpr("ceil.w.d", fd, fs)
                case CEIL_W_S(fd, fs) => op2fpr("ceil.w.s", fd, fs)
                case CFC1(rt, fs) => op2rfpr("cfc1", rt, fs)
                case CTC1(rt, fs) => op2rfpr("ctc1", rt, fs)
                case CVT_D_L(fd, fs) => op2fpr("cvt.d.l", fd, fs)
                case CVT_D_S(fd, fs) => op2fpr("cvt.d.s", fd, fs)
                case CVT_D_W(fd, fs) => op2fpr("cvt.d.w", fd, fs)
                case CVT_L_D(fd, fs) => op2fpr("cvt.l.d", fd, fs)
                case CVT_L_S(fd, fs) => op2fpr("cvt.l.s", fd, fs)
                case CVT_S_L(fd, fs) => op2fpr("cvt.s.l", fd, fs)
                case CVT_S_D(fd, fs) => op2fpr("cvt.s.d", fd, fs)
                case CVT_S_W(fd, fs) => op2fpr("cvt.s.w", fd, fs)
                case CVT_W_D(fd, fs) => op2fpr("cvt.w.d", fd, fs)
                case CVT_W_S(fd, fs) => op2fpr("cvt,w,s", fd, fs)
                case DIV_D(fd, fs, ft) => op3fpr("div.d", fd, fs, ft)
                case DIV_S(fd, fs, ft) => op3fpr("div.s", fd, fs, ft)
                case DMFC1(rt, fs) => op2rfpr("dmfc1", rt, fs)
                case DMTC1(rt, fs) => op2rfpr("dmtc1", rt, fs)
                case FLOOR_L_D(fd, fs) => op2fpr("floor.l.d", fd, fs)
                case FLOOR_L_S(fd, fs) => op2fpr("floor.l.s", fd, fs)
                case FLOOR_W_D(fd, fs) => op2fpr("floor.w.d", fd, fs)
                case FLOOR_W_S(fd, fs) => op2fpr("floor.w.s", fd, fs)
                case LDC1(ft, offset, base) => opfpmem("ldc1", ft, base, offset)
                case LDXC1(fs, index, base) => opfpmem ("ldxc1", fs, base, index)
                case LWC1(ft, offset, base) => opfpmem("lwc1", ft, base, offset)
                case LWXC1(ft, index, base) => opfpmem("lwxc1", ft, base, index)
                case MFC1(rt, fs) => op2rfpr("mfc1", rt, fs)
                case MADD_D(fd, fr, fs, ft) => op4fpr("madd.d", fd, fr, fs, ft)
                case MADD_S(fd, fr, fs, ft) => op4fpr("madd.s", fd, fr, fs, ft)
                case MSUB_D(fd, fr, fs, ft) => op4fpr("msub.d", fd, fr, fs, ft)
                case MSUB_S(fd, fr, fs, ft) => op4fpr("msub.s", fd, fr, fs, ft)
                case MOV_D(fd, fs) => op2fpr("mov.d", fd, fs)
                case MOV_S(fd, fs) => op2fpr("mov.s", fd, fs)
                case MOVF(rd, rs, cc) => op2r("movf", rd, rs) : ", " : [cc]
                case MOVF_D(fd, fs, cc) => op2fpr("movf.d", fd, fs) : ", " : [cc]
                case MOVF_S(fd, fs, cc) => op2fpr("movf.s", fd, fs) : ", " : [cc]
                case MOVN_D(fd, fs, rt) => op3fpr("movn.d", fd, fs, rt)
                case MOVN_S(fd, fs, rt) => op3fpr("movn.s", fd, fs, rt)
                case MOVT(rd, rs, cc) => op2r("movt", rd, rs) : ", " : [cc]
                case MOVT_D(fd, fs, cc) => op2fpr("movt.d", fd, fs) : ", " : [cc]
                case MOVT_S(fd, fs, cc) => op2fpr("movt.s", fd, fs) : ", " : [cc]
                case MOVZ_D(fd, fs, rt) => op3fpr("movz.d", fd, fs, rt)
                case MOVZ_S(fd, fs, rt) => op3fpr("movz.s", fd, fs, rt)
                case MTC1(rt, fs) => op2rfpr("mtc1", rt, fs)
                case MUL_D(fd, fs, ft) => op3fpr("mul.d", fd, fs, ft)
                case MUL_S(fd, fs, ft) => op3fpr("mul.s", fd, fs, ft)
                case NEG_D(fd, fs) => op2fpr("neg.d", fd, fs)
                case NEG_S(fd, fs) => op2fpr("neg.s", fd, fs)
                case ROUND_L_D(fd, fs) => op2fpr("round.l.d", fd, fs)
                case ROUND_L_S(fd, fs) => op2fpr("round.l.s", fd, fs)
                case ROUND_W_D(fd, fs) => op2fpr("round.w.d", fd, fs)
                case ROUND_W_S(fd, fs) => op2fpr("round.w.s", fd, fs)
                case SDC1(ft, offset, base) => opfpmem("sdc1", ft, base, offset)
                case SDXC1(fs, index, base) => opfpmem("sdxc1", fs, base, index)
                case SWC1(ft, offset, base) => opfpmem("swc1", ft, base, offset)
                case SWXC1(ft, index, base) => opfpmem("swxc1", ft, base, index)
                case SUB_D(fd, fs, ft) => op3fpr("sub.d", fd, fs, ft)
                case SUB_S(fd, fs, ft) => op3fpr("sub.s", fd, fs, ft)
                case SQRT_D(fd, fs) => op2fpr("sqrt.d", fd, fs)
                case SQRT_S(fd, fs) => op2fpr("sqrt.s", fd, fs)
                case TRUNC_L_D(fd, fs) => op2fpr("trunc.l.d", fd, fs)
                case TRUNC_L_S(fd, fs) => op2fpr("trunc.l.s", fd, fs)
                case TRUNC_W_D(fd, fs) => op2fpr("trunc.w.d", fd, fs)
                case TRUNC_W_S(fd, fs) => op2fpr("trunc.w.s", fd, fs)
                case UnknownFPInstruction => "Unknown floating point instruction"
                -- case _ => "Unmatched floating point instruction"
            }
        case _ => "Unmatched floating point instruction"
    }

word COP1Encode (i::instruction) = '010001' : 0
