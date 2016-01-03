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
                case ABS_D(fd, fs) => op2r("abs.d", fd, fs)
                case ABS_S(fd, fs) => op2r("abs.s", fd, fs)
                case ADD_D(fd, fs, ft) => op3r("add.d", fd, fs, ft)
                case ADD_S(fd, fs, ft) => op3r("add.s", fd, fs, ft)
                case BC1F(i) => "bc1f"
                case BC1FL(i) => "bc1fl"
                case BC1T(i) => "bc1t"
                case BC1TL(i) => "bc1tl"
                case C_F_D(fs, ft) => op2r("c.f.d", fs, ft)
                case C_F_S(fs, ft) => op2r("c.f.s", fs, ft)
                case C_UN_D(fs, ft) => op2r("c.un.d", fs, ft)
                case C_UN_S(fs, ft) => op2r("c.un.s", fs, ft)
                case C_EQ_D(fs, ft) => op2r("c.eq.d", fs, ft)
                case C_EQ_S(fs, ft) => op2r("c.eq.s", fs, ft)
                case C_UEQ_D(fs, ft) => op2r("c.ueq.d", fs, ft)
                case C_UEQ_S(fs, ft) => op2r("c.ueq.s", fs, ft)
                case C_OLT_D(fs, ft) => op2r("c.olt.d", fs, ft)
                case C_OLT_S(fs, ft) => op2r("c.olt.s", fs, ft)
                case C_ULT_D(fs, ft) => op2r("c.ult.d", fs, ft)
                case C_ULT_S(fs, ft) => op2r("c.ult.s", fs, ft)
                case C_OLE_D(fs, ft) => op2r("c.ole.d", fs, ft)
                case C_OLE_S(fs, ft) => op2r("c.ole.s", fs, ft)
                case C_ULE_D(fs, ft) => op2r("c.ule.d", fs, ft)
                case C_ULE_S(fs, ft) => op2r("c.ule.s", fs, ft)
                case CEIL_L_D(fd, fs) => op2r("ceil.l.d", fd, fs)
                case CEIL_L_S(fd, fs) => op2r("ceil.l.s", fd, fs)
                case CEIL_W_D(fd, fs) => op2r("ceil.w.d", fd, fs)
                case CEIL_W_S(fd, fs) => op2r("ceil.w.s", fd, fs)
                case CFC1(rt, fs) => op2r("cfc1", rt, fs)
                case CTC1(rt, fs) => op2r("ctc1", rt, fs)
                case CVT_D_L(fd, fs) => op2r("cvt.d.l", fd, fs)
                case CVT_D_S(fd, fs) => op2r("cvt.d.s", fd, fs)
		case CVT_D_W(fd, fs) => op2r("cvt.d.w", fd, fs)
                case CVT_S_L(fd, fs) => op2r("cvt.s.l", fd, fs)
                case CVT_S_D(fd, fs) => op2r("cvt.s.d", fd, fs)
                case CVT_S_W(fd, fs) => op2r("cvt.s.w", fd, fs)
                case DIV_D(fd, fs, ft) => op3r("div.d", fd, fs, ft)
                case DIV_S(fd, fs, ft) => op3r("div.s", fd, fs, ft)
		case DMFC1(rt, fs) => op2r("dmfc1", rt, fs)
		case DMTC1(rt, fs) => op2r("dmtc1", rt, fs)
                case FLOOR_L_D(fd, fs) => op2r("floor.l.d", fd, fs)
                case FLOOR_L_S(fd, fs) => op2r("floor.l.s", fd, fs)
                case FLOOR_W_D(fd, fs) => op2r("floor.w.d", fd, fs)
                case FLOOR_W_S(fd, fs) => op2r("floor.w.s", fd, fs)
                case LDC1(ft, offset, base) => "ldc1"
                case LDXC1(fs, index, base) => "ldxc1"
                case LWC1(ft, offset, base) => "lwc1"
                case MFC1(rt, fs) => op2r("mfc1", rt, fs)
                case MADD_D(fd, fr, fs, ft) => "madd.d"
                case MADD_S(fd, fr, fs, ft) => "madd.s"
                case MSUB_D(fd, fr, fs, ft) => "msub.d"
                case MSUB_S(fd, fr, fs, ft) => "msub.s"
                case MOV_D(fd, fs) => op2r("mov.d", fd, fs)
                case MOV_S(fd, fs) => op2r("mov.s", fd, fs)
                case MOVF(rd, rs) => op2r("movf", rd, rs)
                case MOVF_D(fd, fs) => op2r("movf.d", fd, fs)
                case MOVF_S(fd, fs) => op2r("movf.s", fd, fs)
                case MOVN_D(fd, fs, rt) => op3r("movn.d", fd, fs, rt)
                case MOVN_S(fd, fs, rt) => op3r("movn.s", fd, fs, rt)
                case MOVT(rd, rs) => op2r("movt", rd, rs)
                case MOVT_D(fd, fs) => op2r("movt.d", fd, fs)
                case MOVT_S(fd, fs) => op2r("movt.s", fd, fs)
                case MOVZ_D(fd, fs, rt) => op3r("movz.d", fd, fs, rt)
                case MOVZ_S(fd, fs, rt) => op3r("movz.s", fd, fs, rt)
                case MTC1(rt, fs) => op2r("mtc1", rt, fs)
                case MUL_D(fd, fs, ft) => op3r("mul.d", fd, fs, ft)
                case MUL_S(fd, fs, ft) => op3r("mul.s", fd, fs, ft)
                case NEG_D(fd, fs) => op2r("neg.d", fd, fs)
                case NEG_S(fd, fs) => op2r("neg.s", fd, fs)
                case ROUND_L_D(fd, fs) => op2r("round.l.d", fd, fs)
                case ROUND_L_S(fd, fs) => op2r("round.l.s", fd, fs)
                case ROUND_W_D(fd, fs) => op2r("round.w.d", fd, fs)
                case ROUND_W_S(fd, fs) => op2r("round.w.s", fd, fs)
                case SDC1(ft, offset, base) => opmem("sdc1", ft, base, offset)
                case SDXC1(fs, index, base) => "sdxc1"
                case SWC1(ft, offset, base) => "swc1"
                case SUB_D(fd, fs, ft) => op3r("sub.d", fd, fs, ft)
                case SUB_S(fd, fs, ft) => op3r("sub.s", fd, fs, ft)
                case SQRT_D(fd, fs) => op2r("sqrt.d", fd, fs)
                case SQRT_S(fd, fs) => op2r("sqrt.s", fd, fs) 
                case TRUNC_L_D(fd, fs) => op2r("trunc.l.d", fd, fs)
                case TRUNC_L_S(fd, fs) => op2r("trunc.l.s", fd, fs)
                case TRUNC_W_D(fd, fs) => op2r("trunc.w.d", fd, fs)
                case TRUNC_W_S(fd, fs) => op2r("trunc.w.s", fd, fs)
                case UnknownFPInstruction => "Unknown floating point instruction"
                case _ => "Unmatched floating point instruction"
            }
        case _ => "Unmatched floating point instruction"
    }

word COP1Encode (i::instruction) = '010001' : 0
