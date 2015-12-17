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
                case BC1T(i) => "bc1t"
                case C_F_D(fs, ft) => "c.f.d"
                case C_F_S(fs, ft) => "c.f.s"
                case C_UN_D(fs, ft) => "c.un.d"
                case C_UN_S(fs, ft) => "c.un.s"
                case C_EQ_D(fs, ft) => "c.eq.d"
                case C_EQ_S(fs, ft) => "c.eq.s"
                case C_UEQ_D(fs, ft) => "c.ueq.d"
                case C_UEQ_S(fs, ft) => "c.ueq.s"
                case C_OLT_D(fs, ft) => "c.olt.d"
                case C_OLT_S(fs, ft) => "c.olt.s"
                case C_ULT_D(fs, ft) => "c.ult.d"
                case C_ULT_S(fs, ft) => "c.ult.s"
                case C_OLE_D(fs, ft) => "c.ole.d"
                case C_OLE_S(fs, ft) => "c.ole.s"
                case C_ULE_D(fs, ft) => "c.ule.d"
                case C_ULE_S(fs, ft) => "c.ule.s"
                case CEIL_L_D(fd, fs) => "ceil.l.d"
                case CEIL_L_S(fd, fs) => "ceil.l.s"
                case CEIL_W_D(fd, fs) => "ceil.w.d"
                case CEIL_W_S(fd, fs) => "ceil.w.s"
                case CFC1(rt, fs) => "cfc1"
                case CTC1(rt, fs) => "ctc1"
                case CVT_D_L(fd, fs) => "cvt.d.l"
                case CVT_D_S(fd, fs) => "cvt.d.s"
		case CVT_D_W(fd, fs) => "cvt.d.w"
                case CVT_S_L(fd, fs) => "cvt.s.l"
                case CVT_S_D(fd, fs) => "cvt.s.d"
                case CVT_S_W(fd, fs) => "cvt.s.w"
                case DIV_D(fd, fs, ft) => op3r("div.d", fd, fs, ft)
                case DIV_S(fd, fs, ft) => op3r("div.s", fd, fs, ft)
		case DMFC1(rt, fs) => "dmfc1"
		case DMTC1(rt, fs) => "dmtc1"
                case FLOOR_L_D(fd, fs) => "floor.l.d"
                case FLOOR_L_S(fd, fs) => "floor.l.s"
                case FLOOR_W_D(fd, fs) => "floor.w.d"
                case FLOOR_W_S(fd, fs) => "floor.w.s"
                case LDC1(ft, offset, base) => "ldc1"
                case LDXC1(fs, index, base) => "ldxc1"
                case LWC1(ft, offset, base) => "lwc1"
                case MFC1(rt, fs) => "mfc1"
                case MADD_D(fd, fr, fs, ft) => "madd.d"
                case MADD_S(fd, fr, fs, ft) => "madd.s"
                case MSUB_D(fd, fr, fs, ft) => "msub.d"
                case MSUB_S(fd, fr, fs, ft) => "msub.s"
                case MOV_D(fd, fs) => "mov.d"
                case MOV_S(fd, fs) => "mov.s"
                case MOVF(rd, rs) => "movf"
                case MOVF_D(fd, fs) => "movf.d"
                case MOVF_S(fd, fs) => "movf.s"
                case MOVN_D(fd, fs, rt) => "movn.d"
                case MOVN_S(fd, fs, rt) => "movn.s"
                case MOVT(rd, rs) => "movt"
                case MOVT_D(fd, fs) => "movt.d"
                case MOVT_S(fd, fs) => "movt.s"
                case MOVZ_D(fd, fs, rt) => "movz.d"
                case MOVZ_S(fd, fs, rt) => "movz.s"
                case MTC1(rt, fs) => "mtc1"
                case MUL_D(fd, fs, ft) => op3r("mul.d", fd, fs, ft)
                case MUL_S(fd, fs, ft) => op3r("mul.s", fd, fs, ft)
                case NEG_D(fd, fs) => op2r("neg.d", fd, fs)
                case NEG_S(fd, fs) => op2r("neg.s", fd, fs)
                case ROUND_L_D(fd, fs) => "round.l.d"
                case ROUND_L_S(fd, fs) => "round.l.s"
                case ROUND_W_D(fd, fs) => "round.w.d"
                case ROUND_W_S(fd, fs) => "round.w.s"
                case SDC1(ft, offset, base) => opmem("sdc1", ft, base, offset)
                case SDXC1(fs, index, base) => "sdxc1"
                case SWC1(ft, offset, base) => "swc1"
                case SUB_D(fd, fs, ft) => op3r("sub.d", fd, fs, ft)
                case SUB_S(fd, fs, ft) => op3r("sub.s", fd, fs, ft)
                case SQRT_D(fd, fs) => "sqrt.d"
                case SQRT_S(fd, fs) => "sqrt.s"
                case TRUNC_L_D(fd, fs) => "trunc.l.d"
                case TRUNC_L_S(fd, fs) => "trunc.l.s"
                case TRUNC_W_D(fd, fs) => "trunc.w.d"
                case TRUNC_W_S(fd, fs) => "trunc.w.s"
                case UnknownFPInstruction => "Unknown floating point instruction"
                case _ => "Unmatched floating point instruction"
            }
        case _ => "Unmatched floating point instruction"
    }

word COP1Encode (i::instruction) = '010001' : 0
