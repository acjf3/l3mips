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
                case ABS_D(fd, fs) => "abs.d"
                case ABS_S(fd, fs) => "abs.s"
                case ADD_D(fd, fs, ft) => "add.d"
                case ADD_S(fd, fs, ft) => "add.s"
                case C_F_D(ft, fs) => "c.f.d"
                case C_UN_D(ft, fs) => "c.un.d"
                case C_EQ_D(ft, fs) => "c.eq.d"
                case C_UEQ_D(ft, fs) => "c.ueq.d"
                case C_OLT_D(ft, fs) => "c.olt.d"
                case C_ULT_D(ft, fs) => "c.ult.d"
                case C_OLE_D(ft, fs) => "c.ole.d"
                case C_ULE_D(ft, fs) => "c.ule.d"
                case C_EQ_S(ft, fs) => "c.eq.s"
                case CEIL_L_D(fd, fs) => "ceil.l.d"
                case CEIL_W_D(fd, fs) => "ceil.w.d"
                case CFC1(rt, fs) => "cfc1"
                case CTC1(rt, fs) => "ctc1"
                case CVT_D_L(fd, fs) => "cvt.d.l"
                case CVT_D_S(fd, fs) => "cvt.d.s"
		case CVT_D_W(fd, fs) => "cvt.d.w"
                case CVT_S_D(fd, fs) => "cvt.s.d"
                case CVT_S_W(fd, fs) => "cvt.s.w"
                case DIV_D(fd, fs, ft) => "div.d"
                case DIV_S(fd, fs, ft) => "div.s"
		case DMFC1(rt, fs) => "dmfc1"
		case DMTC1(rt, fs) => "dmtc1"
                case FLOOR_L_D(fd, fs) => "floor.l.d"
                case FLOOR_W_D(fd, fs) => "floor.w.d"
                case MFC1(rt, fs) => "mfc1"
                case MOV_D(fd, fs) => "mov.d"
                case MOVF_D(fd, fs) => "movf.d"
                case MOVN_D(fd, fs, rt) => "movn.d"
                case MOVT_D(fd, fs) => "movt.d"
                case MOVZ_D(fd, fs, rt) => "movz.d"
                case MTC1(rt, fs) => "mtc1"
                case MUL_D(fd, fs, ft) => "mul.d"
                case MUL_S(fd, fs, ft) => "mul.s"
                case NEG_D(fd, fs) => "neg.d"
                case NEG_S(fd, fs) => "neg.s"
                case ROUND_L_D(fd, fs) => "round.l.d"
                case ROUND_W_D(fd, fs) => "round.w.d"
                case SUB_D(fd, fs, ft) => "sub.d"
                case SUB_S(fd, fs, ft) => "sub.s"
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
