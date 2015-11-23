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
                case ADD_D(fd, fs, ft) => "add.d"
                case CEIL_L_D(fd, fs) => "ceil.l.d"
                case CVT_D_L(fd, fs) => "cvt.d.l"
                case DIV_D(fd, fs, ft) => "div.d"
		case DMFC1(rt, fs) => "dmfc1"
		case DMTC1(rt, fs) => "dmtc1"
                case FLOOR_L_D(fd, fs) => "floor.l.d"
                case MFC1(rt, fs) => "mfc1"
                case MOV_D(fd, fs) => "mov.d"
                case MTC1(rt, fs) => "mtc1"
                case MUL_D(fd, fs, ft) => "mul.d"
                case NEG_D(fd, fs) => "neg.d"
                case ROUND_L_D(fd, fs) => "round.l.d"
                case SUB_D(fd, fs, ft) => "sub.d"
                case TRUNC_L_D(fd, fs) => "trunc.l.d"
                case UnknownFPInstruction => "Unknown floating point instruction"
            }
        case _ => "Unmatched floating point instruction"
    }

word COP1Encode (i::instruction) = '010001' : 0
