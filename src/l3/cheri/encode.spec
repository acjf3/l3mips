---------------------------------------------------------------------------
-- Pretty printing stubs for CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

string pad (s::string) = PadRight (#" ", 12, s)
string gr (n::reg) = "$" : [[n]::nat]
string cr (n::reg) = "c" : [[n]::nat]
string imm (n::bits(N)) = if n <+ 10 then [n] else "0x":ToLower([n])

string op_cr(op::string, cr1::reg) = pad(op):cr(cr1)
string op_gr(op::string, r1::reg) = pad(op):gr(r1)
string op_gr_imm(op::string, gr1::reg, i::bits(N)) = pad(op):gr(gr1):", ":imm(i)
string op_cr_imm(op::string, cr1::reg, i::bits(N)) = pad(op):cr(cr1):", ":imm(i)
string op_cr_gr(op::string, cr1::reg, r1::reg) = pad(op):cr(cr1):", ":gr(r1)
string op_gr_cr(op::string, r1::reg, cr1::reg) = pad(op):gr(r1):", ":cr(cr1)
string op_cr_cr(op::string, cr1::reg, cr2::reg) = pad(op):cr(cr1):", ":cr(cr2)
string op_cr_cr_gr(op::string, cr1::reg, cr2::reg, r1::reg) = pad(op):cr(cr1):", ":cr(cr2):", ":gr(r1)
string op_cr_cr_cr(op::string, cr1::reg, cr2::reg, cr3::reg) = pad(op):cr(cr1):", ":cr(cr2):", ":cr(cr3)
string op_gr_cr_cr(op::string, r1::reg, cr1::reg, cr2::reg) = pad(op):gr(r1):", ":cr(cr1):", ":cr(cr2)
string op_gr_cr_gr_imm(op::string, r1::reg, cr1::reg, r2::reg, i::bits(N)) = pad(op):gr(r1):", ":cr(cr1):", ":gr(r2):", ":imm(i)
string op_cr_cr_gr_imm(op::string, cr1::reg, cr2::reg, r1::reg, i::bits(N)) = pad(op):cr(cr1):", ":cr(cr2):", ":gr(r1):", ":imm(i)

string COP2InstructionToString (i::instruction) =
    match i
    {
        case COP2(CHERICOP2(j)) =>
            match j
            {
                case DumpCapReg                     => "mtc2 ?,?,6"
                case CGet(CGetBase(rd, cb))         => op_gr_cr("cgetbase",rd,cb)
                case CGet(CGetOffset(rd, cb))       => op_gr_cr("cgetoffset",rd,cb)
                case CGet(CGetLen(rd, cb))          => op_gr_cr("cgetlen",rd,cb)
                case CGet(CGetTag(rd, cb))          => op_gr_cr("cgettag",rd,cb)
                case CGet(CGetSealed(rd, cb))       => op_gr_cr("cgetsealed",rd,cb)
                case CGet(CGetPerm(rd, cb))         => op_gr_cr("cgetperm",rd,cb)
                case CGet(CGetType(rd, cb))         => op_gr_cr("cgettype",rd,cb)
                case CGet(CGetPCC(cd))              => op_cr("cgetpcc",cd)
                case CGet(CGetPCCSetOffset(cd, rs)) => op_cr_gr("cgetpccsetoffset",cd,rs)
                case CGet(CGetCause(rd))            => op_gr("cgetcause",rd)
                case CSet(CSetCause(rt))            => op_gr("csetcause",rt)
                case CSet(CSetBounds(cd, cb, rt))   => op_cr_cr_gr("csetbounds",cd,cb,rt)
                case CSet(CIncOffset(cd, cb, rt))   => op_cr_cr_gr("cincoffset",cd,cb,rt)
                case CSet(CClearRegs(regset, mask)) => op_gr_imm("cclearregs", regset, mask)
                case CSet(CClearTag(cd, cb))        => op_cr_cr("ccleartag",cd,cb)
                case CSet(CAndPerm(cd, cb, rt))     => op_cr_cr_gr("candperm",cd,cb,rt)
                case CSet(CSetOffset(cd, cb, rt))   => op_cr_cr_gr("csetoffset",cd,cb,rt)
                case CCheck(CCheckPerm(cs, rt))     => op_cr_gr("ccheckperm",cs,rt)
                case CCheck(CCheckType(cs, cb))     => op_cr_cr("cchecktype",cs,cb)
                case CSet(CFromPtr(cd, cb, rt))     => op_cr_cr_gr("cfromptr",cd,cb,rt)
                case CGet(CToPtr(rd, cb, ct))       => op_gr_cr_cr("ctoptr",rd,cb,ct)
                case CPtrCmp(rd, cb, ct, t)         =>
                    match t
                    {
                        case 0b000 => op_gr_cr_cr("ceq",rd,cb,ct)
                        case 0b001 => op_gr_cr_cr("cne",rd,cb,ct)
                        case 0b010 => op_gr_cr_cr("clt",rd,cb,ct)
                        case 0b011 => op_gr_cr_cr("cle",rd,cb,ct)
                        case 0b100 => op_gr_cr_cr("cltu",rd,cb,ct)
                        case 0b101 => op_gr_cr_cr("cleu",rd,cb,ct)
                        case _     => "unmatched_cap_inst"
                    }
                case CBTU(cb, offset)             => op_cr_imm("cbtu",cb,offset)
                case CBTS(cb, offset)             => op_cr_imm("cbts",cb,offset)
                case CJR(cb)                      => op_cr("cjr",cb)
                case CJALR(cd, cb)                => op_cr_cr("cjalr",cd,cb)
                case CSeal(cd, cs, ct)            => op_cr_cr_cr("cseal",cd,cs,ct)
                case CUnseal(cd, cs, ct)          => op_cr_cr_cr("cunseal",cd,cs,ct)
                case CCall(cs, cb)                => op_cr_cr("ccall",cs,cb)
                case CReturn                      => "creturn"
                case CLLx(rd, cb, stt)            => "cllx" -- TODO
                case CLLC(cd, cb)                 => op_cr_cr("cllc", cd, cb)
                case CSCx(rcs, cb, rd, ctt)       => "cscx" -- TODO
                case CSCC(cs, cb, rd)             => op_gr_cr_cr("cscc", rd, cs, cb)
                case UnknownCapInstruction        => "unknown_cap_inst"
            }
        case _ => "unmatched_cap_inst"
    }

string LWC2InstructionToString (i::instruction) =
    match i
    {
        case LWC2(CHERILWC2(j)) =>
            match j
            {
                case CLoad(rd, cb, rt, offset, 0b0, t) =>
                    match t
                    {
                        case 0b00 => op_gr_cr_gr_imm("clbu",rd,cb,rt,offset)
                        case 0b01 => op_gr_cr_gr_imm("clhu",rd,cb,rt,offset)
                        case 0b10 => op_gr_cr_gr_imm("clwu",rd,cb,rt,offset)
                        case 0b11 => op_gr_cr_gr_imm("cld",rd,cb,rt,offset)
                    }
                case CLoad(rd, cb, rt, offset, 0b1, 0b00) => op_gr_cr_gr_imm("clb",rd,cb,rt,offset)
                case CLoad(rd, cb, rt, offset, 0b1, 0b01) => op_gr_cr_gr_imm("clh",rd,cb,rt,offset)
                case CLoad(rd, cb, rt, offset, 0b1, 0b10) => op_gr_cr_gr_imm("clw",rd,cb,rt,offset)
                case _                                    => "unmatched_cap_inst"
            }
        case _ => "unmatched_cap_inst"
    }

string LDC2InstructionToString (i::instruction) =
    match i
    {
        case LDC2(CHERILDC2(j)) =>
            match j
            {
                case CLC(cd, cb, rt, offset) => op_cr_cr_gr_imm("clc",cd,cb,rt,offset)
            }
        case _ => "unmatched_cap_inst"
    }

string SWC2InstructionToString (i::instruction) =
    match i
    {
        case SWC2(CHERISWC2(j)) =>
            match j
            {
                case CStore(rs, cb, rt, offset, t) =>
                    match t
                    {
                        case 0b00 => op_gr_cr_gr_imm("csb",rs,cb,rt,offset)
                        case 0b01 => op_gr_cr_gr_imm("csh",rs,cb,rt,offset)
                        case 0b10 => op_gr_cr_gr_imm("csw",rs,cb,rt,offset)
                        case 0b11 => op_gr_cr_gr_imm("csd",rs,cb,rt,offset)
                    }
            }
        case _ => "unmatched_cap_inst"
    }

string SDC2InstructionToString (i::instruction) =
    match i
    {
        case SDC2(CHERISDC2(j)) =>
            match j
            {
                case CSC(cs, cb, rt, offset) => op_cr_cr_gr_imm("csc",cs,cb,rt,offset)
            }
        case _ => "unmatched_cap_inst"
    }

word COP2Encode (i::instruction) = '010010' : 0
word LWC2Encode (i::instruction) = '110010' : 0
word LDC2Encode (i::instruction) = '110110' : 0
word SWC2Encode (i::instruction) = '111010' : 0
word SDC2Encode (i::instruction) = '111110' : 0
