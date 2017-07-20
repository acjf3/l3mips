---------------------------------------------------------------------------
-- Pretty printing stubs for CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

inline string pad (s::string) = PadRight (#" ", 12, s)
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
string op_gr_gr_cr(op::string, r1::reg, r2::reg, cr1::reg) = pad(op):gr(r1):", ":gr(r2):", ":cr(cr1)

string COP2InstructionToString (i::instruction) =
    match i
    {
        case COP2(CHERICOP2(j)) =>
            match j
            {
                case DumpCapReg                         => "mtc2 ?,?,6"
                case CGet(CGetBase(rd, cb))             => op_gr_cr("cgetbase",rd,cb)
                case CGet(CGetOffset(rd, cb))           => op_gr_cr("cgetoffset",rd,cb)
                case CGet(CGetLen(rd, cb))              => op_gr_cr("cgetlen",rd,cb)
                case CGet(CGetTag(rd, cb))              => op_gr_cr("cgettag",rd,cb)
                case CGet(CGetSealed(rd, cb))           => op_gr_cr("cgetsealed",rd,cb)
                case CGet(CGetPerm(rd, cb))             => op_gr_cr("cgetperm",rd,cb)
                case CGet(CGetType(rd, cb))             => op_gr_cr("cgettype",rd,cb)
                case CGet(CGetPCC(cd))                  => op_cr("cgetpcc",cd)
                case CGet(CGetPCCSetOffset(cd, rs))     => op_cr_gr("cgetpccsetoffset",cd,rs)
                case CGet(CGetCause(rd))                => op_gr("cgetcause",rd)
                case CSet(CSetCause(rt))                => op_gr("csetcause",rt)
                case CSet(CSetBounds(cd, cb, rt))       => op_cr_cr_gr("csetbounds",cd,cb,rt)
                case CSet(CSetBoundsExact(cd, cb, rt))  => op_cr_cr_gr("csetboundsexact",cd,cb,rt)
                case CSet(CIncOffset(cd, cb, rt))       => op_cr_cr_gr("cincoffset",cd,cb,rt)
                case CSet(CClearRegs(regset, mask))     => op_gr_imm("cclearregs", regset, mask)
                case CSet(CClearTag(cd, cb))            => op_cr_cr("ccleartag",cd,cb)
                case CSet(CAndPerm(cd, cb, rt))         => op_cr_cr_gr("candperm",cd,cb,rt)
                case CSet(CSetOffset(cd, cb, rt))       => op_cr_cr_gr("csetoffset",cd,cb,rt)
                case CSub(rd, cb, ct)                   => op_gr_cr_cr("csub",rd,cb,ct)
                case CCheck(CCheckPerm(cs, rt))         => op_cr_gr("ccheckperm",cs,rt)
                case CCheck(CCheckType(cs, cb))         => op_cr_cr("cchecktype",cs,cb)
                case CSet(CFromPtr(cd, cb, rt))         => op_cr_cr_gr("cfromptr",cd,cb,rt)
                case CGet(CToPtr(rd, cb, ct))           => op_gr_cr_cr("ctoptr",rd,cb,ct)
                case CPtrCmp(rd, cb, ct, t)             =>
                    match t
                    {
                        case 0b000 => op_gr_cr_cr("ceq",rd,cb,ct)
                        case 0b001 => op_gr_cr_cr("cne",rd,cb,ct)
                        case 0b010 => op_gr_cr_cr("clt",rd,cb,ct)
                        case 0b011 => op_gr_cr_cr("cle",rd,cb,ct)
                        case 0b100 => op_gr_cr_cr("cltu",rd,cb,ct)
                        case 0b101 => op_gr_cr_cr("cleu",rd,cb,ct)
                        case 0b110 => op_gr_cr_cr("cexeq",rd,cb,ct)
                        case 0b111 => op_gr_cr_cr("cnexeq",rd,cb,ct)
                        case _     => "unmatched_cap_inst"
                    }
                case CBTU(cb, offset)             => op_cr_imm("cbtu",cb,offset)
                case CBTS(cb, offset)             => op_cr_imm("cbts",cb,offset)
                case CJR(cb)                      => op_cr("cjr",cb)
                case CJALR(cd, cb)                => op_cr_cr("cjalr",cb,cd)
                case CSeal(cd, cs, ct)            => op_cr_cr_cr("cseal",cd,cs,ct)
                case CUnseal(cd, cs, ct)          => op_cr_cr_cr("cunseal",cd,cs,ct)
                case CCall0(cs, cb)               => op_cr_cr("ccall0",cs,cb)
                case CCall1(cs, cb)               => op_cr_cr("ccall1",cs,cb)
                case CReturn                      => "creturn"
                case CLLx(rd, cb, stt)            =>
                    match stt
                    {
                        case 0b000 => op_gr_cr("cllbu", rd, cb)
                        case 0b001 => op_gr_cr("cllhu", rd, cb)
                        case 0b010 => op_gr_cr("cllwu", rd, cb)
                        case 0b011 => op_gr_cr("clld", rd, cb)
                        case 0b100 => op_gr_cr("cllb", rd, cb)
                        case 0b101 => op_gr_cr("cllh", rd, cb)
                        case 0b110 => op_gr_cr("cllw", rd, cb)
                        case 0b111 => op_gr_cr("cllx", rd, cb)
                    }
                case CLLC(cd, cb)                 => op_cr_cr("cllc", cd, cb)
                case CSCx(rs, cb, rd, tt)         =>
                    match tt
                    {
                        case 0b00 => op_gr_gr_cr("cscb", rd, rs, cb)
                        case 0b01 => op_gr_gr_cr("csch", rd, rs, cb)
                        case 0b10 => op_gr_gr_cr("cscw", rd, rs, cb)
                        case 0b11 => op_gr_gr_cr("cscd", rd, rs, cb)
                    }
                case CSCC(cs, cb, rd)             => op_gr_cr_cr("cscc", rd, cs, cb)
                case CMOVN(cd, cb, rt)            => op_cr_cr_gr("cmovn", cd, cb, rt)
                case CMOVZ(cd, cb, rt)            => op_cr_cr_gr("cmovz", cd, cb, rt)
                --case CBuildCap(cd, cb, ct)        => op_cr_cr_cr("cbuildcap", cd, cb, ct)
                --case CCopyType(cd, cb, ct)        => op_cr_cr_cr("ccopytype", cd, cb, ct)
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

bits(26) CHERICOP2Encode (j::CHERICOP2) =
    match j
    {
        case DumpCapReg                     => '00100' : 0 : '110'
        case CGet(CGetBase(rd, cb))         => '00000' : rd : cb : 0 : '000010'
        case CGet(CGetOffset(rd, cb))       => '01101' : rd : cb : 0 : '000010'
        case CGet(CGetLen(rd, cb))          => '00000' : rd : cb : 0 : '000011'
        case CGet(CGetTag(rd, cb))          => '00000' : rd : cb : 0 : '000101'
        case CGet(CGetSealed(rd, cb))       => '00000' : rd : cb : 0 : '000110'
        case CGet(CGetPerm(rd, cb))         => '00000' : rd : cb : 0 : '000000'
        case CGet(CGetType(rd, cb))         => '00000' : rd : cb : 0 : '000001'
        case CGet(CGetPCC(cd))              => '00000' : cd : '00000' : '11111' : '111111'
        case CGet(CGetPCCSetOffset(cd,rs))  => '00000' : cd : rs : '00111' : '111111'
        case CGet(CGetCause(rd))            => '00000' : rd : '00000' : 0 : '00100'
        case CSet(CSetCause(rt))            => '00100' : '00000' : '00000' : rt : 0 : '100'
        case CSet(CSetBounds(cd, cb, rt))   => '00001' : cd : cb : rt : 0
        case CSet(CIncOffset(cd, cb, rt))   => '01101' : cd : cb : rt : 0 : '000'
        case CSet(CClearRegs(regset, mask)) => '01111' : regset : mask
        case CSet(CClearTag(cd, cb))        => '00100' : cd : cb : 0 : '101'
        case CSet(CAndPerm(cd, cb, rt))     => '00100' : cd : cb : rt : 0 : '000'
        case CSet(CSetOffset(cd, cb, rt))   => '01101' : cd : cb : rt : 0 : '001'
        case CSub(rd, cb, ct)               => '00000' : rd : cb : ct : '001010'
        case CCheck(CCheckPerm(cs, rt))     => '01011' : cs : 0`5 : rt : 0 : '000'
        case CCheck(CCheckType(cs, cb))     => '01011' : cs : cb : 0 : '001'
        case CSet(CFromPtr(cd, cb, rt))     => '00100' : cd : cb : rt : 0 : '111'
        case CGet(CToPtr(rd, cb, ct))       => '01100' : rd : cb : ct : 0
        case CPtrCmp(rd, cb, ct, t)         => '01110' : rd : cb : ct : 0 : t
        case CBTU(cb, offset)               => '01001' : cb : offset
        case CBTS(cb, offset)               => '01010' : cb : offset
        case CJR(cb)                        => '01000' : 0`5 : cb : 0
        case CJALR(cd, cb)                  => '00111' : cd : cb : 0
        case CSeal(cd, cs, ct)              => '00010' : cd : cs : ct : 0
        case CUnseal(cd, cs, ct)            => '00011' : cd : cs : ct : 0
        case CCall0(cs, cb)                 => '00101' : cs : cb : '00000000000'
        case CCall1(cs, cb)                 => '00101' : cs : cb : '00000000001'
        case CReturn                        => '00110' : 0
        case CLLx(rd, cb, 's 00')           => '10000' : rd : cb : 0 : '1' : s : '00'
        case CLLx(rd, cb, 's 01')           => '10000' : rd : cb : 0 : '1' : s : '01'
        case CLLx(rd, cb, 's 10')           => '10000' : rd : cb : 0 : '1' : s : '10'
        case CLLx(rd, cb, '011')            => '10000' : rd : cb : 0 : '1011'
        case CLLx(rd, cb, '111')            => UNKNOWN
        case CLLC(cd, cb)                   => '10000' : cd : cb : 0 : '1111'
        case CSCx(rs, cb, rd, tt)           => '10000' : rs : cb : rd : 0 : '00' : tt
        case CSCC(cs, cb, rd)               => '10000' : cs : cb : rd : 0 : '0111'
        case UnknownCapInstruction          => '11111' : 0
    }

word COP2Encode (i::instruction) =
    match i
    {
        case COP2(CHERICOP2(j)) => '010010' : CHERICOP2Encode(j)
        case _ => UNKNOWN
    }

word LWC2Encode (i::instruction) =
    match i
    {
        case LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b0, t)))    => '110010' : rd : cb : rt : offset : '0' : t
        case LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b00))) => '110010' : rd : cb : rt : offset : '100'
        case LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b01))) => '110010' : rd : cb : rt : offset : '101'
        case LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b10))) => '110010' : rd : cb : rt : offset : '110'
        case _ => UNKNOWN
    }

word LDC2Encode (i::instruction) =
    match i
    {
        case LDC2(CHERILDC2(CLC(c, cb, rt, offset))) => '110110' : c : cb : rt : offset
        case _ => UNKNOWN
    }

word SWC2Encode (i::instruction) =
    match i
    {
        case SWC2(CHERISWC2(CStore(rs, cb, rt, offset, t))) => '111010' : rs : cb : rt : offset : '0' : t
        case _ => UNKNOWN
    }

word SDC2Encode (i::instruction) =
    match i
    {
        case SDC2(CHERISDC2(CSC(c, cb, rt, offset))) => '111110' : c : cb : rt : offset
        case _ => UNKNOWN
    }

