---------------------------------------------------------------------------
-- Decoders for the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- Instruction decoding
--================================================

pattern drop5 :: bits (5)

instruction COP2Decode (v::bits(26)) =
{
    match v
    {
        case '00000 rd cb _ 010' => COP2(CHERICOP2(CGet(CGetBase(rd, cb))))
        case '01101 rd cb _ 010' => COP2(CHERICOP2(CGet(CGetOffset(rd, cb))))
        case '00000 rd cb _ 011' => COP2(CHERICOP2(CGet(CGetLen(rd, cb))))
        case '00000 rd cb _ 101' => COP2(CHERICOP2(CGet(CGetTag(rd, cb))))
        case '00000 rd cb _ 110' => COP2(CHERICOP2(CGet(CGetSealed(rd, cb))))
        case '00000 rd cb _ 000' => COP2(CHERICOP2(CGet(CGetPerm(rd, cb))))
        case '00000 rd cb _ 001' => COP2(CHERICOP2(CGet(CGetType(rd, cb))))
        case '00000 drop5 cd _ 111' => COP2(CHERICOP2(CGet(CGetPCC(cd))))
        case '00000 rd 00000 _ 100' => COP2(CHERICOP2(CGet(CGetCause(rd))))
        case '00100 00000 00000 rt _ 100' => COP2(CHERICOP2(CSet(CSetCause(rt))))
        case '00100 cd cb rt _ 010' => COP2(CHERICOP2(CSet(CIncBase(cd, cb, rt))))
        case '00100 cd cb rt _ 011' => COP2(CHERICOP2(CSet(CSetLen(cd, cb, rt))))
        case '00100 cd cb _ 101' => COP2(CHERICOP2(CSet(CClearTag(cd, cb))))
        case '00100 cd cb rt _ 000' => COP2(CHERICOP2(CSet(CAndPerm(cd, cb, rt))))
        case '01101 cd cb rt _ 001' => COP2(CHERICOP2(CSet(CSetOffset(cd, cb, rt))))
        case '00100 cd cb rt _ 001' => COP2(CHERICOP2(CSet(CSetType(cd, cb, rt))))
        case '01011 cs drop5 rt _ 000' => COP2(CHERICOP2(CCheck(CCheckPerm(cs, rt))))
        case '01011 cs cb _ 001' => COP2(CHERICOP2(CCheck(CCheckType(cs, cb))))
        case '00100 cd cb rt _ 111' => COP2(CHERICOP2(CSet(CFromPtr(cd, cb, rt))))
        case '01100 rd cb ct _' => COP2(CHERICOP2(CGet(CToPtr(rd, cb, ct))))
        case '01110 rd cb ct _ t' => COP2(CHERICOP2(CPtrCmp(rd, cb, ct, t)))
        case '01001 cb offset' => COP2(CHERICOP2(CBTU(cb, offset)))
        case '01010 cb offset' => COP2(CHERICOP2(CBTS(cb, offset)))
        case '01000 drop5 cb _' => COP2(CHERICOP2(CJR(cb)))
        case '00111 cd cb _' => COP2(CHERICOP2(CJALR(cd, cb)))
        case '00001 cd cs _' => COP2(CHERICOP2(CSealCode(cd, cs)))
        case '00010 cd cs ct _' => COP2(CHERICOP2(CSealData(cd, cs, ct)))
        case '00011 cd cs ct _' => COP2(CHERICOP2(CUnseal(cd, cs, ct)))
        case '00101 cs cb _' => COP2(CHERICOP2(CCall(cs, cb)))
        case '00110 _' => COP2(CHERICOP2(CReturn))
        case _ => COP2(CHERICOP2(UnknownCapInstruction))
    }
}

instruction LWC2Decode (v::bits(26)) =
{
    match v
    {
        case 'rd cb rt offset 0 t' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b0, t)))
        case 'rd cb rt offset 100' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b00)))
        case 'rd cb rt offset 101' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b01)))
        case 'rd cb rt offset 110' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b10)))
        case 'rd cb rt offset 111' => LWC2(CHERILWC2(CLLD(rd, cb, rt, offset)))
        case _ => COP2(CHERICOP2(UnknownCapInstruction))
    }
}
instruction LDC2Decode (v::bits(26)) =
{
    match v
    {
        case 'cd cb rt offset' => LDC2(CHERILDC2(CLC(cd, cb, rt, offset)))
        case _ => COP2(CHERICOP2(UnknownCapInstruction))
    }
}
instruction SWC2Decode (v::bits(26)) =
{
    match v
    {
        case 'rs cb rt offset 0 t' => SWC2(CHERISWC2(CStore(rs, cb, rt, offset, t)))
        case 'rs cb rt offset 111' => SWC2(CHERISWC2(CSCD(rs, cb, rt, offset)))
        case _ => COP2(CHERICOP2(UnknownCapInstruction))
    }
}
instruction SDC2Decode (v::bits(26)) =
{
    match v
    {
        case 'cs cb rt offset' => SDC2(CHERISDC2(CSC(cs, cb, rt, offset)))
        case _ => COP2(CHERICOP2(UnknownCapInstruction))
    }
}
