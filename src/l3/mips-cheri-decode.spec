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
        case '00000 rd cb _ 010' => COP2(CHERI(CGet(CGetBase(rd, cb))))
{-
        case '010 010 01101 rd cb _ 010' => CGet(CGetOffset(rd, cb))
        case '010 010 00000 rd cb _ 011' => CGet(CGetLen(rd, cb))
        case '010 010 00000 rd cb _ 101' => CGet(CGetTag(rd, cb))
        case '010 010 00000 rd cb _ 110' => CGet(CGetUnsealed(rd, cb))
        case '010 010 00000 rd cb _ 000' => CGet(CGetPerm(rd, cb))
        case '010 010 00000 rd cb _ 001' => CGet(CGetType(rd, cb))
        case '010 010 00000 rd cd _ 111' => CGet(CGetPCC(rd, cd))
        case '010 010 00100 00000 00000 rt _ 100' => CSetCause(rt)
        case '010 010 00100 cd cb rt _ 010' => CIncBase(cd, cb, rt)
        case '010 010 00100 cd cb rt _ 011' => CSetLen(cd, cb, rt)
        case '010 010 00100 cd cb _ 101' => CClearTag(cd, cb)
        case '010 010 00100 cd cb rt _ 000' => CAndPerm(cd, cb, rt)
        case '010 010 01101 cd cb rt _ 001' => CSetOffset(cd, cb, rt)
        case '010 010 00100 cd cb rt _ 001' => CSetType(cd, cb, rt)
        case '010 010 01011 cs drop5 rt _ 000' => CCheckPerm(cs, rt)
        case '010 010 01011 cs cb _ 001' => CCheckType(cs, cb)
        case '010 010 00100 cd cb rt _ 111' => CFromPtr(cd, cb, rt)
        case '010 010 01100 rd cb ct _' => CToPtr(rd, cb, ct)
        case '010 010 01001 cb offset' => CBTU(cb, offset)
        case '010 010 01010 cb offset' => CBTS(cb, offset)
        case '111 110 cs cb rt offset' => CSC(cs, cb, rt, offset)
        case '110 110 cd cb rt offset' => CLC(cd, cb, rt, offset)
        case '110 010 rd cb rt offset s t' => CLoad(rd, cb, rt, offset, s, t)
        case '111 010 rs cb rt offset 0 t' => CStore(rd, cb, rt, offset, t)
        case '110 010 rd cb rt offset 111' => CLLD(rd, cb, rt, offset)
        case '111 010 rs cb rt offset 111' => CSCD(rs, cb, rt, offset)
        case '010 010 01000 drop5 cb rt _' => CJR(cb, rt)
        case '010 010 00111 drop5 cb rt _' => CJALR(cb, rt)
        case '010 010 00001 cd cs _' => CSealCode(cd, cs)
        case '010 010 00010 cd cs ct _' => CSealData(cd, cs, ct)
        case '010 010 00011 cd cs ct _' => CUnseal(cd, cs, ct)
        case '010 010 00101 cs cb _' => CCall(cs, cb)
        case '010 010 00110 _' => CReturn
-}
        case _ => COP2(CHERI(UnknownCapInstruction))
    }
}

instruction LWC2Decode (v::bits(26)) =
{
    match v
    {
        case _ => LWC2(CLoad(v))
    }
}
instruction LDC2Decode (v::bits(26)) =
{
    match v
    {
        case _ => LDC2(CLoadCap(v))
    }
}
instruction SWC2Decode (v::bits(26)) =
{
    match v
    {
        case _ =>SWC2(CStore(v))
    }
}
instruction SDC2Decode (v::bits(26)) =
{
    match v
    {
        case _ => SDC2(CStoreCap(v))
    }
}
