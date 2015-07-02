---------------------------------------------------------------------------
-- Decoders for the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

instruction COP2Decode (v::bits(26)) =
   COP2(CHERICOP2
      (match v
       {
           case '00100 _ 110'                => DumpCapReg
           case '00000 rd cb _ 010'          => CGet(CGetBase(rd, cb))
           case '01101 rd cb _ 010'          => CGet(CGetOffset(rd, cb))
           case '00000 rd cb _ 011'          => CGet(CGetLen(rd, cb))
           case '00000 rd cb _ 101'          => CGet(CGetTag(rd, cb))
           case '00000 rd cb _ 110'          => CGet(CGetSealed(rd, cb))
           case '00000 rd cb _ 000'          => CGet(CGetPerm(rd, cb))
           case '00000 rd cb _ 001'          => CGet(CGetType(rd, cb))
           case '00000 _`5 cd _ 111'         => CGet(CGetPCC(cd))
           case '00000 rd 00000 _ 100'       => CGet(CGetCause(rd))
           case '00100 00000 00000 rt _ 100' => CSet(CSetCause(rt))
           case '00001 cd cb rt _'           => CSet(CSetBounds(cd, cb, rt))
           case '00100 cd cb rt _ 010'       => CSet(CIncBase(cd, cb, rt))
           case '01101 cd cb rt _ 000'       => CSet(CIncOffset(cd, cb, rt))
           case '00100 cd cb rt _ 011'       => CSet(CSetLen(cd, cb, rt))
           case '01111 regset mask'          => CSet(CClearRegs(regset, mask))
           case '00100 cd cb _ 101'          => CSet(CClearTag(cd, cb))
           case '00100 cd cb rt _ 000'       => CSet(CAndPerm(cd, cb, rt))
           case '01101 cd cb rt _ 001'       => CSet(CSetOffset(cd, cb, rt))
           case '01011 cs _`5 rt _ 000'      => CCheck(CCheckPerm(cs, rt))
           case '01011 cs cb _ 001'          => CCheck(CCheckType(cs, cb))
           case '00100 cd cb rt _ 111'       => CSet(CFromPtr(cd, cb, rt))
           case '01100 rd cb ct _'           => CGet(CToPtr(rd, cb, ct))
           case '01110 rd cb ct _ t'         => CPtrCmp(rd, cb, ct, t)
           case '01001 cb offset'            => CBTU(cb, offset)
           case '01010 cb offset'            => CBTS(cb, offset)
           case '01000 _`5 cb _'             => CJR(cb)
           case '00111 cd cb _'              => CJALR(cd, cb)
           case '00010 cd cs ct _'           => CSeal(cd, cs, ct)
           case '00011 cd cs ct _'           => CUnseal(cd, cs, ct)
           case '00101 cs cb _'              => CCall(cs, cb)
           case '00110 _'                    => CReturn
           case _                            => UnknownCapInstruction
       }))

instruction LWC2Decode (rd::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
   LWC2(CHERILWC2
      (match v
       {
           case '0 t' => CLoad(rd, cb, rt, offset, 0b0, t)
           case '100' => CLoad(rd, cb, rt, offset, 0b1, 0b00)
           case '101' => CLoad(rd, cb, rt, offset, 0b1, 0b01)
           case '110' => CLoad(rd, cb, rt, offset, 0b1, 0b10)
           case '111' => CLLD(rd, cb, rt, offset)
       }))

instruction SWC2Decode (rs::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
   match v
   {
      case '0 t' => SWC2(CHERISWC2(CStore(rs, cb, rt, offset, t)))
      case '111' => SWC2(CHERISWC2(CSCD(rs, cb, rt, offset)))
      case _     => COP2(CHERICOP2(UnknownCapInstruction))
   }

instruction LDC2Decode (a::reg * reg * reg * bits(11)) = LDC2(CHERILDC2(CLC(a)))
instruction SDC2Decode (a::reg * reg * reg * bits(11)) = SDC2(CHERISDC2(CSC(a)))
