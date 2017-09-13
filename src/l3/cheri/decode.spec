---------------------------------------------------------------------------
-- Decoders for the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

instruction COP2Decode (v::bits(26)) =
   COP2(CHERICOP2
      (match v
       {
           case '00100 _ 110'                   => DumpCapReg
           case '00000 rd cb _ 000010'          => CGet(CGetBase(rd, cb))
           case '01101 rd cb _ 000010'          => CGet(CGetOffset(rd, cb))
           case '00000 rd cb _ 000011'          => CGet(CGetLen(rd, cb))
           case '00000 rd cb _ 000101'          => CGet(CGetTag(rd, cb))
           case '00000 rd cb _ 000110'          => CGet(CGetSealed(rd, cb))
           case '00000 rd cb _ 000000'          => CGet(CGetPerm(rd, cb))
           case '00000 rd cb _ 000001'          => CGet(CGetType(rd, cb))
           case '00000 cd 00000 11111 111111'   => CGet(CGetPCC(cd))
           case '00000 cd rs 00111 111111'      => CGet(CGetPCCSetOffset(cd,rs))
           case '00000 rd 00000 _ 000100'       => CGet(CGetCause(rd))
           case '00100 00000 00000 rt _ 100'    => CSet(CSetCause(rt))
           case '00001 cd cb rt _'              => CSet(CSetBounds(cd, cb, rt))
           case '00000 cd cb rt 001001'         => CSet(CSetBoundsExact(cd, cb, rt))
           case '10100 cd cb length'            => CSet(CSetBoundsImmediate(cd, cb, length))
           case '01101 cd cb rt _ 000'          => CSet(CIncOffset(cd, cb, rt))
           case '01011 cd cb increment'         => CSet(CIncOffsetImmediate(cd, cb, increment))
           case '01111 regset mask'             => CSet(CClearRegs(regset, mask))
           case '00100 cd cb _ 101'             => CSet(CClearTag(cd, cb))
           case '00100 cd cb rt _ 000'          => CSet(CAndPerm(cd, cb, rt))
           case '01101 cd cb rt _ 001'          => CSet(CSetOffset(cd, cb, rt))
           case '00000 rd cb ct 001010'         => CSub(rd, cb, ct)
           case '01011 cs _`5 rt _ 000'         => CCheck(CCheckPerm(cs, rt))
           case '01011 cs cb _ 001'             => CCheck(CCheckType(cs, cb))
           case '00100 cd cb rt _ 111'          => CSet(CFromPtr(cd, cb, rt))
           case '01100 rd cb ct _'              => CGet(CToPtr(rd, cb, ct))
           case '01110 rd cb ct _ t'            => CPtrCmp(rd, cb, ct, t)
           case '01001 cb offset'               => CBTU(cb, offset)
           case '01010 cb offset'               => CBTS(cb, offset)
           case '01000 _`5 cb _'                => CJR(cb)
           case '00111 cd cb _'                 => CJALR(cd, cb)
           case '00010 cd cs ct _'              => CSeal(cd, cs, ct)
           case '00011 cd cs ct _'              => CUnseal(cd, cs, ct)
           case '00101 cs cb 00000000000'       => CCall0(cs, cb)
           case '00101 cs cb 00000000001'       => CCall1(cs, cb)
           case '00110 _'                       => CReturn
           case '10000 rd cb _ 1 s 00'          => CLLx(rd, cb, s:'00')
           case '10000 rd cb _ 1 s 01'          => CLLx(rd, cb, s:'01')
           case '10000 rd cb _ 1 s 10'          => CLLx(rd, cb, s:'10')
           case '10000 rd cb _ 1011'            => CLLx(rd, cb, '011')
           case '10000 cd cb _ 1111'            => CLLC(cd, cb)
           case '10000 rs cb rd _ 00 tt'        => CSCx(rs, cb, rd, tt)
           case '10000 cs cb rd _ 0111'         => CSCC(cs, cb, rd)
           case '00000 cd cb rt 011100'         => CMOVN(cd, cb, rt)
           case '00000 cd cb rt 011011'         => CMOVZ(cd, cb, rt)
           --case '00000 rd cb ct 100000'         => CTestSubset(rd, cb, ct)
           --case '00000 cd cb ct 011101'         => CBuildCap(cd, cb, ct)
           --case '00000 cd cb ct 011110'         => CCopyType(cd, cb, ct)
           case _                               => UnknownCapInstruction
       }))

instruction LWC2Decode (rd::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
      match v
       {
           case '0 t' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b0, t)))
           case '100' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b00)))
           case '101' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b01)))
           case '110' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b10)))
           case _     => COP2(CHERICOP2(UnknownCapInstruction))
       }

instruction SWC2Decode (rs::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
   match v
   {
      case '0 t' => SWC2(CHERISWC2(CStore(rs, cb, rt, offset, t)))
      case _     => COP2(CHERICOP2(UnknownCapInstruction))
   }

instruction LDC2Decode (a::reg * reg * reg * bits(11)) = LDC2(CHERILDC2(CLC(a)))
instruction SDC2Decode (a::reg * reg * reg * bits(11)) = SDC2(CHERISDC2(CSC(a)))
