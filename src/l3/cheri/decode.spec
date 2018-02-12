---------------------------------------------------------------------------
-- Decoders for the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- Bits 31:26 = 0x12
instruction COP2Decode (v::bits(26)) =
   COP2(CHERICOP2
      (match v
       {
           case '00100 _ 110'                   => DumpCapReg

           -- Capability-Inspection Instructions
           case '00000    rd    cb 00000 111111'   => CGet(CGetPerm(rd, cb))
           case '00000    rd    cb 00001 111111'   => CGet(CGetType(rd, cb))
           case '00000    rd    cb 00010 111111'   => CGet(CGetBase(rd, cb))
           case '00000    rd    cb 00011 111111'   => CGet(CGetLen(rd, cb))
           case '00000    rd    cb 00100 111111'   => CGet(CGetTag(rd, cb))
           case '00000    rd    cb 00101 111111'   => CGet(CGetSealed(rd, cb))
           case '00000    rd    cb 00110 111111'   => CGet(CGetOffset(rd, cb))
           case '00000    cd 00000 11111 111111'   => CGet(CGetPCC(cd))
           case '00000    cd    rs 00111 111111'   => CGet(CGetPCCSetOffset(cd,rs))

           -- Capability-Modification Instructions
           case '00000    cd    cs    ct 001011'   => CSeal(cd, cs, ct)
           case '00000    cd    cs    ct 001100'   => CUnseal(cd, cs, ct)
           case '00000    cd    cs    rt 001101'   => CSet(CAndPerm(cd, cs, rt))
           case '00000    cd    cs    rt 001111'   => CSet(CSetOffset(cd, cs, rt))
           case '00000    cd    cs    rt 001000'   => CSet(CSetBounds(cd, cs, rt))
           case '00000    cd    cs    rt 001001'   => CSet(CSetBoundsExact(cd, cs, rt))
           case '10100    cd    cb       length'   => CSet(CSetBoundsImmediate(cd, cb, length))
           case '00000    cd    cb 01011 111111'   => CSet(CClearTag(cd, cb))
           case '00000    cd    cb    rt 010001'   => CSet(CIncOffset(cd, cb, rt))
           case '10011    cd    cb    increment'   => CSet(CIncOffsetImmediate(cd, cb, increment))

           -- Pointer-Arithmetic Instructions
           case '00000    rd    cb    cs 010010'   => CGet(CToPtr(rd, cb, cs))
           case '00000    cd    cb    rs 010011'   => CSet(CFromPtr(cd, cb, rs))
           case '00000    rt    cb    cs 001010'   => CSub(rt, cb, cs)
           case '00000    cd    cs 01010 111111'   => CMove(cd, cs)
           case '00000    cd    cs    rs 011011'   => CMOVZ(cd, cs, rs)
           case '00000    cd    cs    rs 011100'   => CMOVN(cd, cs, rs)

           -- Pointer-Comparison Instructions
           case '00000    rd    cb    cs 010100'   => CEQ(rd, cb, cs)
           case '00000    rd    cb    cs 010101'   => CNE(rd, cb, cs)
           case '00000    rd    cb    cs 010110'   => CLT(rd, cb, cs)
           case '00000    rd    cb    cs 010111'   => CLE(rd, cb, cs)
           case '00000    rd    cb    cs 011000'   => CLTU(rd, cb, cs)
           case '00000    rd    cb    cs 011001'   => CLEU(rd, cb, cs)
           case '00000    rd    cb    cs 011010'   => CEXEQ(rd, cb, cs)
           case '00000    rd    cb    cs 100001'   => CNEXEQ(rd, cb, cs)

           -- Exception-Handling Instructions
           case '00000    rd 00001 11111 111111'   => CGet(CGetCause(rd))
           case '00000    rs 00010 11111 111111'   => CSet(CSetCause(rs))

           -- Control-Flow Instructions
           case '01001    cd             offset'   => CBTU(cd, offset)
           case '01010    cd             offset'   => CBTS(cd, offset)
           case '00000    cb 00011 11111 111111'   => CJR(cb)
           case '00000    cd    cb 01100 111111'   => CJALR(cd, cb)
           case '00101    cs    cb 00000 000001'   => CCallFast(cs, cb)
           case '00101 00000 00000 11111 111111'   => CReturn -- TODO that's slightly nasty... Fix the spec a bit?
           case '00101    cs    cb     selector'   => CCall(cs, cb, selector)

           -- Assertion Instructions
           case '00000    cs    rt 01000 111111'   => CCheck(CCheckPerm(cs, rt))
           case '00000    cs    cb 01001 111111'   => CCheck(CCheckType(cs, cb))

           -- Fast Register-Clearing Instructions
           case '01111 00000               mask'   => ClearLo(mask)
           case '01111 00001               mask'   => ClearHi(mask)
           case '01111 00010               mask'   => CClearLo(mask)
           case '01111 00011               mask'   => CClearHi(mask)
--TODO?    case '01111 00100               mask'   => FPClearLo(mask)
--TODO?    case '01111 00101               mask'   => FPClearHi(mask)

           case '10000 rd cb _ 1 s 00'          => CLLx(rd, cb, s:'00')
           case '10000 rd cb _ 1 s 01'          => CLLx(rd, cb, s:'01')
           case '10000 rd cb _ 1 s 10'          => CLLx(rd, cb, s:'10')
           case '10000 rd cb _ 1011'            => CLLx(rd, cb, '011')
           case '10000 cd cb _ 1111'            => CLLC(cd, cb)
           case '10000 rs cb rd _ 00 tt'        => CSCx(rs, cb, rd, tt)
           case '10000 cs cb rd _ 0111'         => CSCC(cs, cb, rd)
           --case '00000 rd cb ct 100000'         => CTestSubset(rd, cb, ct)
           --case '00000 cd cb ct 011101'         => CBuildCap(cd, cb, ct)
           --case '00000 cd cb ct 011110'         => CCopyType(cd, cb, ct)
           case _                               => UnknownCapInstruction
       }))

-- Bits 31:26 = 0x32
instruction LWC2Decode (rd::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
      match v
       {
           case '0 t' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b0, t)))
           case '100' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b00)))
           case '101' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b01)))
           case '110' => LWC2(CHERILWC2(CLoad(rd, cb, rt, offset, 0b1, 0b10)))
           case _     => COP2(CHERICOP2(UnknownCapInstruction))
       }

-- Bits 31:26 = 0x3a
instruction SWC2Decode (rs::reg, cb::reg, rt::reg, offset::byte, v::bits(3)) =
   match v
   {
      case '0 t' => SWC2(CHERISWC2(CStore(rs, cb, rt, offset, t)))
      case _     => COP2(CHERICOP2(UnknownCapInstruction))
   }

-- Bits 31:26 = 0x36
instruction LDC2Decode (a::reg * reg * reg * bits(11)) = LDC2(CHERILDC2(CLC(a)))
-- Bits 31:26 = 0x3e
instruction SDC2Decode (a::reg * reg * reg * bits(11)) = SDC2(CHERISDC2(CSC(a)))
