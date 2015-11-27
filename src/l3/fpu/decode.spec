---------------------------------------------------------------------------
-- Instruction decoder for MIPS ISA floating point instructions
-- (c) 2015 Michael Roe
---------------------------------------------------------------------------

instruction COP1Decode (v::bits(26)) =
    COP1
    (
        match v
        {
            case '00000 rt fs 00000000000'    => MFC1(rt, fs)
            case '00001 rt fs 00000000000'    => DMFC1(rt, fs)
            case '00010 rt fs 00000000000'    => CFC1(rt, fs)
            case '00100 rt fs 00000000000'    => MTC1(rt, fs)
            case '00101 rt fs 00000000000'    => DMTC1(rt, fs)
            case '00110 rt fs 00000000000'    => CTC1(rt, fs)

            case '10000 ft fs 00000 110010'   => C_EQ_S(ft, fs)

            case '10001 ft fs fd 000000'      => ADD_D(ft, fs, fd)
            case '10001 ft fs fd 000001'      => SUB_D(ft, fs, fd)
            case '10001 ft fs fd 000010'      => MUL_D(ft, fs, fd)
            case '10001 ft fs fd 000011'      => DIV_D(ft, fs, fd)
            -- 100 is SQRT_D
            case '10001 00000 fs fd 000101'   => ABS_D(fs, fd)
            case '10001 00000 fs fd 000110'   => MOV_D(fs, fd)
            case '10001 00000 fs fd 000111'   => NEG_D(fs, fd)
            case '10001 00000 fs fd 001000'   => ROUND_L_D(fd, fs)
            case '10001 00000 fs fd 001001'   => TRUNC_L_D(fd, fs)
            case '10001 00000 fs fd 001010'   => CEIL_L_D(fd, fs)
            case '10001 00000 fs fd 001011'   => FLOOR_L_D(fd, fs)
            case '10001 00000 fs fd 001100'   => ROUND_W_D(fd, fs)
            case '10001 00000 fs fd 001101'   => TRUNC_W_D(fd, fs)
            case '10001 00000 fs fd 001110'   => CEIL_W_D(fd, fs)
            case '10001 00000 fs fd 001111'   => FLOOR_W_D(fd, fs)
            case '10001 rt fs fd    010011'   => MOVN_D(fd, fs, rt)
            case '10001 ft fs 00000 110000'   => C_F_D(ft, fs)
            case '10001 ft fs 00000 110001'   => C_UN_D(ft, fs)
            case '10001 ft fs 00000 110010'   => C_EQ_D(ft, fs)
            case '10001 ft fs 00000 110011'   => C_UEQ_D(ft, fs)
            case '10001 ft fs 00000 110100'   => C_OLT_D(ft, fs)
            case '10001 ft fs 00000 110101'   => C_ULT_D(ft, fs)
            case '10001 ft fs 00000 110110'   => C_OLE_D(ft, fs)
            case '10001 ft fs 00000 110111'   => C_ULE_D(ft, fs)

            case '10100 00000 fs fd 100001'   => CVT_D_W(fd, fs)
            case '10101 00000 fs fd 100001'   => CVT_D_L(fd, fs)

            case _                            => UnknownFPInstruction
        }
    )
