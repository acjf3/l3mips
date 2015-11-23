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
            case '00100 rt fs 00000000000'    => MTC1(rt, fs)
            case '00101 rt fs 00000000000'    => DMTC1(rt, fs)

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

            case '10101 00000 fs fd 100001'   => CVT_D_L(fd, fs)

            case _                            => UnknownFPInstruction
        }
    )
