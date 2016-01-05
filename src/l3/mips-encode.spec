---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- instruction to string

string instructionToString (i::instruction) =
   match i
   {
     case Shift (SLL (0, 0, 0))          => "nop"
     case Shift (SLL (0, 0, 1))          => "ssnop"
     case Shift (SLL (rt, rd, imm5))     => op2ri ("sll",rd,rt,imm5)
     case Shift (SRL (rt, rd, imm5))     => op2ri ("srl",rd,rt,imm5)
     case Shift (SRA (rt, rd, imm5))     => op2ri ("sra",rd,rt,imm5)
     case Shift (SLLV (rs, rt, rd))      => op3r ("sllv",rd,rt,rs)
     case Shift (SRLV (rs, rt, rd))      => op3r ("srlv",rd,rt,rs)
     case Shift (SRAV (rs, rt, rd))      => op3r ("srav",rd,rt,rs)
     case Branch (JR (rs))               => op1r ("jr",rs)
     case Branch (JALR (rs, rd))         => op2r ("jalr",rd,rs)
     case MultDiv (MFHI (rd))            => op1r ("mfhi",rd)
     case MultDiv (MTHI (rd))            => op1r ("mthi",rd)
     case MultDiv (MFLO (rs))            => op1r ("mflo",rs)
     case MultDiv (MTLO (rs))            => op1r ("mtlo",rs)
     case Shift (DSLLV (rs, rt, rd))     => op3r ("dsllv",rd,rt,rs)
     case Shift (DSRLV (rs, rt, rd))     => op3r ("dsrlv",rd,rt,rs)
     case Shift (DSRAV (rs, rt, rd))     => op3r ("dsrav",rd,rt,rs)
     case MultDiv (MADD (rs, rt))        => op2r ("madd",rs,rt)
     case MultDiv (MADDU (rs, rt))       => op2r ("maddu",rs,rt)
     case MultDiv (MSUB (rs, rt))        => op2r ("msub",rs,rt)
     case MultDiv (MSUBU (rs, rt))       => op2r ("msubu",rs,rt)
     case MultDiv (MUL (rs, rt, rd))     => op3r ("mul",rd,rs,rt)
     case MultDiv (MULT (rs, rt))        => op2r ("mult",rs,rt)
     case MultDiv (MULTU (rs, rt))       => op2r ("multu",rs,rt)
     case MultDiv (DIV (rs, rt))         => op2r ("div",rs,rt)
     case MultDiv (DIVU (rs, rt))        => op2r ("divu",rs,rt)
     case MultDiv (DMULT (rs, rt))       => op2r ("dmult",rs,rt)
     case MultDiv (DMULTU (rs, rt))      => op2r ("dmultu",rs,rt)
     case MultDiv (DDIV (rs, rt))        => op2r ("ddiv",rs,rt)
     case MultDiv (DDIVU (rs, rt))       => op2r ("ddivu",rs,rt)
     case ArithR (MOVN (rs, rt, rd))     => op3r ("movn",rd,rs,rt)
     case ArithR (MOVZ (rs, rt, rd))     => op3r ("movz",rd,rs,rt)
     case ArithR (ADD (rs, rt, rd))      => op3r ("add",rd,rs,rt)
     case ArithR (ADDU (rs, rt, rd))     => op3r ("addu",rd,rs,rt)
     case ArithR (SUB (rs, rt, rd))      => op3r ("sub",rd,rs,rt)
     case ArithR (SUBU (rs, rt, rd))     => op3r ("subu",rd,rs,rt)
     case ArithR (AND (rs, rt, rd))      => op3r ("and",rd,rs,rt)
     case ArithR (OR (rs, rt, rd))       => op3r ("or",rd,rs,rt)
     case ArithR (XOR (rs, rt, rd))      => op3r ("xor",rd,rs,rt)
     case ArithR (NOR (rs, rt, rd))      => op3r ("nor",rd,rs,rt)
     case ArithR (SLT (rs, rt, rd))      => op3r ("slt",rd,rs,rt)
     case ArithR (SLTU (rs, rt, rd))     => op3r ("sltu",rd,rs,rt)
     case ArithR (DADD (rs, rt, rd))     => op3r ("dadd",rd,rs,rt)
     case ArithR (DADDU (rs, rt, rd))    => op3r ("daddu",rd,rs,rt)
     case ArithR (DSUB (rs, rt, rd))     => op3r ("dsub",rd,rs,rt)
     case ArithR (DSUBU (rs, rt, rd))    => op3r ("dsubu",rd,rs,rt)
     case Trap (TGE (rs, rt))            => op2r ("tge",rs,rt)
     case Trap (TGEU (rs, rt))           => op2r ("tgeu",rs,rt)
     case Trap (TLT (rs, rt))            => op2r ("tlt",rs,rt)
     case Trap (TLTU (rs, rt))           => op2r ("tltu",rs,rt)
     case Trap (TEQ (rs, rt))            => op2r ("teq",rs,rt)
     case Trap (TNE (rs, rt))            => op2r ("tne",rs,rt)
     case Shift (DSLL (rt, rd, imm5))    => op2ri ("dsll",rd,rt,imm5)
     case Shift (DSRL (rt, rd, imm5))    => op2ri ("dsrl",rd,rt,imm5)
     case Shift (DSRA (rt, rd, imm5))    => op2ri ("dsra",rd,rt,imm5)
     case Shift (DSLL32 (rt, rd, imm5))  => op2ri ("dsll32",rd,rt,imm5)
     case Shift (DSRL32 (rt, rd, imm5))  => op2ri ("dsrl32",rd,rt,imm5)
     case Shift (DSRA32 (rt, rd, imm5))  => op2ri ("dsra32",rd,rt,imm5)
     case Branch (BLTZ (rs, imm))        => op1ri ("bltz",rs,imm)
     case Branch (BGEZ (rs, imm))        => op1ri ("bgez",rs,imm)
     case Branch (BLTZL (rs, imm))       => op1ri ("bltzl",rs,imm)
     case Branch (BGEZL (rs, imm))       => op1ri ("bgezl",rs,imm)
     case Trap (TGEI (rs, imm))          => op1ri ("tgei",rs,imm)
     case Trap (TGEIU (rs, imm))         => op1ri ("tgeiu",rs,imm)
     case Trap (TLTI (rs, imm))          => op1ri ("tlti",rs,imm)
     case Trap (TLTIU (rs, imm))         => op1ri ("tltiu",rs,imm)
     case Trap (TEQI (rs, imm))          => op1ri ("teqi",rs,imm)
     case Trap (TNEI (rs, imm))          => op1ri ("tnei",rs,imm)
     case Branch (BLTZAL (rs, imm))      => op1ri ("bltzal",rs,imm)
     case Branch (BGEZAL (rs, imm))      => op1ri ("bgezal",rs,imm)
     case Branch (BLTZALL (rs, imm))     => op1ri ("bltzall",rs,imm)
     case Branch (BGEZALL (rs, imm))     => op1ri ("bgezall",rs,imm)
     case Branch (J (imm))               => op1i ("j",imm)
     case Branch (JAL (imm))             => op1i ("jal",imm)
     case CP (MFC0 (rt, rd, sel))        => op2roi ("mfc0",rt,rd,sel)
     case CP (DMFC0 (rt, rd, sel))       => op2roi ("dmfc0",rt,rd,sel)
     case CP (MTC0 (rt, rd, sel))        => op2roi ("mtc0",rt,rd,sel)
     case CP (DMTC0 (rt, rd, sel))       => op2roi ("dmtc0",rt,rd,sel)
     case Branch (BEQ (0, 0, imm))       => op1i ("b",imm)
     case Branch (BEQ (rs, rt, imm))     => op2ri ("beq",rs,rt,imm)
     case Branch (BNE (rs, rt, imm))     => op2ri ("bne",rs,rt,imm)
     case Branch (BLEZ (rs, imm))        => op1ri ("blez",rs,imm)
     case Branch (BGTZ (rs, imm))        => op1ri ("bgtz",rs,imm)
     case ArithI (ADDI (rs, rt, imm))    => op2ri ("addi",rt,rs,imm)
     case ArithI (ADDIU (rs, rt, imm))   => op2ri ("addiu",rt,rs,imm)
     case ArithI (SLTI (rs, rt, imm))    => op2ri ("slti",rt,rs,imm)
     case ArithI (SLTIU (rs, rt, imm))   => op2ri ("sltiu",rt,rs,imm)
     case ArithI (ANDI (rs, rt, imm))    => op2ri ("andi",rt,rs,imm)
     case ArithI (ORI (rs, rt, imm))     => op2ri ("ori",rt,rs,imm)
     case ArithI (XORI (rs, rt, imm))    => op2ri ("xori",rt,rs,imm)
     case ArithI (LUI (rt, imm))         => op1ri ("lui",rt,imm)
     case Branch (BEQL (rs, rt, imm))    => op2ri ("beql",rs,rt,imm)
     case Branch (BNEL (rs, rt, imm))    => op2ri ("bnel",rs,rt,imm)
     case Branch (BLEZL (rs, imm))       => op1ri ("blezl",rs,imm)
     case Branch (BGTZL (rs, imm))       => op1ri ("bgtzl",rs,imm)
     case ArithI (DADDI (rs, rt, imm))   => op2ri ("daddi",rt,rs,imm)
     case ArithI (DADDIU (rs, rt, imm))  => op2ri ("daddiu",rt,rs,imm)
     case Load (LDL (rs, rt, imm))       => opmem ("ldl",rt,rs,imm)
     case Load (LDR (rs, rt, imm))       => opmem ("ldr",rt,rs,imm)
     case Load (LB (rs, rt, imm))        => opmem ("lb",rt,rs,imm)
     case Load (LH (rs, rt, imm))        => opmem ("lh",rt,rs,imm)
     case Load (LWL (rs, rt, imm))       => opmem ("lwl",rt,rs,imm)
     case Load (LW (rs, rt, imm))        => opmem ("lw",rt,rs,imm)
     case Load (LBU (rs, rt, imm))       => opmem ("lbu",rt,rs,imm)
     case Load (LHU (rs, rt, imm))       => opmem ("lhu",rt,rs,imm)
     case Load (LWR (rs, rt, imm))       => opmem ("lwr",rt,rs,imm)
     case Load (LWU (rs, rt, imm))       => opmem ("lwu",rt,rs,imm)
     case Store (SB (rs, rt, imm))       => opmem ("sb",rt,rs,imm)
     case Store (SH (rs, rt, imm))       => opmem ("sh",rt,rs,imm)
     case Store (SWL (rs, rt, imm))      => opmem ("swl",rt,rs,imm)
     case Store (SW (rs, rt, imm))       => opmem ("sw",rt,rs,imm)
     case Store (SDL (rs, rt, imm))      => opmem ("sdl",rt,rs,imm)
     case Store (SDR (rs, rt, imm))      => opmem ("sdr",rt,rs,imm)
     case Store (SWR (rs, rt, imm))      => opmem ("swr",rt,rs,imm)
     case Load (LL (rs, rt, imm))        => opmem ("ll",rt,rs,imm)
     case Load (LLD (rs, rt, imm))       => opmem ("lld",rt,rs,imm)
     case Load (LD (rs, rt, imm))        => opmem ("ld",rt,rs,imm)
     case Store (SC (rs, rt, imm))       => opmem ("sc",rt,rs,imm)
     case Store (SCD (rs, rt, imm))      => opmem ("scd",rt,rs,imm)
     case Store (SD (rs, rt, imm))       => opmem ("sd",rt,rs,imm)
     case CACHE (rs, opn, imm)           => opmem ("cache",opn,rs,imm)
     case SYSCALL                        => "syscall"
     case BREAK                          => "break"
     case SYNC (imm5)                    => "sync ":[imm5]
     case TLBR                           => "tlbr"
     case TLBWI                          => "tlbwi"
     case TLBWR                          => "tlbwr"
     case TLBP                           => "tlbp"
     case ERET                           => "eret"
     case RDHWR (rt, rd)                 => op2r ("rdhwr", rt, rd)
     case WAIT                           => "wait"
     case COP1 (_)                       => COP1InstructionToString(i)
     case COP2 (_)                       => COP2InstructionToString(i)
     case LWC2 (_)                       => LWC2InstructionToString(i)
     case LDC2 (_)                       => LDC2InstructionToString(i)
     case SWC2 (_)                       => SWC2InstructionToString(i)
     case SDC2 (_)                       => SDC2InstructionToString(i)
     case Unpredictable                  => "???"
     case ReservedInstruction            => "???"
   }

word Encode (i::instruction) =
   match i
   {
     case Shift (SLL (rt, rd, imm5))     => form1 (0, rt, rd, imm5, '000000')
     case Shift (SRL (rt, rd, imm5))     => form1 (0, rt, rd, imm5, '000010')
     case Shift (SRA (rt, rd, imm5))     => form1 (0, rt, rd, imm5, '000011')
     case Shift (SLLV (rs, rt, rd))      => form1 (rs, rt, rd, 0, '000100')
     case Shift (SRLV (rs, rt, rd))      => form1 (rs, rt, rd, 0, '000110')
     case Shift (SRAV (rs, rt, rd))      => form1 (rs, rt, rd, 0, '000111')
     case Branch (JR (rs))               => form1 (rs, 0, 0, 0, '001000')
     case Branch (JALR (rs, rd))         => form1 (rs, 0, rd, 0, '001001')
     case MultDiv (MFHI (rd))            => form1 (0, 0, rd, 0, '010000')
     case MultDiv (MTHI (rs))            => form1 (rs, 0, 0, 0, '010001')
     case MultDiv (MFLO (rd))            => form1 (0, 0, rd, 0, '010010')
     case MultDiv (MTLO (rs))            => form1 (rs, 0, 0, 0, '010011')
     case Shift (DSLLV (rs, rt, rd))     => form1 (rs, rt, rd, 0, '010100')
     case Shift (DSRLV (rs, rt, rd))     => form1 (rs, rt, rd, 0, '010110')
     case Shift (DSRAV (rs, rt, rd))     => form1 (rs, rt, rd, 0, '010111')
     case MultDiv (MADD (rs, rt))        => form5 (rs, rt, 0, '000000')
     case MultDiv (MADDU (rs, rt))       => form5 (rs, rt, 0, '000001')
     case MultDiv (MSUB (rs, rt))        => form5 (rs, rt, 0, '000100')
     case MultDiv (MSUBU (rs, rt))       => form5 (rs, rt, 0, '000101')
     case MultDiv (MUL (rs, rt, rd))     => form5 (rs, rt, rd, '000010')
     case MultDiv (MULT (rs, rt))        => form1 (rs, rt, 0, 0, '011000')
     case MultDiv (MULTU (rs, rt))       => form1 (rs, rt, 0, 0, '011001')
     case MultDiv (DIV (rs, rt))         => form1 (rs, rt, 0, 0, '011010')
     case MultDiv (DIVU (rs, rt))        => form1 (rs, rt, 0, 0, '011011')
     case MultDiv (DMULT (rs, rt))       => form1 (rs, rt, 0, 0, '011100')
     case MultDiv (DMULTU (rs, rt))      => form1 (rs, rt, 0, 0, '011101')
     case MultDiv (DDIV (rs, rt))        => form1 (rs, rt, 0, 0, '011110')
     case MultDiv (DDIVU (rs, rt))       => form1 (rs, rt, 0, 0, '011111')
     case ArithR (MOVZ (rs, rt, rd))     => form1 (rs, rt, rd, 0, '001010')
     case ArithR (MOVN (rs, rt, rd))     => form1 (rs, rt, rd, 0, '001011')
     case ArithR (ADD (rs, rt, rd))      => form1 (rs, rt, rd, 0, '100000')
     case ArithR (ADDU (rs, rt, rd))     => form1 (rs, rt, rd, 0, '100001')
     case ArithR (SUB (rs, rt, rd))      => form1 (rs, rt, rd, 0, '100010')
     case ArithR (SUBU (rs, rt, rd))     => form1 (rs, rt, rd, 0, '100011')
     case ArithR (AND (rs, rt, rd))      => form1 (rs, rt, rd, 0, '100100')
     case ArithR (OR (rs, rt, rd))       => form1 (rs, rt, rd, 0, '100101')
     case ArithR (XOR (rs, rt, rd))      => form1 (rs, rt, rd, 0, '100110')
     case ArithR (NOR (rs, rt, rd))      => form1 (rs, rt, rd, 0, '100111')
     case ArithR (SLT (rs, rt, rd))      => form1 (rs, rt, rd, 0, '101010')
     case ArithR (SLTU (rs, rt, rd))     => form1 (rs, rt, rd, 0, '101011')
     case ArithR (DADD (rs, rt, rd))     => form1 (rs, rt, rd, 0, '101100')
     case ArithR (DADDU (rs, rt, rd))    => form1 (rs, rt, rd, 0, '101101')
     case ArithR (DSUB (rs, rt, rd))     => form1 (rs, rt, rd, 0, '101110')
     case ArithR (DSUBU (rs, rt, rd))    => form1 (rs, rt, rd, 0, '101111')
     case Trap (TGE (rs, rt))            => form1 (rs, rt, 0, 0, '110000')
     case Trap (TGEU (rs, rt))           => form1 (rs, rt, 0, 0, '110001')
     case Trap (TLT (rs, rt))            => form1 (rs, rt, 0, 0, '110010')
     case Trap (TLTU (rs, rt))           => form1 (rs, rt, 0, 0, '110011')
     case Trap (TEQ (rs, rt))            => form1 (rs, rt, 0, 0, '110100')
     case Trap (TNE (rs, rt))            => form1 (rs, rt, 0, 0, '110110')
     case Shift (DSLL (rt, rd, imm5))    => form1 (0, rt, rd, imm5, '111000')
     case Shift (DSRL (rt, rd, imm5))    => form1 (0, rt, rd, imm5, '111010')
     case Shift (DSRA (rt, rd, imm5))    => form1 (0, rt, rd, imm5, '111011')
     case Shift (DSLL32 (rt, rd, imm5))  => form1 (0, rt, rd, imm5, '111100')
     case Shift (DSRL32 (rt, rd, imm5))  => form1 (0, rt, rd, imm5, '111110')
     case Shift (DSRA32 (rt, rd, imm5))  => form1 (0, rt, rd, imm5, '111111')
     case Branch (BLTZ (rs, imm))        => form2 (rs, '00000', imm)
     case Branch (BGEZ (rs, imm))        => form2 (rs, '00001', imm)
     case Branch (BLTZL (rs, imm))       => form2 (rs, '00010', imm)
     case Branch (BGEZL (rs, imm))       => form2 (rs, '00011', imm)
     case Trap (TGEI (rs, imm))          => form2 (rs, '01000', imm)
     case Trap (TGEIU (rs, imm))         => form2 (rs, '01001', imm)
     case Trap (TLTI (rs, imm))          => form2 (rs, '01010', imm)
     case Trap (TLTIU (rs, imm))         => form2 (rs, '01011', imm)
     case Trap (TEQI (rs, imm))          => form2 (rs, '01100', imm)
     case Trap (TNEI (rs, imm))          => form2 (rs, '01110', imm)
     case Branch (BLTZAL (rs, imm))      => form2 (rs, '10000', imm)
     case Branch (BGEZAL (rs, imm))      => form2 (rs, '10001', imm)
     case Branch (BLTZALL (rs, imm))     => form2 (rs, '10010', imm)
     case Branch (BGEZALL (rs, imm))     => form2 (rs, '10011', imm)
     case Branch (J (imm))               => '000010' : imm
     case Branch (JAL (imm))             => '000011' : imm
     case CP (MFC0 (rt, rd, sel))        => form3 ('00000', rt, rd, sel)
     case CP (DMFC0 (rt, rd, sel))       => form3 ('00001', rt, rd, sel)
     case CP (MTC0 (rt, rd, sel))        => form3 ('00100', rt, rd, sel)
     case CP (DMTC0 (rt, rd, sel))       => form3 ('00101', rt, rd, sel)
     case Branch (BEQ (rs, rt, imm))     => form4 ('000100', rs, rt, imm)
     case Branch (BNE (rs, rt, imm))     => form4 ('000101', rs, rt, imm)
     case Branch (BLEZ (rs, imm))        => form4 ('000110', rs, 0, imm)
     case Branch (BGTZ (rs, imm))        => form4 ('000111', rs, 0, imm)
     case ArithI (ADDI (rs, rt, imm))    => form4 ('001000', rs, rt, imm)
     case ArithI (ADDIU (rs, rt, imm))   => form4 ('001001', rs, rt, imm)
     case ArithI (SLTI (rs, rt, imm))    => form4 ('001010', rs, rt, imm)
     case ArithI (SLTIU (rs, rt, imm))   => form4 ('001011', rs, rt, imm)
     case ArithI (ANDI (rs, rt, imm))    => form4 ('001100', rs, rt, imm)
     case ArithI (ORI (rs, rt, imm))     => form4 ('001101', rs, rt, imm)
     case ArithI (XORI (rs, rt, imm))    => form4 ('001110', rs, rt, imm)
     case ArithI (LUI (rt, imm))         => form4 ('001111', 0, rt, imm)
     case Branch (BEQL (rs, rt, imm))    => form4 ('010100', rs, rt, imm)
     case Branch (BNEL (rs, rt, imm))    => form4 ('010101', rs, rt, imm)
     case Branch (BLEZL (rs, imm))       => form4 ('010110', rs, 0, imm)
     case Branch (BGTZL (rs, imm))       => form4 ('010111', rs, 0, imm)
     case ArithI (DADDI (rs, rt, imm))   => form4 ('011000', rs, rt, imm)
     case ArithI (DADDIU (rs, rt, imm))  => form4 ('011001', rs, rt, imm)
     case Load (LDL (rs, rt, imm))       => form4 ('011010', rs, rt, imm)
     case Load (LDR (rs, rt, imm))       => form4 ('011011', rs, rt, imm)
     case Load (LB (rs, rt, imm))        => form4 ('100000', rs, rt, imm)
     case Load (LH (rs, rt, imm))        => form4 ('100001', rs, rt, imm)
     case Load (LWL (rs, rt, imm))       => form4 ('100010', rs, rt, imm)
     case Load (LW (rs, rt, imm))        => form4 ('100011', rs, rt, imm)
     case Load (LBU (rs, rt, imm))       => form4 ('100100', rs, rt, imm)
     case Load (LHU (rs, rt, imm))       => form4 ('100101', rs, rt, imm)
     case Load (LWR (rs, rt, imm))       => form4 ('100110', rs, rt, imm)
     case Load (LWU (rs, rt, imm))       => form4 ('100111', rs, rt, imm)
     case Store (SB (rs, rt, imm))       => form4 ('101000', rs, rt, imm)
     case Store (SH (rs, rt, imm))       => form4 ('101001', rs, rt, imm)
     case Store (SWL (rs, rt, imm))      => form4 ('101010', rs, rt, imm)
     case Store (SW (rs, rt, imm))       => form4 ('101011', rs, rt, imm)
     case Store (SDL (rs, rt, imm))      => form4 ('101100', rs, rt, imm)
     case Store (SDR (rs, rt, imm))      => form4 ('101101', rs, rt, imm)
     case Store (SWR (rs, rt, imm))      => form4 ('101110', rs, rt, imm)
     case Load (LL (rs, rt, imm))        => form4 ('110000', rs, rt, imm)
     case Load (LLD (rs, rt, imm))       => form4 ('110100', rs, rt, imm)
     case Load (LD (rs, rt, imm))        => form4 ('110111', rs, rt, imm)
     case Store (SC (rs, rt, imm))       => form4 ('111000', rs, rt, imm)
     case Store (SCD (rs, rt, imm))      => form4 ('111100', rs, rt, imm)
     case Store (SD (rs, rt, imm))       => form4 ('111111', rs, rt, imm)
     case CACHE (rs, opn, imm)           => form4 ('101111', rs, opn, imm)
     case SYSCALL                        => ['001100']
     case BREAK                          => ['001101']
     case SYNC (imm5)                    => [imm5 : '001111']
     case TLBR                           => '01000010000000000000000000000001'
     case TLBWI                          => '01000010000000000000000000000010'
     case TLBWR                          => '01000010000000000000000000000110'
     case TLBP                           => '01000010000000000000000000001000'
     case ERET                           => '01000010000000000000000000011000'
     case RDHWR (rt, rd)                 => form6 (rt, rd, '111011')
     case WAIT                           => '01000010000000000000000000100000'
     case COP1 (_)                       => COP1Encode(i)
     case COP2 (_)                       => COP2Encode(i)
     case LWC2 (_)                       => LWC2Encode(i)
     case LDC2 (_)                       => LDC2Encode(i)
     case SWC2 (_)                       => SWC2Encode(i)
     case SDC2 (_)                       => SDC2Encode(i)
     case Unpredictable                  => '00000111111100000000000000000000'
     case ReservedInstruction            => 0
   }
