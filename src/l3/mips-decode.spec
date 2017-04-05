---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- Instruction decoding
--================================================

inline instruction Decode000_000 (s::int, t::int, i::int) =
{
   i, r = QuotRem (i, 0x40);
   d, i = QuotRem (i, 0x20);
   rs = [s];
   rt = [t];
   rd = [d];
   imm5 = [i];
   match s, t, d, i, r
   {
      case 0, _, _, _, 0b000_000 => Shift (SLL (rt, rd, imm5))
      case _, _, _, 0, 0b000_001 => MOVCIDecode(rs, rt, rd)
      case 0, _, _, _, 0b000_010 => Shift (SRL (rt, rd, imm5))
      case 0, _, _, _, 0b000_011 => Shift (SRA (rt, rd, imm5))
      case _, _, _, 0, 0b000_100 => Shift (SLLV (rs, rt, rd))
      case _, _, _, 0, 0b000_110 => Shift (SRLV (rs, rt, rd))
      case _, _, _, 0, 0b000_111 => Shift (SRAV (rs, rt, rd))
      case _, 0, 0, _, 0b001_000 => Branch (JR (rs))
      case _, 0, _, _, 0b001_001 => Branch (JALR (rs, rd))
      case _, _, _, 0, 0b001_010 => ArithR (MOVZ (rs, rt, rd))
      case _, _, _, 0, 0b001_011 => ArithR (MOVN (rs, rt, rd))
      case 0, _, _, _, 0b001_100 => SYSCALL
      case 0, _, _, _, 0b001_101 => BREAK
      case 0, 0, 0, _, 0b001_111 => SYNC (imm5)
      case 0, 0, _, 0, 0b010_000 => MultDiv (MFHI (rd))
      case _, 0, 0, 0, 0b010_001 => MultDiv (MTHI (rs))
      case 0, 0, _, 0, 0b010_010 => MultDiv (MFLO (rd))
      case _, 0, 0, 0, 0b010_011 => MultDiv (MTLO (rs))
      case _, _, _, 0, 0b010_100 => Shift (DSLLV (rs, rt, rd))
      case _, _, _, 0, 0b010_110 => Shift (DSRLV (rs, rt, rd))
      case _, _, _, 0, 0b010_111 => Shift (DSRAV (rs, rt, rd))
      case _, _, 0, 0, 0b011_000 => MultDiv (MULT (rs, rt))
      case _, _, 0, 0, 0b011_001 => MultDiv (MULTU (rs, rt))
      case _, _, 0, 0, 0b011_010 => MultDiv (DIV (rs, rt))
      case _, _, 0, 0, 0b011_011 => MultDiv (DIVU (rs, rt))
      case _, _, 0, 0, 0b011_100 => MultDiv (DMULT (rs, rt))
      case _, _, 0, 0, 0b011_101 => MultDiv (DMULTU (rs, rt))
      case _, _, 0, 0, 0b011_110 => MultDiv (DDIV (rs, rt))
      case _, _, 0, 0, 0b011_111 => MultDiv (DDIVU (rs, rt))
      case _, _, _, 0, 0b100_000 => ArithR (ADD (rs, rt, rd))
      case _, _, _, 0, 0b100_001 => ArithR (ADDU (rs, rt, rd))
      case _, _, _, 0, 0b100_010 => ArithR (SUB (rs, rt, rd))
      case _, _, _, 0, 0b100_011 => ArithR (SUBU (rs, rt, rd))
      case _, _, _, 0, 0b100_100 => ArithR (AND (rs, rt, rd))
      case _, _, _, 0, 0b100_101 => ArithR (OR (rs, rt, rd))
      case _, _, _, 0, 0b100_110 => ArithR (XOR (rs, rt, rd))
      case _, _, _, 0, 0b100_111 => ArithR (NOR (rs, rt, rd))
      case _, _, _, 0, 0b101_010 => ArithR (SLT (rs, rt, rd))
      case _, _, _, 0, 0b101_011 => ArithR (SLTU (rs, rt, rd))
      case _, _, _, 0, 0b101_100 => ArithR (DADD (rs, rt, rd))
      case _, _, _, 0, 0b101_101 => ArithR (DADDU (rs, rt, rd))
      case _, _, _, 0, 0b101_110 => ArithR (DSUB (rs, rt, rd))
      case _, _, _, 0, 0b101_111 => ArithR (DSUBU (rs, rt, rd))
      case _, _, _, _, 0b110_000 => Trap (TGE (rs, rt))
      case _, _, _, _, 0b110_001 => Trap (TGEU (rs, rt))
      case _, _, _, _, 0b110_010 => Trap (TLT (rs, rt))
      case _, _, _, _, 0b110_011 => Trap (TLTU (rs, rt))
      case _, _, _, _, 0b110_100 => Trap (TEQ (rs, rt))
      case _, _, _, _, 0b110_110 => Trap (TNE (rs, rt))
      case 0, _, _, _, 0b111_000 => Shift (DSLL (rt, rd, imm5))
      case 0, _, _, _, 0b111_010 => Shift (DSRL (rt, rd, imm5))
      case 0, _, _, _, 0b111_011 => Shift (DSRA (rt, rd, imm5))
      case 0, _, _, _, 0b111_100 => Shift (DSLL32 (rt, rd, imm5))
      case 0, _, _, _, 0b111_110 => Shift (DSRL32 (rt, rd, imm5))
      case 0, _, _, _, 0b111_111 => Shift (DSRA32 (rt, rd, imm5))
      case _ => ReservedInstruction
   }
}

inline instruction Decode000_001 (s::int, r::int, i::int) =
{
   immediate = [i];
   rs = [s];
   match r
   {
      case 0b00_000 => Branch (BLTZ (rs, immediate))
      case 0b00_001 => Branch (BGEZ (rs, immediate))
      case 0b00_010 => Branch (BLTZL (rs, immediate))
      case 0b00_011 => Branch (BGEZL (rs, immediate))
      case 0b01_000 => Trap (TGEI (rs, immediate))
      case 0b01_001 => Trap (TGEIU (rs, immediate))
      case 0b01_010 => Trap (TLTI (rs, immediate))
      case 0b01_011 => Trap (TLTIU (rs, immediate))
      case 0b01_100 => Trap (TEQI (rs, immediate))
      case 0b01_110 => Trap (TNEI (rs, immediate))
      case _ =>
      {
         q, r = QuotRem (r, 4);
         if q == 0b100 then
            if s == 0b11111 then
               Unpredictable
            else match r
                 {
                  case 0b00 => Branch (BLTZAL (rs, immediate))
                  case 0b01 => Branch (BGEZAL (rs, immediate))
                  case 0b10 => Branch (BLTZALL (rs, immediate))
                  case    _ => Branch (BGEZALL (rs, immediate))
                 }
         else ReservedInstruction
      }
   }
}

inline instruction Decode010_000 (r::int) =
{
   q, r = QuotRem (r, 0x1000000);
   match q
   {
      case 0b00 =>
      {
         q, s = QuotRem (r, 8);
         q, i = QuotRem (q, 0x100);
         q, d = QuotRem (q, 0x20);
         q, t = QuotRem (q, 0x20);
         rt = [t];
         rd = [d];
         sel = [s];
         match q, i
         {
            case 0b000, 0 => CP (MFC0 (rt, rd, sel))
            case 0b001, 0 => CP (DMFC0 (rt, rd, sel))
            case 0b100, 0 => CP (MTC0 (rt, rd, sel))
            case 0b101, 0 => CP (DMTC0 (rt, rd, sel))
            case _ => ReservedInstruction
         }
      }
      case 0b10 =>
         match r
         {
            case 0b000001 => TLBR
            case 0b000010 => TLBWI
            case 0b000110 => TLBWR
            case 0b001000 => TLBP
            case 0b011000 => ERET
            case 0b100000 => WAIT
            case _ => ReservedInstruction
         }
      case _ => ReservedInstruction
   }
}

inline instruction Decode011_100 (s::int, t::int, i::int) =
{
   d, i = QuotRem (i, 0x800);
   rs = [s];
   rt = [t];
   rd = [d];
   match d, i
   {
      case 0, 0b000 => MultDiv (MADD (rs, rt))
      case 0, 0b001 => MultDiv (MADDU (rs, rt))
      case 0, 0b100 => MultDiv (MSUB (rs, rt))
      case 0, 0b101 => MultDiv (MSUBU (rs, rt))
      case _, 0b010 => MultDiv (MUL (rs, rt, rd))
      case _ => ReservedInstruction
   }
}

inline instruction SimpleDecode (w::word) =
   match w
   {
      case '000 000 rs rt rd 00000 000 001' => MOVCIDecode (rs, rt, rd)
      case '000 000 00000 rt rd imm5 000 000' => Shift (SLL (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 000 010' => Shift (SRL (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 000 011' => Shift (SRA (rt, rd, imm5))
      case '000 000 rs rt rd 00000 000 100' => Shift (SLLV (rs, rt, rd))
      case '000 000 rs rt rd 00000 000 110' => Shift (SRLV (rs, rt, rd))
      case '000 000 rs rt rd 00000 000 111' => Shift (SRAV (rs, rt, rd))
      case '000 000 rs 00000 00000 hint 001 000' => Branch (JR (rs))
      case '000 000 rs 00000 rd hint 001 001' => Branch (JALR (rs, rd))
      case '000 000 rs rt rd 00000 001 010' => ArithR (MOVZ (rs, rt, rd))
      case '000 000 rs rt rd 00000 001 011' => ArithR (MOVN (rs, rt, rd))
      case '000 000 00000 code 001 100' => SYSCALL
      case '000 000 00000 code 001 101' => BREAK
      case '000 000 00000 00000 00000 imm5 001 111' => SYNC (imm5)
      case '000 000 00000 00000 rd 00000 010 000' => MultDiv (MFHI (rd))
      case '000 000 rs 00000 00000 00000 010 001' => MultDiv (MTHI (rs))
      case '000 000 00000 00000 rd 00000 010 010' => MultDiv (MFLO (rd))
      case '000 000 rs 00000 00000 00000 010 011' => MultDiv (MTLO (rs))
      case '000 000 rs rt rd 00000 010 100' => Shift (DSLLV (rs, rt, rd))
      case '000 000 rs rt rd 00000 010 110' => Shift (DSRLV (rs, rt, rd))
      case '000 000 rs rt rd 00000 010 111' => Shift (DSRAV (rs, rt, rd))
      case '000 000 rs rt 00000 00000 011 000' => MultDiv (MULT (rs, rt))
      case '000 000 rs rt 00000 00000 011 001' => MultDiv (MULTU (rs, rt))
      case '000 000 rs rt 00000 00000 011 010' => MultDiv (DIV (rs, rt))
      case '000 000 rs rt 00000 00000 011 011' => MultDiv (DIVU (rs, rt))
      case '000 000 rs rt 00000 00000 011 100' => MultDiv (DMULT (rs, rt))
      case '000 000 rs rt 00000 00000 011 101' => MultDiv (DMULTU (rs, rt))
      case '000 000 rs rt 00000 00000 011 110' => MultDiv (DDIV (rs, rt))
      case '000 000 rs rt 00000 00000 011 111' => MultDiv (DDIVU (rs, rt))
      case '000 000 rs rt rd 00000 100 000' => ArithR (ADD (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 001' => ArithR (ADDU (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 010' => ArithR (SUB (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 011' => ArithR (SUBU (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 100' => ArithR (AND (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 101' => ArithR (OR (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 110' => ArithR (XOR (rs, rt, rd))
      case '000 000 rs rt rd 00000 100 111' => ArithR (NOR (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 010' => ArithR (SLT (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 011' => ArithR (SLTU (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 100' => ArithR (DADD (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 101' => ArithR (DADDU (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 110' => ArithR (DSUB (rs, rt, rd))
      case '000 000 rs rt rd 00000 101 111' => ArithR (DSUBU (rs, rt, rd))
      case '000 000 rs rt code 110 000' => Trap (TGE (rs, rt))
      case '000 000 rs rt code 110 001' => Trap (TGEU (rs, rt))
      case '000 000 rs rt code 110 010' => Trap (TLT (rs, rt))
      case '000 000 rs rt code 110 011' => Trap (TLTU (rs, rt))
      case '000 000 rs rt code 110 100' => Trap (TEQ (rs, rt))
      case '000 000 rs rt code 110 110' => Trap (TNE (rs, rt))
      case '000 000 00000 rt rd imm5 111 000' => Shift (DSLL (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 111 010' => Shift (DSRL (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 111 011' => Shift (DSRA (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 111 100' => Shift (DSLL32 (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 111 110' => Shift (DSRL32 (rt, rd, imm5))
      case '000 000 00000 rt rd imm5 111 111' => Shift (DSRA32 (rt, rd, imm5))
      case '000 001 rs 00 000 immediate' => Branch (BLTZ (rs, immediate))
      case '000 001 rs 00 001 immediate' => Branch (BGEZ (rs, immediate))
      case '000 001 rs 00 010 immediate' => Branch (BLTZL (rs, immediate))
      case '000 001 rs 00 011 immediate' => Branch (BGEZL (rs, immediate))
      case '000 001 rs 01 000 immediate' => Trap (TGEI (rs, immediate))
      case '000 001 rs 01 001 immediate' => Trap (TGEIU (rs, immediate))
      case '000 001 rs 01 010 immediate' => Trap (TLTI (rs, immediate))
      case '000 001 rs 01 011 immediate' => Trap (TLTIU (rs, immediate))
      case '000 001 rs 01 100 immediate' => Trap (TEQI (rs, immediate))
      case '000 001 rs 01 110 immediate' => Trap (TNEI (rs, immediate))
      case '000 001 11111 10 0 _`2 immediate' => Unpredictable
      case '000 001 rs 10 000 immediate' => Branch (BLTZAL (rs, immediate))
      case '000 001 rs 10 001 immediate' => Branch (BGEZAL (rs, immediate))
      case '000 001 rs 10 010 immediate' => Branch (BLTZALL (rs, immediate))
      case '000 001 rs 10 011 immediate' => Branch (BGEZALL (rs, immediate))
      case '000 010 immediate' => Branch (J (immediate))
      case '000 011 immediate' => Branch (JAL (immediate))
      case '010 000 10000000000000000000 000001' => TLBR
      case '010 000 10000000000000000000 000010' => TLBWI
      case '010 000 10000000000000000000 000110' => TLBWR
      case '010 000 10000000000000000000 001000' => TLBP
      case '010 000 10000000000000000000 011000' => ERET
      case '010 000 00 000 rt rd 00000000 sel' => CP (MFC0 (rt, rd, sel))
      case '010 000 00 001 rt rd 00000000 sel' => CP (DMFC0 (rt, rd, sel))
      case '010 000 00 100 rt rd 00000000 sel' => CP (MTC0 (rt, rd, sel))
      case '010 000 00 101 rt rd 00000000 sel' => CP (DMTC0 (rt, rd, sel))
      case '000 110 rs 00000 immediate' => Branch (BLEZ (rs, immediate))
      case '000 111 rs 00000 immediate' => Branch (BGTZ (rs, immediate))
      case '001 111 00000 rt immediate' => ArithI (LUI (rt, immediate))
      case '010 110 rs 00000 immediate' => Branch (BLEZL (rs, immediate))
      case '010 111 rs 00000 immediate' => Branch (BGTZL (rs, immediate))
      case '011 100 rs rt 00000 00000 000000' => MultDiv (MADD (rs, rt))
      case '011 100 rs rt 00000 00000 000001' => MultDiv (MADDU (rs, rt))
      case '011 100 rs rt 00000 00000 000100' => MultDiv (MSUB (rs, rt))
      case '011 100 rs rt 00000 00000 000101' => MultDiv (MSUBU (rs, rt))
      case '011 100 rs rt rd 00000 000010' => MultDiv (MUL (rs, rt, rd))
      case '000 100 rs rt immediate' => Branch (BEQ (rs, rt, immediate))
      case '000 101 rs rt immediate' => Branch (BNE (rs, rt, immediate))
      case '001 000 rs rt immediate' => ArithI (ADDI (rs, rt, immediate))
      case '001 001 rs rt immediate' => ArithI (ADDIU (rs, rt, immediate))
      case '001 010 rs rt immediate' => ArithI (SLTI (rs, rt, immediate))
      case '001 011 rs rt immediate' => ArithI (SLTIU (rs, rt, immediate))
      case '001 100 rs rt immediate' => ArithI (ANDI (rs, rt, immediate))
      case '001 101 rs rt immediate' => ArithI (ORI (rs, rt, immediate))
      case '001 110 rs rt immediate' => ArithI (XORI (rs, rt, immediate))
      case '010 100 rs rt immediate' => Branch (BEQL (rs, rt, immediate))
      case '010 101 rs rt immediate' => Branch (BNEL (rs, rt, immediate))
      case '011 000 rs rt immediate' => ArithI (DADDI (rs, rt, immediate))
      case '011 001 rs rt immediate' => ArithI (DADDIU (rs, rt, immediate))
      case '011 010 rs rt immediate' => Load (LDL (rs, rt, immediate))
      case '011 011 rs rt immediate' => Load (LDR (rs, rt, immediate))
      case '100 000 rs rt immediate' => Load (LB (rs, rt, immediate))
      case '100 001 rs rt immediate' => Load (LH (rs, rt, immediate))
      case '100 010 rs rt immediate' => Load (LWL (rs, rt, immediate))
      case '100 011 rs rt immediate' => Load (LW (rs, rt, immediate))
      case '100 100 rs rt immediate' => Load (LBU (rs, rt, immediate))
      case '100 101 rs rt immediate' => Load (LHU (rs, rt, immediate))
      case '100 110 rs rt immediate' => Load (LWR (rs, rt, immediate))
      case '100 111 rs rt immediate' => Load (LWU (rs, rt, immediate))
      case '101 000 rs rt immediate' => Store (SB (rs, rt, immediate))
      case '101 001 rs rt immediate' => Store (SH (rs, rt, immediate))
      case '101 010 rs rt immediate' => Store (SWL (rs, rt, immediate))
      case '101 011 rs rt immediate' => Store (SW (rs, rt, immediate))
      case '101 100 rs rt immediate' => Store (SDL (rs, rt, immediate))
      case '101 101 rs rt immediate' => Store (SDR (rs, rt, immediate))
      case '101 110 rs rt immediate' => Store (SWR (rs, rt, immediate))
      case '110 000 rs rt immediate' => Load (LL (rs, rt, immediate))
      case '110 100 rs rt immediate' => Load (LLD (rs, rt, immediate))
      case '110 111 rs rt immediate' => Load (LD (rs, rt, immediate))
      case '111 000 rs rt immediate' => Store (SC (rs, rt, immediate))
      case '111 100 rs rt immediate' => Store (SCD (rs, rt, immediate))
      case '111 111 rs rt immediate' => Store (SD (rs, rt, immediate))
      case '101 111 base opn immediate' => CACHE (base, opn, immediate)
      case '011 111 00000 rt rd 00000 111011' => RDHWR (rt, rd)
      case '010 000 1000 0000 0000 0000 0000 100000' => WAIT
      -- Coprocessor and reserved instructions
      case '010 001 v' => COP1Decode (v)
      case '010 010 v' => COP2Decode (v)
      case '010 011 v' => COP3Decode (v)
      case '110 001 rs rt immediate' => LWC1Decode (rt, immediate, rs)
      case '111 001 rs rt immediate' => SWC1Decode (rt, immediate, rs)
      case '110 101 rs rt immediate' => LDC1Decode (rt, immediate, rs)
      case '111 101 rs rt immediate' => SDC1Decode (rt, immediate, rs)
      case '110 010 r cb rt offset v' => LWC2Decode (r, cb, rt, offset, v)
      case '111 010 r cb rt offset v' => SWC2Decode (r, cb, rt, offset, v)
      case '110 110 c cb rt offset' => LDC2Decode (c, cb, rt, offset)
      case '111 110 c cb rt offset' => SDC2Decode (c, cb, rt, offset)
      case _ => ReservedInstruction
   }

instruction Decode (w::word) =
if PROVER_EXPORT then SimpleDecode (w) else
{
   c, r = QuotRem ([[w]::nat], 0x4000000);
   t, i = QuotRem (r, 0x10000);
   s, t = QuotRem (t, 0x20);
   match c
   {
      case 0b000_000 => Decode000_000 (s, t, i)
      case 0b000_001 => Decode000_001 (s, t, i)
      case 0b000_010 => Branch (J ([r]))
      case 0b000_011 => Branch (JAL ([r]))
      case 0b000_100 => Branch (BEQ ([s], [t], [i]))
      case 0b000_101 => Branch (BNE ([s], [t], [i]))
      case 0b000_110 =>
         if t == 0 then Branch (BLEZ ([s], [i])) else ReservedInstruction
      case 0b000_111 =>
         if t == 0 then Branch (BGTZ ([s], [i])) else ReservedInstruction
      case 0b001_000 => ArithI (ADDI ([s], [t], [i]))
      case 0b001_001 => ArithI (ADDIU ([s], [t], [i]))
      case 0b001_010 => ArithI (SLTI ([s], [t], [i]))
      case 0b001_011 => ArithI (SLTIU ([s], [t], [i]))
      case 0b001_100 => ArithI (ANDI ([s], [t], [i]))
      case 0b001_101 => ArithI (ORI ([s], [t], [i]))
      case 0b001_110 => ArithI (XORI ([s], [t], [i]))
      case 0b001_111 =>
         if s == 0 then ArithI (LUI ([t], [i])) else ReservedInstruction
      case 0b010_000 => Decode010_000 (r)
      case 0b010_001 => COP1Decode ([r])
      case 0b010_010 => COP2Decode ([r])
      case 0b010_011 => COP3Decode ([r])
      case 0b010_100 => Branch (BEQL ([s], [t], [i]))
      case 0b010_101 => Branch (BNEL ([s], [t], [i]))
      case 0b010_110 =>
         if t == 0 then Branch (BLEZL ([s], [i])) else ReservedInstruction
      case 0b010_111 =>
         if t == 0 then Branch (BGTZL ([s], [i])) else ReservedInstruction
      case 0b011_000 => ArithI (DADDI ([s], [t], [i]))
      case 0b011_001 => ArithI (DADDIU ([s], [t], [i]))
      case 0b011_010 => Load (LDL ([s], [t], [i]))
      case 0b011_011 => Load (LDR ([s], [t], [i]))
      case 0b011_100 => Decode011_100 (s, t, i)
      case 0b011_111 =>
      {
         d, i = QuotRem (i, 0x800);
         if s == 0 and i == 0b111011
            then RDHWR ([t], [d])
            else ReservedInstruction
      }
      case 0b100_000 => Load (LB ([s], [t], [i]))
      case 0b100_001 => Load (LH ([s], [t], [i]))
      case 0b100_010 => Load (LWL ([s], [t], [i]))
      case 0b100_011 => Load (LW ([s], [t], [i]))
      case 0b100_100 => Load (LBU ([s], [t], [i]))
      case 0b100_101 => Load (LHU ([s], [t], [i]))
      case 0b100_110 => Load (LWR ([s], [t], [i]))
      case 0b100_111 => Load (LWU ([s], [t], [i]))
      case 0b101_000 => Store (SB ([s], [t], [i]))
      case 0b101_001 => Store (SH ([s], [t], [i]))
      case 0b101_010 => Store (SWL ([s], [t], [i]))
      case 0b101_011 => Store (SW ([s], [t], [i]))
      case 0b101_100 => Store (SDL ([s], [t], [i]))
      case 0b101_101 => Store (SDR ([s], [t], [i]))
      case 0b101_110 => Store (SWR ([s], [t], [i]))
      case 0b101_111 => CACHE ([s], [t], [i])
      case 0b110_000 => Load (LL ([s], [t], [i]))
      case 0b110_100 => Load (LLD ([s], [t], [i]))
      case 0b110_111 => Load (LD ([s], [t], [i]))
      case 0b111_000 => Store (SC ([s], [t], [i]))
      case 0b111_100 => Store (SCD ([s], [t], [i]))
      case 0b111_111 => Store (SD ([s], [t], [i]))
      -- Coprocessor 2 and reserved instructions
      case 0b110_001 => LWC1Decode([t]`5, [i]`16, [s]`5)
      case 0b110_010 =>
         match [i] { case 'rt oset v' => LWC2Decode ([s], [t], rt, oset, v) }
      case 0b111_001 => SWC1Decode([t]`5, [i]`16, [s]`5)
      case 0b111_010 =>
         match [i] { case 'rt oset v' => SWC2Decode ([s], [t], rt, oset, v) }
      case 0b110_101 => LDC1Decode([t]`5, [i]`16, [s]`5)
      case 0b110_110 =>
         match [i] { case 'rt offset' => LDC2Decode ([s], [t], rt, offset) }
      case 0b111_101 => SDC1Decode([t]`5, [i]`16, [s]`5)
      case 0b111_110 =>
         match [i] { case 'rt offset' => SDC2Decode ([s], [t], rt, offset) }
      case _ => ReservedInstruction
   }
}
