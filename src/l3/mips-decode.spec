---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- Instruction decoding
--================================================

instruction Decode000_000 (s::int, t::int, i::int) =
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

instruction Decode000_001 (s::int, r::int, i::int) =
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

instruction Decode010_000 (r::int) =
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

instruction Decode011_100 (s::int, t::int, i::int) =
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

instruction Decode (w::word) =
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
