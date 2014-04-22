---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Gereral purpose register access
--------------------------------------------------

component GPR (n::reg) :: dword
{
   value = if n == 0 then 0 else gpr(n)
   assign value = when n <> 0 do gpr(n) <- value
}

component HI :: dword
{
   value = match hi { case Some (v) => v
                      case None => #UNPREDICTABLE ("HI")
                    }
   assign value = hi <- Some (value)
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => #UNPREDICTABLE ("LO")
                    }
   assign value = lo <- Some (value)
}

--------------------------------------------------
-- CP0 register access
--------------------------------------------------

component CPR (n::nat, reg::bits(5), sel::bits(3)) :: dword
{
   value =
      match n, reg, sel
      {
         case 0,  8, 0 =>  CP0.BadVAddr
         case 0,  9, 0 => [CP0.Count]
         case 0, 11, 0 => [CP0.Compare]
         case 0, 12, 0 => [CP0.&Status]
         case 0, 13, 0 => [CP0.&Cause]
         case 0, 14, 0 =>  CP0.EPC
         case 0, 15, 0 => [CP0.PRId]
         case 0, 16, 0 => [CP0.&Config]
         case 0, 17, 0 => [CP0.LLAddr]
         case 0, 23, 0 => [CP0.Debug]
         case 0, 26, 0 => [CP0.ErrCtl]
         case 0, 30, 0 =>  CP0.ErrorEPC
         case _ => UNKNOWN
      }
   assign value =
      match n, reg, sel
      {
         -- case 0,  8, 0 => CP0.BadVAddr <- value
         case 0,  9, 0 => CP0.Count    <- value<31:0>
         case 0, 11, 0 => CP0.Compare  <- value<31:0>
         case 0, 12, 0 => CP0.&Status  <- value<31:0>
         case 0, 13, 0 => CP0.&Cause   <- value<31:0>
         case 0, 14, 0 => CP0.EPC      <- value
         case 0, 16, 0 => CP0.&Config  <- value<31:0>
         -- case 0, 17, 0 => CP0.LLAddr   <- value<31:0>
         case 0, 23, 0 => CP0.Debug    <- value<31:0>
         case 0, 26, 0 => CP0.ErrCtl   <- value<31:0>
         case 0, 30, 0 => CP0.ErrorEPC <- value
         case _ => nothing
      }
}

--------------------------------------------------
-- Memory access
--------------------------------------------------

type pAddr = bits(64)

nat PSIZE = 64 -- 64-bit physical memory

declare MEM :: pAddr -> byte -- memory

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, LorS::LorS) =
   return (vAddr, 2) -- null address translation

dword LoadMemory (CCA::CCA, AccessLength::bits(3),
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
{
   a = pAddr && ~0b111; -- align to 64-bit word
   if BigEndianMem then
      MEM(a) :
      MEM(a + 1) :
      MEM(a + 2) :
      MEM(a + 3) :
      MEM(a + 4) :
      MEM(a + 5) :
      MEM(a + 6) :
      MEM(a + 7)
   else
      MEM(a + 7) :
      MEM(a + 6) :
      MEM(a + 5) :
      MEM(a + 4) :
      MEM(a + 3) :
      MEM(a + 2) :
      MEM(a + 1) :
      MEM(a)
}

unit StoreMemory (CCA::CCA, AccessLength::bits(3), MemElem::dword,
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
{  a = pAddr && ~0b111; -- align to 64-bit word
   l = pAddr<2:0>;
   h = l + AccessLength;
   if BigEndianMem then
   {
      when l == 0              do MEM(a)     <- MemElem<63:56>;
      when l <=+ 1 and 1 <=+ h do MEM(a + 1) <- MemElem<55:48>;
      when l <=+ 2 and 2 <=+ h do MEM(a + 2) <- MemElem<47:40>;
      when l <=+ 3 and 3 <=+ h do MEM(a + 3) <- MemElem<39:32>;
      when l <=+ 4 and 4 <=+ h do MEM(a + 4) <- MemElem<31:24>;
      when l <=+ 5 and 5 <=+ h do MEM(a + 5) <- MemElem<23:16>;
      when l <=+ 6 and 6 <=+ h do MEM(a + 6) <- MemElem<15:8>;
      when l <=+ 7 and 7 <=+ h do MEM(a + 7) <- MemElem<7:0>
   }
   else
   {
      when l <=+ 7 and 7 <=+ h do MEM(a + 7) <- MemElem<63:56>;
      when l <=+ 6 and 6 <=+ h do MEM(a + 6) <- MemElem<55:48>;
      when l <=+ 5 and 5 <=+ h do MEM(a + 5) <- MemElem<47:40>;
      when l <=+ 4 and 4 <=+ h do MEM(a + 4) <- MemElem<39:32>;
      when l <=+ 3 and 3 <=+ h do MEM(a + 3) <- MemElem<31:24>;
      when l <=+ 2 and 2 <=+ h do MEM(a + 2) <- MemElem<23:16>;
      when l <=+ 1 and 1 <=+ h do MEM(a + 1) <- MemElem<15:8>;
      when l == 0              do MEM(a)     <- MemElem<7:0>
   }
}

word option Fetch =
   if PC<1:0> == 0 then
   {
      vAddr = PC;
      pAddr, CCA = AddressTranslation (vAddr, INSTRUCTION, LOAD);
      memdoubleword = LoadMemory (CCA, WORD, pAddr, vAddr, INSTRUCTION);
      bytesel = vAddr<2:0> ?? (BigEndianCPU : '00'); -- must be aligned
      memword = memdoubleword <31 + 8 * [bytesel] : 8 * [bytesel]>;
      return (Some (memword))
   }
   else
   {
      SignalException (AdEL);
      None
   }

--------------------------------------------------
-- Stub TLB instructions
--------------------------------------------------

--------
-- TLBP
--------
define TLBP = SignalException (RI)

--------
-- TLBR
--------
define TLBR = SignalException (RI)

--------
-- TLBWI
--------
define TLBWI = SignalException (RI)

--------
-- TLBWR
--------
define TLBWR = SignalException (RI)

-------------------------
-- CACHE op, offset(base)
-------------------------
define CACHE (base::reg, opn::bits(5), offset::bits(16)) = SignalException (RI)
