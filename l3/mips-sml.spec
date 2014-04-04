---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- Code used for SML emulation of MIPS

construct event
{
   w_gpr :: reg * dword
   w_hi :: dword
   w_lo :: dword
   w_c0 :: reg * dword
   w_mem :: pAddr * pAddr * bits(3) * dword
}

declare log :: event list

unit mark (e::event) = log <- e @ log

--------------------------------------------------
-- Gereral purpose register access
--------------------------------------------------

component GPR (n::reg) :: dword
{
   value = if n == 0 then 0 else gpr(n)
   assign value = when n <> 0 do { gpr(n) <- value; mark (w_gpr (n, value)) }
}

component HI :: dword
{
   value = match hi { case Some (v) => v
                      case None => #UNPREDICTABLE ("HI")
                    }
   assign value = { hi <- Some (value); mark (w_hi (value)) }
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => #UNPREDICTABLE ("LO")
                    }
   assign value = { lo <- Some (value); mark (w_lo (value)) }
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
         case 0,  9, 0 =>
            { CP0.Count <- value<31:0>   ; mark (w_c0 (reg, value)) }
         case 0, 11, 0 =>
            { CP0.Compare <- value<31:0> ; mark (w_c0 (reg, value)) }
         case 0, 12, 0 =>
            { CP0.&Status <- value<31:0> ; mark (w_c0 (reg, value)) }
         case 0, 13, 0 =>
            { CP0.&Cause <- value<31:0>  ; mark (w_c0 (reg, value)) }
         case 0, 14, 0 =>
            { CP0.EPC <- value           ; mark (w_c0 (reg, value)) }
         case 0, 16, 0 =>
            { CP0.&Config <- value<31:0> ; mark (w_c0 (reg, value)) }
         case 0, 23, 0 =>
            { CP0.Debug <- value<31:0>   ; mark (w_c0 (reg, value)) }
         case 0, 26, 0 =>
            { CP0.ErrCtl <- value<31:0>  ; mark (w_c0 (reg, value)) }
         case 0, 30, 0 =>
            { CP0.ErrorEPC <- value      ; mark (w_c0 (reg, value)) }
         case _ => nothing
      }
}

--------------------------------------------------
-- Memory access
--------------------------------------------------

type mAddr = bits(61)

declare MEM :: mAddr -> dword -- memory

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, LorS::LorS) =
   return (vAddr && 0x00FFFFFFFFFFFFFF, UNKNOWN) -- mask top byte

dword LoadMemory (CCA::CCA, AccessLength::bits(3),
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
   MEM (pAddr<63:3>)

-- Big-endian memory
unit StoreMemory (CCA::CCA, AccessLength::bits(3), MemElem::dword,
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
{  a = pAddr<63:3>;
   l = 64 - ([AccessLength] + 1 + [pAddr<2:0>]) * 0n8;
   mask`64 = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];
   MEM(a) <- MEM(a) && ~mask || MemElem && mask;
   mark (w_mem (pAddr, mask, AccessLength, MemElem))
}

word option Fetch =
{
   log <- Nil;
   if PC<1:0> == 0 then
   {
      d = MEM([PC<55:3>]);
      Some (if PC<2> then d<31:0> else d<63:32>)
   }
   else
      None
}

--------------------------------------------------
-- Initialisation and termination
--------------------------------------------------

unit initMips (pc::nat) =
{
   CP0.Count <- 0;
   CP0.Config.BE  <- true;      -- big-endian
   CP0.&Status <- 0x044000e0;
   CP0.PRId <- 0x400;           -- processor ID
   BranchDelay <- None;
   BranchTo <- None;
   LLbit <- None;
   hi <- None;
   lo <- None;
   PC <- [pc];
   MEM <- InitMap (0x0);
   gpr <- InitMap (0xAAAAAAAAAAAAAAAA)
}

bool done =
   return
     (match log
      {
         case list {w_c0 (23, _)} => true
         case _ => false
      } or
      match BranchDelay
      {
         case Some (addr) => addr == PC - 8
         case None => false
      })
