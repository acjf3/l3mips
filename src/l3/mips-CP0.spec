---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Coprocessor register access
--------------------------------------------------

component CP0R (reg::bits(5), sel::bits(3)) :: dword
{
   value =
      match reg, sel
      {
        --------------------------------------------------
        -- CP0 register access
        --------------------------------------------------
         case  0, 0 => [CP0.&Index]
         case  1, 0 => [CP0.&Random]
         case  2, 0 =>  CP0.&EntryLo0
         case  3, 0 =>  CP0.&EntryLo1
         case  4, 0 =>  CP0.&Context
         case  4, 2 =>  CP0.UsrLocal
         case  5, 0 => [CP0.&PageMask]
         case  6, 0 => [CP0.&Wired]
         case  7, 0 => [CP0.&HWREna]
         case  8, 0 =>  CP0.BadVAddr
         case  8, 1 => [CP0.BadInstr]
         case  8, 2 => [CP0.BadInstrP]
         case  9, 0 => [CP0.Count]
         case 10, 0 =>  CP0.&EntryHi
         case 11, 0 => [CP0.Compare]
         case 12, 0 => [CP0.&Status]
         case 13, 0 => [CP0.&Cause]
         case 14, 0 =>  CP0.EPC
         case 15, 0 => [CP0.PRId]
         case 15, 1 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                 : ([procID] :: bits(16)) )
         case 15, 6 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                 : ([procID] :: bits(16)) )
         case 16, 0 => [CP0.&Config]
         case 16, 1 => [CP0.&Config1]
         case 16, 2 => [CP0.&Config2]
         case 16, 3 => [CP0.&Config3]
         case 16, 4 => 1 -- Mimic BERI
         case 16, 5 => 1 -- Mimic BERI
         case 16, 6 => [CP0.&Config6]
         case 17, 0 =>  CP0.LLAddr
         case 20, 0 =>  CP0.&XContext
         case 23, 0 => [CP0.Debug]
         case 26, 0 => [CP0.ErrCtl]
         case 30, 0 =>  CP0.ErrorEPC
         case _ => UNKNOWN(next_unknown("cop-reg"))
      }
   assign value =
   {
      mark_log (2, log_w_c0 (reg, value));
      match reg, sel
      {
         case  0, 0 => CP0.Index.Index <- value<8:0>
         case  2, 0 => CP0.&EntryLo0 <- value
         case  3, 0 => CP0.&EntryLo1 <- value
         case  4, 0 => CP0.Context.PTEBase <- value<63:23>
         case  4, 2 => CP0.UsrLocal <- value
         case  5, 0 => CP0.PageMask.Mask <- value<24:13>
         case  6, 0 => {
                         CP0.Wired.Wired <- value<7:0>;
                         CP0.Random.Random <- [TLBAssocEntries-1]
                       }
         case  7, 0 => {
                         CP0.HWREna.CPUNum <- value<0>;
                         CP0.HWREna.CC     <- value<2>;
                         CP0.HWREna.CCRes  <- value<3>;
                         CP0.HWREna.RS     <- value<27>;
                         CP0.HWREna.DS     <- value<28>;
                         CP0.HWREna.UL     <- value<29>
                       }
         case  9, 0 => CP0.Count <- value<31:0>
         case 10, 0 => CP0.&EntryHi <- value
         case 11, 0 => {
                         CP0.Compare <- value<31:0>;
                         CP0.Cause.IP<7> <- false;
                         CP0.Cause.TI <- false
                       }
         case 12, 0 => {
                         CP0.&Status <- value<31:0>;
                         CP0.Status.CU2 <- CP0.Status.CU2 and hasCP2;
                         CP0.Status.CU1 <- CP0.Status.CU1 and hasCP1;
                         CP0.Status.RE <- false -- force RE to False
                       }
         case 13, 0 => CP0.Cause.IP<1:0> <- value<9:8>
         case 14, 0 => CP0.EPC <- value
         case 16, 0 => CP0.Config.K0 <- value<2:0>
         case 16, 2 => CP0.Config2.SU <- value<15:12>
         case 16, 6 => CP0.Config6.LTLB <- value<2>
         case 20, 0 => CP0.XContext.PTEBase <- value<63:33>
         case 23, 0 => {CP0.Debug <- value<31:0>; done <- true}
         case 26, 0 => {CP0.ErrCtl <- value<31:0>; dumpRegs()}
         case 30, 0 => CP0.ErrorEPC <- value
         case _ => unmark_log (2)
      }
   }
}
