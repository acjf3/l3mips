---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

type mAddr = bits(37)

nat PSIZE = 40         -- 40-bit physical memory

declare done :: bool   -- Flag to request termination

--------------------------------------------------
-- Stats dump and reset
--------------------------------------------------

unit resetStats =
{
    initCoreStats;
    initMemStats
}

string dumpStats =
{
    var out = "";
    for i in 0 .. totalCore-1 do out <- out : "-- Core " : [i::nat] : " stats --\\n" : printCoreStats : "\\n";
    out <- out : " -- Memory stats --\\n" : printMemStats : "\\n";
    out
}

--------------------------------------------------
-- HI/LO registers
--------------------------------------------------

declare
{
   UNPREDICTABLE_LO :: unit -> unit
   UNPREDICTABLE_HI :: unit -> unit
}

component HI :: dword
{
   value = match hi { case Some (v) => v
                      case None => { UNPREDICTABLE_HI (); UNKNOWN }
                    }
   assign value = { hi <- Some (value); mark_log (2, log_w_hi (value)) }
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => { UNPREDICTABLE_LO (); UNKNOWN }
                    }
   assign value = { lo <- Some (value); mark_log (2, log_w_lo (value)) }
}

--------------------------------------------------
-- Coprocessor register access
--------------------------------------------------

component CPR (n::nat, reg::bits(5), sel::bits(3)) :: dword
{
   value =
      match n, reg, sel
      {
        --------------------------------------------------
        -- CP0 register access
        --------------------------------------------------
         case 0,  0, 0 => [CP0.&Index]
         case 0,  1, 0 => [CP0.&Random]
         case 0,  2, 0 =>  CP0.&EntryLo0
         case 0,  3, 0 =>  CP0.&EntryLo1
         case 0,  4, 0 =>  CP0.&Context
         case 0,  4, 2 =>  CP0.UsrLocal
         case 0,  5, 0 => [CP0.&PageMask]
         case 0,  6, 0 => [CP0.&Wired]
         case 0,  7, 0 => [CP0.&HWREna]
         case 0,  8, 0 =>  CP0.BadVAddr
         case 0,  8, 1 => [CP0.EInstr]
         case 0,  9, 0 => [CP0.Count]
         case 0, 10, 0 =>  CP0.&EntryHi
         case 0, 11, 0 => [CP0.Compare]
         case 0, 12, 0 => [CP0.&Status]
         case 0, 13, 0 => [CP0.&Cause]
         case 0, 14, 0 =>  CP0.EPC
         case 0, 15, 0 => [CP0.PRId]
         case 0, 15, 1 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                    : ([procID] :: bits(16)) )
         case 0, 15, 6 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                    : ([procID] :: bits(16)) )
         case 0, 16, 0 => [CP0.&Config]
         case 0, 16, 1 => [CP0.&Config1]
         case 0, 16, 2 => [CP0.&Config2]
         case 0, 16, 3 => [CP0.&Config3]
         case 0, 16, 4 => 1 -- Mimic BERI
         case 0, 16, 5 => 1 -- Mimic BERI
         case 0, 16, 6 => [CP0.&Config6]
         case 0, 17, 0 => [CP0.LLAddr]
         case 0, 20, 0 =>  CP0.&XContext
         case 0, 23, 0 => [CP0.Debug]
         case 0, 26, 0 => [CP0.ErrCtl]
         case 0, 30, 0 =>  CP0.ErrorEPC
         case _ => UNKNOWN
      }
   assign value =
   {
      mark_log (2, log_w_c0 (reg, value));
      match n, reg, sel
      {
         case 0,  0, 0 => CP0.Index.Index <- value<7:0>
         case 0,  2, 0 => CP0.&EntryLo0 <- value
         case 0,  3, 0 => CP0.&EntryLo1 <- value
         case 0,  4, 0 => CP0.Context.PTEBase <- value<63:23>
         case 0,  4, 2 => CP0.UsrLocal <- value
         case 0,  5, 0 => CP0.PageMask.Mask <- value<24:13>
         case 0,  6, 0 => {
                            CP0.Wired.Wired <- value<7:0>;
                            CP0.Random.Random <- [TLBEntries-1]
                          }
         case 0,  7, 0 => {
                            CP0.HWREna.CPUNum <- value<0>;
                            CP0.HWREna.CC     <- value<2>;
                            CP0.HWREna.CCRes  <- value<3>;
                            CP0.HWREna.UL     <- value<29>
                          }
         case 0,  9, 0 => CP0.Count <- value<31:0>
         case 0, 10, 0 => CP0.&EntryHi <- value
         case 0, 11, 0 => {
                            CP0.Compare <- value<31:0>;
                            CP0.Cause.IP<7> <- false;
                            CP0.Cause.TI <- false
                          }
         case 0, 12, 0 => CP0.&Status <- value<31:0>
         case 0, 13, 0 => CP0.Cause.IP<1:0> <- value<9:8>
         case 0, 14, 0 => CP0.EPC <- value
         case 0, 16, 0 => CP0.Config.K0 <- value<2:0>
         case 0, 16, 2 => CP0.Config2.SU <- value<15:12>
         case 0, 16, 6 => CP0.Config6.LTLB <- value<2>
         case 0, 20, 0 => CP0.XContext.PTEBase <- value<63:33>
         case 0, 23, 0 => {CP0.Debug <- value<31:0>; done <- true}
         case 0, 26, 0 => {CP0.ErrCtl <- value<31:0>; dumpRegs()}
         case 0, 30, 0 => CP0.ErrorEPC <- value
         case _ => unmark_log(2)
      }
   }
}
