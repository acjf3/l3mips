---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Core switching
--------------------------------------------------

-- Switch cores by saving the current core state in the global map and then
-- update to the current core state to be that of core "i".

nat switchCore (n::nat) =
{
   old = [procID];
   when n <> old do
   {
      i = [n];
      switchCoreTLB (i);
      switchCoreCAP (i);
      all_gpr (procID) <- c_gpr;
      all_state (procID) <- c_state;
      c_gpr <- all_gpr (i);
      c_state <- all_state (i);
      procID <- i
   };
   return old
}

--------------------------------------------------
-- Stats dump and reset
--------------------------------------------------

unit resetStats =
{
    initCoreStats;
    initMemAccessStats;
    initMemStats
}

declare csv_stats_header_done :: bool

string dumpStats (inst::nat, ips::string, fmt :: string option) =
if PROVER_EXPORT then "" else
match fmt
{
    case Some ("csv") =>
    {
        var out = "";
        when not csv_stats_header_done do
        {
            out <- "inst,ips,";
            out <- out : csvHeaderCoreStats : ",";
            out <- out : csvHeaderMemAccessStats : ",";
            out <- out : csvHeaderMemStats : "\n";
            csv_stats_header_done <- true
        };
        out <- out:[inst]:",":ips:",";
        out <- out : csvCoreStats : ",";
        out <- out : csvMemAccessStats : ",";
        out <- out : csvMemStats : "\n";
        out
    }
    case _ =>
    {
        var out = "";
        out <- "===========================================================\n";
        out <- "instruction #" : [inst] : " (":ips:" inst/sec)\n";
        for i in 0 .. totalCore-1 do
            out <- out : "-- Core " : [i] : " stats --\n" : printCoreStats : "\n";
        out <- out : " -- Memory accesses stats --\n" : printMemAccessStats : "\n";
        out <- out : " -- Memory stats --\n" : printMemStats : "\n";
        out
    }
}

unit clearDynamicStats () = clearDynamicMemStats ()

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
                      case None => { UNPREDICTABLE_HI (); UNKNOWN(next_unknown("hi-reg")) }
                    }
   assign value = { hi <- Some (value);
                    mark_log (2, log_w_hi (value))
                  }
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => { UNPREDICTABLE_LO (); UNKNOWN(next_unknown("lo-reg")) }
                    }
   assign value = { lo <- Some (value);
                    mark_log (2, log_w_lo (value))
                  }
}
