---------------------------------------------------------------------------
-- Instructions of the MIPS TLB
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- TLB instructions
--------------------------------------------------

-----------------------------------
-- TLBP
-----------------------------------
define TLBP =
  if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
  else match LookupTLB (CP0.EntryHi.R, CP0.EntryHi.VPN2)
  {
      case Nil =>
      {
          CP0.Index.P <- true;
          CP0.Index.Index <- UNKNOWN(next_unknown)
      }
      case list {(i, _)} =>
      {
          CP0.Index.P <- false;
          CP0.Index.Index <- i
      }
      case _ => #UNPREDICTABLE ("TLB: multiple matches")
  }

-----------------------------------
-- TLBR
-----------------------------------
define TLBR =
  if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
  else
  {
      i = CP0.Index.Index;
      match   if [i] >= TLBAssocEntries then
                  TLB_direct (i<7:0>)
              else
                  TLB_assoc ([i])
      {
          case Some (e) =>
          {
              CP0.PageMask.Mask <- e.Mask;
              CP0.EntryHi.R     <- e.R;
              CP0.EntryHi.VPN2  <- e.VPN2;
              CP0.EntryHi.ASID  <- e.ASID;
              CP0.EntryLo1.PFN  <- e.PFN1;
              CP0.EntryLo1.C    <- e.C1;
              CP0.EntryLo1.D    <- e.D1;
              CP0.EntryLo1.V    <- e.V1;
              CP0.EntryLo1.G    <- e.G;
              CP0.EntryLo0.PFN  <- e.PFN0;
              CP0.EntryLo0.C    <- e.C0;
              CP0.EntryLo0.D    <- e.D0;
              CP0.EntryLo0.V    <- e.V0;
              CP0.EntryLo0.G    <- e.G
          }
          case _ => #UNPREDICTABLE ("TLB: reading from invalid entry")
      }
  }

-----------------------------------
-- TLBWI
-----------------------------------
define TLBWI =
  if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
  else if not IsSome(checkMask(CP0.PageMask.Mask)) then
  {
      UNPREDICTABLE_TLB ();
      SignalException (MCheck)
  }
  else if [CP0.Index.Index] < TLBAssocEntries then
  {
      i`4 = [CP0.Index.Index];
      TLB_assoc (i) <- Some (CP0TLBEntry ())
  }
  else if [CP0.Index.Index] < TLBEntries then
  {
      j = CP0.EntryHi.VPN2<7:0>;
      TLB_direct (j) <- Some (CP0TLBEntry ())
  }
  else
  {
      UNPREDICTABLE_TLB ();
      SignalException (MCheck)
  }

-----------------------------------
-- TLBWR
-----------------------------------
define TLBWR =
  if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
  else if not IsSome(checkMask(CP0.PageMask.Mask)) then
  {
      UNPREDICTABLE_TLB ();
      SignalException (MCheck)
  }
  else if CP0.Config6.LTLB then
  {
      j = CP0.EntryHi.VPN2<7:0>;
      match TLB_direct (j)
      {
          case Some (old) => TLB_assoc ([CP0.Random.Random]) <- Some (old)
          case _ => nothing
      };
      TLB_direct (j) <- Some (CP0TLBEntry ())
  }
  else
  {
      j = CP0.Random.Random;
      TLB_assoc ([j]) <- Some (CP0TLBEntry ())
  }
