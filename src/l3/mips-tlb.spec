---------------------------------------------------------------------------
-- Model of the MIPS TLB
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

nat TLBEntries = 16

-- Each core has its own TLB, with both associative and direct-mapped
-- regions.  (See BERI manual.)

type TLBAssocMap = bits(4) -> TLBEntry
type TLBDirectMap = bits(7) -> TLBEntry

declare
{
   c_TLB_direct :: id -> TLBDirectMap
   c_TLB_assoc  :: id -> TLBAssocMap
}

-- The following two components give read/write access to the TLB of
-- the currently-running core.

component TLB_direct (i::bits(7)) :: TLBEntry
{
   value = { var m = c_TLB_direct(procID); m(i) }
   assign value = { var m = c_TLB_direct(procID)
                  ; m(i) <- value
                  ; c_TLB_direct(procID) <- m }
}

component TLB_assoc (i::bits(4)) :: TLBEntry
{
   value = { var m = c_TLB_assoc(procID); m(i) }
   assign value = { var m = c_TLB_assoc(procID)
                  ; m(i) <- value
                  ; c_TLB_assoc(procID) <- m}
}

(bits(8) * TLBEntry) list LookupTLB (r::bits(2), vpn2::bits(27)) =
{
   e = TLB_direct (vpn2<6:0>);
   index`8 = if [vpn2<6:0>] >= TLBEntries
             then [vpn2<6:0>] else 128 + [vpn2<6:0>];
   nmask`27 = ~[e.Mask];
   var found = Nil;
   when CP0.Config6.LTLB do
     found <- if e.VPN2 && nmask == vpn2 && nmask and e.R == r
                    and (e.G or e.ASID == CP0.EntryHi.ASID) then
                     list {(index, e)}
              else Nil;
   for i in 0 .. TLBEntries - 1 do
   {
      e = TLB_assoc ([i]);
      nmask`27 = ~[e.Mask];
      when e.VPN2 && nmask == vpn2 && nmask and e.R == r
           and (e.G or e.ASID == CP0.EntryHi.ASID) do
         found <- ([i], e) @ found
   };
   return found
}

TLBEntry ModifyTLB (ie::TLBEntry) =
{
   eHi = CP0.EntryHi;
   eLo1 = CP0.EntryLo1;
   eLo0 = CP0.EntryLo0;
   var e = ie;
   e.Mask <- CP0.PageMask.Mask;
   e.R <- eHi.R;
   e.VPN2 <- eHi.VPN2;
   e.ASID <- eHi.ASID;
   e.PFN1 <- eLo1.PFN;
   e.C1 <- eLo1.C;
   e.D1 <- eLo1.D;
   e.V1 <- eLo1.V;
   e.G <- eLo1.G and eLo0.G;
   e.PFN0 <- eLo0.PFN;
   e.C0 <- eLo0.C;
   e.D0 <- eLo0.D;
   e.V0 <- eLo0.V;
   return e
}

pAddr * CCA SignalTLBException (e::ExceptionType, asid::bits(8), vAddr::vAddr) =
{
   r = vAddr<63:62>;
   vpn2 = vAddr<39:13>;
   SignalException (e);
   CP0.BadVAddr <- vAddr;
   CP0.EntryHi.R <- r;
   CP0.EntryHi.VPN2 <- vpn2;
   CP0.EntryHi.ASID <- asid;
   CP0.XContext.R <- r;
   CP0.XContext.BadVPN2 <- vpn2;
   CP0.Context.BadVPN2 <- vAddr<31:13>;
   UNKNOWN
}

(pAddr * CCA) option * bool CheckSegment (vAddr::vAddr) =
   if UserMode then
      None, vAddr <+ 0x0000_0100_0000_0000      -- xuseg
   else if SupervisorMode then
      None,
      vAddr <+ 0x0000_0100_0000_0000 or         -- xsuseg
      vAddr <=+ 0x4000_0000_0000_0000 and
      vAddr <+  0x4000_0100_0000_0000 or        -- xsseg
      vAddr <=+ 0xFFFF_FFFF_C000_0000 and
      vAddr <+  0xFFFF_FFFF_E000_0000           -- csseg
   else if vAddr <+ 0x0000_0100_0000_0000 then  -- xkuseg
      None, true
   else if 0x4000_0000_0000_0000 <=+ vAddr and
           vAddr <+  0x4000_0100_0000_0000 then -- xksseg
      None, true
   else if 0x8000_0000_0000_0000 <=+ vAddr and
           vAddr <+  0xC000_0000_0000_0000 then -- xkphys (unmapped)
      Some (vAddr<39:0>, vAddr<61:59>), vAddr<58:40> == 0
   else if 0xC000_0000_0000_0000 <=+ vAddr and
           vAddr <+  0xC000_00FF_8000_0000 then -- xkseg
      None, true
   else if 0xFFFF_FFFF_8000_0000 <=+ vAddr and
           vAddr <+  0xFFFF_FFFF_A000_0000 then -- ckseg0 (unmapped)
      Some (vAddr<39:0> - 0xFF_8000_0000, CP0.Config.K0), true
   else if 0xFFFF_FFFF_A000_0000 <=+ vAddr and
           vAddr <+  0xFFFF_FFFF_C000_0000 then -- ckseg1 (unmapped+uncached)
      Some (vAddr<39:0> - 0xFF_A000_0000, 2), true
   else
      None, 0xFFFF_FFFF_C000_0000 <=+ vAddr     -- cksseg/ckseg3

--------------------------------------------------
-- TLB instructions
--------------------------------------------------

define TLBP =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
     match LookupTLB (CP0.EntryHi.R, CP0.EntryHi.VPN2)
     {
        case Nil =>
           {
              CP0.Index.P <- true;
              CP0.Index.Index <- UNKNOWN
           }
        case list {(i, e)} =>
           {
              CP0.Index.P <- false;
              CP0.Index.Index <- i
           }
        case _ => #UNPREDICTABLE ("TLB: multiple matches")
     }

define TLBR =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
   {
     i = CP0.Index.Index;
     e = if [i] >= TLBEntries then
           TLB_direct (i<6:0>)
         else
           TLB_assoc ([i]);
     CP0.PageMask.Mask <- e.Mask;
     CP0.EntryHi.R <- e.R;
     CP0.EntryHi.VPN2 <- e.VPN2;
     CP0.EntryHi.ASID <- e.ASID;
     CP0.EntryLo1.PFN <- e.PFN1;
     CP0.EntryLo1.C <- e.C1;
     CP0.EntryLo1.D <- e.D1;
     CP0.EntryLo1.V <- e.V1;
     CP0.EntryLo1.G <- e.G;
     CP0.EntryLo0.PFN <- e.PFN0;
     CP0.EntryLo0.C <- e.C0;
     CP0.EntryLo0.D <- e.D0;
     CP0.EntryLo0.V <- e.V0;
     CP0.EntryLo0.G <- e.G
   }

define TLBWI =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
   {
     if [CP0.Index.Index] >= TLBEntries then
     {
        j = CP0.EntryHi.VPN2<6:0>;
        TLB_direct (j) <- ModifyTLB (TLB_direct (j))
     }
     else
     {
        i`4 = [CP0.Index.Index];
        TLB_assoc (i) <- ModifyTLB (TLB_assoc (i))
     }
   }

define TLBWR =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
   {
     if CP0.Config6.LTLB then
     {
       j = CP0.EntryHi.VPN2<6:0>;
       old = TLB_direct (j);
       TLB_direct (j) <- ModifyTLB (old);
       when old.V0 and old.V1 do TLB_assoc ([CP0.Random.Random]) <- old
     }
     else
     {
       j = CP0.Random.Random;
       TLB_assoc ([j]) <- ModifyTLB (TLB_assoc ([j]))
     }
   }
