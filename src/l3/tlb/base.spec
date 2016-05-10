---------------------------------------------------------------------------
-- Model of the MIPS TLB
-- (c) Anthony Fox, University of Cambridge
-- (c) Aleaxndre Joannou, University of Cambridge
---------------------------------------------------------------------------

nat TLBAssocEntries = 16
nat TLBDirectEntries = 256
nat TLBEntries = TLBAssocEntries + TLBDirectEntries

-- Each core has its own TLB, with both associative and direct-mapped
-- regions.  (See BERI manual.)

type TLBAssocMap = bits(4) -> TLBEntry option
type TLBDirectMap = bits(8) -> TLBEntry option

declare
{
   c_TLB_direct :: id -> TLBDirectMap
   c_TLB_assoc  :: id -> TLBAssocMap
   UNPREDICTABLE_TLB :: unit -> unit
}

-- The following two components give read/write access to the TLB of
-- the currently-running core.

component TLB_direct (i::bits(8)) :: TLBEntry option
{
   value = { var m = c_TLB_direct(procID); m(i) }
   assign value = { var m = c_TLB_direct(procID)
                  ; m(i) <- value
                  ; c_TLB_direct(procID) <- m }
}

component TLB_assoc (i::bits(4)) :: TLBEntry option
{
   value = { var m = c_TLB_assoc(procID); m(i) }
   assign value = { var m = c_TLB_assoc(procID)
                  ; m(i) <- value
                  ; c_TLB_assoc(procID) <- m}
}

(bits(9) * TLBEntry) list LookupTLB (r::bits(2), vpn2::bits(27)) =
{
    var found = Nil;
    match TLB_direct (vpn2<7:0>)
    {
        case Some (e) =>
        {
            index`9 = if [vpn2<7:0>] >= TLBAssocEntries then
                      [vpn2<7:0>] else [TLBDirectEntries] + [vpn2<7:0>];
            nmask`27 = ~[e.Mask];
            when CP0.Config6.LTLB do
                found <- if e.VPN2 && nmask == vpn2 && nmask and e.R == r
                         and (e.G or e.ASID == CP0.EntryHi.ASID) then
                            list {(index, e)}
                         else Nil
        }
        case _ => found <- Nil
    };
    for i in 0 .. TLBAssocEntries - 1 do
    {
        match TLB_assoc ([i])
        {
            case Some (e) =>
            {
                nmask`27 = ~[e.Mask];
                when e.VPN2 && nmask == vpn2 && nmask and e.R == r
                     and (e.G or e.ASID == CP0.EntryHi.ASID) do
                     found <- ([i], e) @ found
            }
            case _ => nothing
        }
   };
   return found
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
      0x4000_0000_0000_0000 <=+ vAddr and
      vAddr <+  0x4000_0100_0000_0000 or        -- xsseg
      0xFFFF_FFFF_C000_0000 <=+ vAddr and
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

nat option checkMask (mask::bits(12)) = match mask
{
    case 0b0000_0000_0000 => Some(12)
    case 0b0000_0000_0011 => Some(14)
    case 0b0000_0000_1111 => Some(16)
    case 0b0000_0011_1111 => Some(18)
    case 0b0000_1111_1111 => Some(20)
    case 0b0011_1111_1111 => Some(22)
    case 0b1111_1111_1111 => Some(24)
    case _                => None
}
