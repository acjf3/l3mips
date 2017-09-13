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
   all_TLB_direct :: id -> TLBDirectMap
   all_TLB_assoc  :: id -> TLBAssocMap
   c_TLB_direct :: TLBDirectMap
   c_TLB_assoc  :: TLBAssocMap
   UNPREDICTABLE_TLB :: unit -> unit
}

-- The following two components give read/write access to the TLB of
-- the currently-running core.

component TLB_direct (i::bits(8)) :: TLBEntry option
{
   value = c_TLB_direct(i)
   assign value = c_TLB_direct(i) <- value
}

component TLB_assoc (i::bits(4)) :: TLBEntry option
{
   value = c_TLB_assoc(i)
   assign value = c_TLB_assoc(i) <- value
}

unit switchCoreTLB (i::id) =
{
   all_TLB_direct (procID) <- c_TLB_direct;
   all_TLB_assoc (procID) <- c_TLB_assoc;
   c_TLB_direct <- all_TLB_direct (i);
   c_TLB_assoc <- all_TLB_assoc (i)
}

inline bool MatchingEntry (r::bits(2), vpn2::bits(27), e::TLBEntry) =
  if e.R == r and (e.G or e.ASID == CP0.EntryHi.ASID) then
  {
    nmask`27 = ~[e.Mask];
    return (e.VPN2 && nmask == vpn2 && nmask)
  }
  else
    false

(bits(9) * TLBEntry) list LookupTLB (r::bits(2), vpn2::bits(27)) =
{
    var found = Nil;
    when CP0.Config6.LTLB do
    {
       b = vpn2<7:0>;
       match TLB_direct (b)
       {
          case Some (e) =>
            when MatchingEntry (r, vpn2, e) do
            {
              index`9 = if TLBAssocEntries <= [b] then
                          [b]
                        else [TLBDirectEntries] + [b];
              found <- list {(index, e)}
            }
          case _ => nothing
       }
    };
    for i in 0 .. TLBAssocEntries - 1 do
       match TLB_assoc ([i])
       {
          case Some (e) =>
             when MatchingEntry (r, vpn2, e) do found <- ([i], e) @ found
          case _ => nothing
       };
   return found
}

unit SignalTLBException_internal (asid::bits(8), vAddr::vAddr) =
{
   r = vAddr<63:62>;
   vpn2 = vAddr<39:13>;
   CP0.BadVAddr <- vAddr;
   CP0.EntryHi.R <- r;
   CP0.EntryHi.VPN2 <- vpn2;
   CP0.EntryHi.ASID <- asid;
   CP0.XContext.R <- r;
   CP0.XContext.BadVPN2 <- vpn2;
   CP0.Context.BadVPN2 <- vAddr<31:13>
}

pAddr * CCA SignalTLBException (e::ExceptionType, asid::bits(8), vAddr::vAddr) =
{
   SignalTLBException_internal(asid,vAddr);
   SignalException (e);
   UNKNOWN(next_unknown)
}

(pAddr * CCA) option * bool CheckSegment (vAddr::vAddr) =
   if UserMode then
     None, vAddr <+ 0x0000_0100_0000_0000       -- xuseg
   else if SupervisorMode then
     None,
     vAddr <+ 0x0000_0100_0000_0000             -- xsuseg
     or
     (0x4000_0000_0000_0000 <=+ vAddr and
      vAddr <+ 0x4000_0100_0000_0000)           -- xsseg
     or
     (0xFFFF_FFFF_C000_0000 <=+ vAddr and
      vAddr <+ 0xFFFF_FFFF_E000_0000)           -- csseg
   else if vAddr <+ 0xC000_0000_0000_0000 then
      if vAddr <+ 0x4000_0000_0000_0000 then
         None, vAddr <+ 0x0000_0100_0000_0000   -- xkuseg/cksseg3
      else if 0x8000_0000_0000_0000 <=+ vAddr then
         Some (vAddr<39:0>, vAddr<61:59>), vAddr<58:40> == 0
                                                -- xkphys (unmapped)
      else
         None, vAddr <+ 0x4000_0100_0000_0000   -- xksseg/cksseg3
   else if vAddr <+ 0xFFFF_FFFF_A000_0000 then
      if 0xFFFF_FFFF_8000_0000 <=+ vAddr then
         Some (vAddr<39:0> - 0xFF_8000_0000, CP0.Config.K0), true
                                                -- ckseg0 (unmapped)
      else
         None, vAddr <+ 0xC000_00FF_8000_0000   -- xkseg/cksseg3
   else if vAddr <+ 0xFFFF_FFFF_C000_0000 then
      Some (vAddr<39:0> - 0xFF_A000_0000, 2), true
                                                -- ckseg1 (unmapped+uncached)
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

unit check_cca (cca::bits(3)) =
  when cca in set {0, 1, 7} do #UNPREDICTABLE("CCA " : [cca] : " Reserved")
