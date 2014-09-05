---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

type pAddr = bits(40)
type mAddr = bits(37)

nat PSIZE = 40 -- 40-bit physical memory

-- Code used for SML emulation of MIPS

construct event
{
   w_gpr :: reg * dword
   w_hi :: dword
   w_lo :: dword
   w_c0 :: reg * dword
   w_mem :: pAddr * vAddr * bits(3) * dword
}

declare log :: event list

unit mark (e::event) = log <- e @ log
unit unmark = log <- Tail (log)


word flip_endian_word (w::word) =
  match w { case 'a`8 b`8 c`8 d' => d : c : b : a }

dword flip_endian_dword (dw::dword) =
  match dw { case 'a`8 b`8 c`8 d`8 e`8 f`8 g`8 h' =>
                  h : g : f : e : d : c : b : a }

nat TLBEntries = 16

declare
{
   MEM :: mAddr -> dword                -- physical memory, doubleword access
}

--------------------------------------------------
-- Gereral purpose register access
--------------------------------------------------

component GPR (n::reg) :: dword
{
   value = if n == 0 then 0 else gpr(n)
   assign value = when n <> 0 do { gpr(n) <- value; mark (w_gpr (n, value)) }
}

declare {
  UNPREDICTABLE_LO :: unit -> unit
  UNPREDICTABLE_HI :: unit -> unit
}

component HI :: dword
{
   value = match hi { case Some (v) => v
                      case None => { UNPREDICTABLE_HI (); UNKNOWN }
                    }
   assign value = { hi <- Some (value); mark (w_hi (value)) }
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => { UNPREDICTABLE_LO (); UNKNOWN }
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
      mark (w_c0 (reg, value));
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
         case 0, 13, 0 => CP0.&Cause <- value<31:0>
         case 0, 14, 0 => CP0.EPC <- value
         case 0, 16, 0 => CP0.Config.K0 <- value<2:0>
         case 0, 16, 2 => CP0.Config2.SU <- value<15:12>
         case 0, 16, 6 => CP0.Config6.LTLB <- value<2>
         case 0, 20, 0 => CP0.XContext.PTEBase <- value<63:33>
         case 0, 23, 0 => CP0.Debug <- value<31:0>
         case 0, 26, 0 => CP0.ErrCtl <- value<31:0>
         case 0, 30, 0 => CP0.ErrorEPC <- value
         case _ => unmark
      }
   }
}

--------------------------------------------------
-- Memory access
--------------------------------------------------

record TLBEntry
{
   Mask :: bits(12)
   R    :: bits(2)
   VPN2 :: bits(27)
   G    :: bool
   ASID :: bits(8)
   PFN0 :: bits(28)
   PFN1 :: bits(28)
   C0   :: bits(3)
   C1   :: bits(3)
   D0   :: bool
   D1   :: bool
   V0   :: bool
   V1   :: bool
}

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

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, LorS::LorS) =
{
   unmapped, valid = CheckSegment (vAddr);
   if valid then
      match unmapped
      {
         case Some (pAddr, cca) => pAddr, cca
         case None =>
            match LookupTLB (vAddr<63:62>, vAddr<39:13>)
            {
               case Nil =>
                  {
                     exc = if  LorS == LOAD
                           then XTLBRefillL else XTLBRefillS;
                     SignalTLBException (exc, CP0.EntryHi.ASID, vAddr)
                  }
               case list {(_, e)} =>
                  {
                     EvenOddBit = match e.Mask
                                  {
                                    case 0b0000_0000_0000 => 12
                                    case 0b0000_0000_0011 => 14
                                    case 0b0000_0000_1111 => 16
                                    case 0b0000_0011_1111 => 18
                                    case 0b0000_1111_1111 => 20
                                    case 0b0011_1111_1111 => 22
                                    case 0b1111_1111_1111 => 24
                                    case _                =>
                                      #UNPREDICTABLE ("TLB: bad mask")
                                  };
                     PFN, C, D, V = if vAddr<EvenOddBit> then
                                       e.PFN1, e.C1, e.D1, e.V1
                                    else
                                       e.PFN0, e.C0, e.D0, e.V0;
                     if V then
                        if not D and LorS == STORE then
                           SignalTLBException (Mod, e.ASID, vAddr)
                        else
                        {
                          PFN_     = [PFN]   :: bool list;
                          vAddr_   = [vAddr] :: bool list;
                          pAddr    = PFN_<27:EvenOddBit-12>
                                   : vAddr_<EvenOddBit-1:0>;
                          ([pAddr], C)
                        }
                     else
                     {
                        exc = if LorS == LOAD then TLBL else TLBS;
                        SignalTLBException (exc, e.ASID, vAddr)
                     }
                  }
               case _ => #UNPREDICTABLE ("TLB: multiple matches")
            }
      }
   else
   {
      CP0.BadVAddr <- vAddr;
      SignalException (if LorS == LOAD then AdEL else AdES);
      UNKNOWN
   }
}

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

-------------------------
-- CACHE op, offset(base)
-------------------------
define CACHE (base::reg, opn::bits(5), offset::bits(16)) =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
   {
     vAddr = GPR(base) + SignExtend(offset);
     pAddr, cca = AddressTranslation (vAddr, DATA, LOAD);
     nothing
   }

---------------
-- RDHWR rt, rd
---------------

define RDHWR (rt::reg, rd::reg) =
   if CP0.Status.CU0 or KernelMode or CP0.&HWREna<[rd]::nat> then
   {
      match rd
      {
         case  0 => GPR(rt) <- [totalCore - 1]
         case  2 => GPR(rt) <- SignExtend(CP0.Count)
         case  3 => GPR(rt) <- 1
         case 29 => GPR(rt) <- CP0.UsrLocal
         case _  => SignalException(ResI)
      }
   }
   else
     SignalException(ResI)

--------------------------------------------------
-- Initialisation and termination
--------------------------------------------------

TLBEntry initTLB = { var e; e.R <- '10'; return e }
