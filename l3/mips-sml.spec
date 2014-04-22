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
         case 0,  0, 0 => [CP0.&Index]
         case 0,  1, 0 => [CP0.&Random]
         case 0,  2, 0 =>  CP0.&EntryLo0
         case 0,  3, 0 =>  CP0.&EntryLo1
         case 0,  5, 0 => [CP0.&PageMask]
         case 0,  8, 0 =>  CP0.BadVAddr
         case 0,  9, 0 => [CP0.Count]
         case 0, 10, 0 =>  CP0.&EntryHi
         case 0, 11, 0 => [CP0.Compare]
         case 0, 12, 0 => [CP0.&Status]
         case 0, 13, 0 => [CP0.&Cause]
         case 0, 14, 0 =>  CP0.EPC
         case 0, 15, 0 => [CP0.PRId]
         case 0, 16, 0 => [CP0.&Config]
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
         case 0,  0, 0 => CP0.Index.Index <- value<5:0>
         case 0,  2, 0 => CP0.&EntryLo0 <- value
         case 0,  3, 0 => CP0.&EntryLo1 <- value
         case 0,  5, 0 => CP0.PageMask.Mask <- value<24:13>
         case 0,  9, 0 => CP0.Count <- value<31:0>
         case 0, 10, 0 => CP0.&EntryHi <- value
         case 0, 11, 0 => CP0.Compare <- value<31:0>
         case 0, 12, 0 => CP0.&Status <- value<31:0>
         case 0, 13, 0 => CP0.&Cause <- value<31:0>
         case 0, 14, 0 => CP0.EPC <- value
         case 0, 16, 0 => CP0.Config.K0 <- value<2:0>
         case 0, 23, 0 => CP0.Debug <- value<31:0>
         case 0, 26, 0 => CP0.ErrCtl <- value<31:0>
         case 0, 30, 0 => CP0.ErrorEPC <- value
         case _ => unmark
      }
   }
}

--------------------------------------------------
-- UART support
--------------------------------------------------

register UART_Status :: half
{
    12 : EOP   -- End-of-packet encountered (optional)
--  11 : CTS   -- Clear-to-send signal (optional)
--  10 : DCTS  -- Change in clear-to-send signal (optional)
     8 : E     -- Exception
     7 : RRDY  -- Receive character ready
     6 : TRDY  -- Transmit ready
     5 : TMT   -- Transmit empty
     4 : TOE   -- Transmit overrun error
     3 : ROE   -- Receive overrun error
     2 : BRK   -- Break detected
     1 : FE    -- Framing error
--   0 : PE    -- Parity error (optional)
}

register UART_Control :: half
{
    12 : IEOP  -- Enable interrupt for end-of-packet
--  11 : RTS   -- Request to send signal
--  10 : IDCTS -- Enable interrupt for a change in CTS signal
     9 : TRBK  -- Transmit break
     8 : IE    -- Enable interrupt for an exception
     7 : IRRDY -- Enable interrupt for a read ready
     6 : ITRDY -- Enable interrupt for a transmission ready
     5 : ITMT  -- Enable interrupt for a transmitter shift register empty
     4 : ITOE  -- Enable interrupt for a transmitter overrun error
     3 : IROE  -- Enable interrupt for a receiver overrun error
     2 : IBRK  -- Enable interrupt for a break detected
     1 : IFE   -- Enable interrupt for a framing error
--   0 : IPE   -- Enable interrupt for a parity error
}

record UART
{
   base_address :: pAddr        -- memory-mapped base address
   rxdata       :: byte         -- offset 0 ; receive data
   txdata       :: byte         -- offset 1 ; transmit data
   status       :: UART_Status  -- offset 2
   control      :: UART_Control -- offset 3
-- divisor      :: half         -- offset 4 ; baud rate divisor
-- endofpacket  :: byte         -- offset 5
   exceptionSignalled :: bool   -- flag for UART interrupt
}

declare UART_RS232 :: UART

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

nat TLBEntries = 16

declare
{
   TLB_direct :: bits(7) -> TLBEntry
   TLB_assoc  :: bits(4) -> TLBEntry
   MEM :: mAddr -> dword                -- physical memory, doubleword access
}

(bits(6) * TLBEntry) list LookupTLB (r::bits(2), vpn2::bits(27)) =
{
   e = TLB_direct (vpn2<6:0>);
   nmask`27 = ~[e.Mask];
   var found = if e.VPN2 && nmask == vpn2 && nmask and e.R == r then
                   list {(16, e)}
               else Nil;
   for i in 0 .. TLBEntries - 1 do
   {
      e = TLB_assoc ([i]);
      nmask`27 = ~[e.Mask];
      when e.VPN2 && nmask == vpn2 && nmask and e.R == r do
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
   UNKNOWN
}

CCA option * bool CheckSegment (vAddr::vAddr) =
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
      Some (vAddr<61:59>), vAddr<58:40> == 0
   else if 0xC000_0000_0000_0000 <=+ vAddr and
           vAddr <+  0xC000_00FF_8000_0000 then -- xkseg
      None, true
   else if 0xFFFF_FFFF_8000_0000 <=+ vAddr and
           vAddr <+  0xFFFF_FFFF_A000_0000 then -- ckseg0 (unmapped)
      Some (CP0.Config.K0), true
   else if 0xFFFF_FFFF_A000_0000 <=+ vAddr and
           vAddr <+  0xFFFF_FFFF_C000_0000 then -- ckseg1 (unmapped+uncached)
      Some (2), true
   else
      None, 0xFFFF_FFFF_C000_0000 <=+ vAddr     -- cksseg/ckseg3

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, LorS::LorS) =
{
   unmapped, valid = CheckSegment (vAddr);
   if valid then
      match unmapped
      {
         case Some (cca) => vAddr<39:0>, cca
         case None =>
            match LookupTLB (vAddr<63:62>, vAddr<39:13>)
            {
               case Nil =>
                  SignalTLBException (XTLBRefill, CP0.EntryHi.ASID, vAddr)
               case list {(_, e)} =>
                  if e.G or e.ASID == CP0.EntryHi.ASID then
                  {
                     PFN, C, D, V = if vAddr<12> then
                                       e.PFN1, e.C1, e.D1, e.V1
                                    else
                                       e.PFN0, e.C0, e.D0, e.V0;
                     if V then
                        if not D and LorS == STORE then
                           SignalTLBException (Mod, e.ASID, vAddr)
                        else
                           PFN : vAddr<11:0>, C
                     else
                     {
                        exc = if LorS == LOAD then TLBL else TLBS;
                        SignalTLBException (exc, e.ASID, vAddr)
                     }
                  }
                  else
                     SignalTLBException (XTLBRefill, e.ASID, vAddr)
               case _ => #UNPREDICTABLE ("TLB: multiple matches")
            }
      }
   else
   {
      SignalException (if LorS == LOAD then AdEL else AdES);
      UNKNOWN
   }
}

-- Pimitive memory load

dword LoadMemory (CCA::CCA, AccessLength::bits(3),
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
{
   when UART_RS232.status.RRDY and pAddr == UART_RS232.base_address and
        match pAddr<2:0>
        {
           case 0 => HALFWORD <=+ AccessLength
           case 1 => true
           case _ => false
        } do
      UART_RS232.status.RRDY <- false;
   MEM (pAddr<39:3>)
}

word loadWord32 (a::pAddr) =
{
   d = MEM (a<39:3>);
   if a<2> then d<31:0> else d<63:32>
}

-- Pimitive memory store. Big-endian.

unit StoreMemory (CCA::CCA, AccessLength::bits(3), MemElem::dword,
                  pAddr::pAddr, vAddr::vAddr, IorD::IorD) =
{  a = pAddr<39:3>;
   l = 64 - ([AccessLength] + 1 + [pAddr<2:0>]) * 0n8;
   mask`64 = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];
   MEM(a) <- MEM(a) && ~mask || MemElem && mask;
   mark (w_mem (pAddr, mask, AccessLength, MemElem));
   when pAddr && 0xFFFFFFFFF8 == UART_RS232.base_address and mask<39:32> <> 0 do
      if UART_RS232.status.TRDY then
         UART_RS232.status.TRDY <- false   -- write to UART transmit register
      else
      {
         UART_RS232.status.TOE <- true;    -- transmit overrun error
         UART_RS232.status.E <- true;
         when UART_RS232.control.ITOE do UART_RS232.exceptionSignalled <- true
      }
}

--------------------------------------------------
-- TLB instructions
--------------------------------------------------

define TLBP =
   match LookupTLB (CP0.EntryHi.R, CP0.EntryHi.VPN2)
   {
      case Nil => CP0.Index.P <- true
      case list {(i, e)} =>
         if e.G or e.ASID == CP0.EntryHi.ASID then
         {
            CP0.Index.P <- false;
            CP0.Index.Index <- i
         }
         else
            CP0.Index.P <- true
      case _ => #UNPREDICTABLE ("TLB: multiple matches")
   }

define TLBR =
{
   i = CP0.Index.Index;
   if i >= [TLBEntries] then
      #UNPREDICTABLE ("TLBR: index > TLBEntries - 1")
   else
   {
      e = TLB_assoc ([i]);
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
}

define TLBWI =
{
   i`4 = [CP0.Index.Index];
   if i >= [TLBEntries] then
   {
      j = CP0.EntryHi.VPN2<6:0>;
      TLB_direct (j) <- ModifyTLB (TLB_direct (j))
   }
   else
      TLB_assoc (i) <- ModifyTLB (TLB_assoc (i))
}

define TLBWR =
{
   j = CP0.EntryHi.VPN2<6:0>;
   old = TLB_direct (j);
   TLB_direct (j) <- ModifyTLB (old);
   when old.V0 and old.V1 do TLB_assoc ([CP0.Random.Random]) <- old
}

-------------------------
-- CACHE op, offset(base)
-------------------------
define CACHE (base::reg, opn::bits(5), offset::bits(16)) =
{
   vAddr = GPR(base) + SignExtend(offset);
   pAddr, cca = AddressTranslation (vAddr, DATA, LOAD);
   nothing
}

--------------------------------------------------
-- UART memory-map
--------------------------------------------------

half * half * half * half UART_RS232_load =
{
   match MEM (UART_RS232.base_address<39:3>)
   {
      case 'a b c d' => (a, b, c, d)
   }
}

unit UART_RS232_store (a::half, b::half, c::half, d::half) =
   MEM (UART_RS232.base_address<39:3>) <- a : b : c : d

unit UART_RS232_read_mm =
{
   rx, tx, st, ct = UART_RS232_load;
   UART_RS232.txdata <- [tx];
   s = UART_Status (st); -- R/C access for some status components
   UART_RS232.status.E   <- UART_RS232.status.E   and s.E;
   UART_RS232.status.TOE <- UART_RS232.status.TOE and s.TOE;
   UART_RS232.status.ROE <- UART_RS232.status.ROE and s.ROE;
   UART_RS232.status.BRK <- UART_RS232.status.BRK and s.BRK;
   UART_RS232.status.FE  <- UART_RS232.status.FE  and s.FE;
   UART_RS232.&control <- [ct]
}

unit UART_RS232_write_mm =
{
   rx, tx, st, ct = UART_RS232_load;
   UART_RS232_store ([UART_RS232.rxdata], tx, UART_RS232.&status, ct)
}

char UART_RS232_putChar =
{
   UART_RS232.status.TRDY <- true;
   when UART_RS232.control.ITRDY do UART_RS232.exceptionSignalled <- true;
   [UART_RS232.txdata]
}

unit UART_RS232_getChar (inp::char option) =
   match inp
   {
      case Some (c) =>
      {
         UART_RS232.rxdata <- [c];
         UART_RS232.status.RRDY <- true;
         when UART_RS232.control.IRRDY do UART_RS232.exceptionSignalled <- true
      }
      case None => ()
   }

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch =
{
   log <- Nil;
   CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired then
                           [TLBEntries - 1]
                        else
                            CP0.Random.Random - 1;
   ti = CP0.Compare == CP0.Count;
   if CP0.Status.IE and CP0.Status.IM<7> and
      (ti or UART_RS232.exceptionSignalled) then
   {
      CP0.Cause.TI <- ti;
      CP0.Cause.IP<7> <- true;
      SignalException (Int);
      UART_RS232.exceptionSignalled <- false;
      None
   }
   else if PC<1:0> == 0 then
   {
      pc, cca = AddressTranslation (PC, INSTRUCTION, LOAD);
      if exceptionSignalled then
         None
      else
         Some (loadWord32 (pc))
   }
   else
   {
      CP0.BadVAddr <- PC;
      SignalException (AdEL);
      None
   }
}

--------------------------------------------------
-- Initialisation and termination
--------------------------------------------------

TLBEntry initTLB = { var e; e.R <- '10'; return e }

unit addTLB (a::vAddr, i::bits(4)) =
{
   pfn = a<39:12>;
   TLB_assoc(i).VPN2 <- [pfn >>+ 1];
   TLB_assoc(i).R <- '00';
   TLB_assoc(i).G <- true;
   if a<12> then
   {
      TLB_assoc(i).PFN1 <- pfn;
      TLB_assoc(i).C1 <- 2;
      TLB_assoc(i).D1 <- true;
      TLB_assoc(i).V1 <- true
   }
   else
   {
      TLB_assoc(i).PFN0 <- pfn;
      TLB_assoc(i).C0 <- 2;
      TLB_assoc(i).D0 <- true;
      TLB_assoc(i).V0 <- true
   }
}

unit initMips (pc::nat, uart::nat) =
{
   CP0.Status.KSU <- '00';
   CP0.Status.EXL <- true;
   CP0.Status.ERL <- true;      -- reset to kernel mode
   CP0.Status.IE <- true;       -- enable interrupts
   CP0.Count <- 0;
   CP0.Compare <- 0;
   CP0.Config.BE  <- true;      -- big-endian
   CP0.Config.MT  <- 1;         -- standard TLB
   CP0.&Status <- 0x044000e0;
   CP0.PRId <- 0x400;           -- processor ID
   CP0.Index.P <- false;
   CP0.Index.Index <- 0x0;
   CP0.Random.Random <- 0x10;
   CP0.Wired.Wired <- 0x2;
   BranchDelay <- None;
   BranchTo <- None;
   LLbit <- None;
   hi <- None;
   lo <- None;
   PC <- [pc];
   UART_RS232.status <- UART_Status (0x0);
   UART_RS232.control <- UART_Control (0x0);
   UART_RS232.status.TRDY <- true;
   UART_RS232.base_address <- [uart] && ~0b111;
   TLB_direct <- InitMap (initTLB);
   TLB_assoc <- InitMap (initTLB);
   addTLB (PC, 0);
   addTLB ([UART_RS232.base_address], 1);
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
