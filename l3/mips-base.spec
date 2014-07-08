---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

type CCA   = bits(3)
type reg   = bits(5)
type byte  = bits(8)
type half  = bits(16)
type word  = bits(32)
type dword = bits(64)
type vAddr = bits(64)

exception UNPREDICTABLE :: string

--------------------------------------------------
-- Coprocessor 0 registers
--------------------------------------------------

register Index :: word
{
   31 : P         -- Probe failure
  5-0 : Index     -- TLB index
}

register Random :: word
{
  5-0 : Random    -- TLB random index
}

register Wired :: word
{
  5-0 : Wired     -- TLB wired boundary
}

register EntryLo :: dword
{
  33-6 : PFN      -- Page Frame Number
   5-3 : C        -- Cacheability and Coherency Attribute
     2 : D        -- Dirty bit
     1 : V        -- Valid bit
     0 : G        -- Global bit
}

register PageMask :: word
{
 24-13 : Mask
}

register EntryHi :: dword
{
 63-62 : R        -- Region (00 user, 01 supervisor, 11 kernel)
 39-13 : VPN2     -- Virtual page number divided by two (maps to two pages)
   7-0 : ASID     -- Address space identifier
}

register StatusRegister :: word
{
   28 : CU0       -- Allow access to CP0
   26 : FR        -- Floating-point data
   25 : RE        -- Reverse endianness
   22 : BEV       -- Controls location of exception vectors
 15-8 : IM        -- Interrupt mask
    7 : KX
    6 : SX
    5 : UX
  4-3 : KSU       -- Operating mode
    2 : ERL       -- Error level
    1 : EXL       -- Exception level
    0 : IE        -- Interrupt enable
}

register ConfigRegister :: word
{
   15 : BE        -- Big endian
  9-7 : MT        -- MMU Type
  2-0 : K0        -- kseg0 cacheability and coherency attribute
}

register CauseRegister :: word
{
    31 : BD       -- In branch delay slot
    30 : TI       -- Timer interrupt is pending
  15-8 : IP       -- Pending hardware/software interrupts
   6-2 : ExcCode  -- Exception code
}

register Context :: dword
{
 63-23 : PTEBase  -- PTE base
  22-4 : BadVPN2  -- The bad Virtual Page Number
}

register XContext :: dword
{
 63-33 : PTEBase  -- PTE base
 32-31 : R        -- The region
  30-4 : BadVPN2  -- The bad Virtual Page Number
}

record CP0
{
   Index    :: Index           -- 0   Index to TLB array
   Random   :: Random          -- 1   Pseudorandom pointer to TLB array
   EntryLo0 :: EntryLo         -- 2   Low half of TLB entry for even VPN
   EntryLo1 :: EntryLo         -- 3   Low half of TLB entry for odd VPN
   Context  :: Context         -- 4   Kernel virtual page table entry (PTE)
   PageMask :: PageMask        -- 5   TLB page mask
   Wired    :: Wired           -- 6   Number of wired TLB entries
-- HWREna   :: word            -- 7   See RDHWR instruction
   BadVAddr :: dword           -- 8   Bad virtual address
   Count    :: word            -- 9   Timer count
   EntryHi  :: EntryHi         -- 10  High half of TLB entry
   Compare  :: word            -- 11  Timer compare
   Status   :: StatusRegister  -- 12  Status register
   Cause    :: CauseRegister   -- 13  Cause of last exception
   EPC      :: dword           -- 14  Exception program counter
   PRId     :: word            -- 15  Processor revision identifier
   Config   :: ConfigRegister  -- 16  Configuration register
   LLAddr   :: dword           -- 17  Load linked address
-- WatchLo  :: word            -- 18  Memory reference trap address low bits
-- WatchHi  :: word            -- 19  Memory reference trap address high bits
   XContext :: XContext        -- 20  PTE entry in 64-bit mode
-- Reserved                    -- 21
-- Implementation dependent    -- 22
   Debug    :: word            -- 23  EJTAG Debug register
-- DEPC     :: word            -- 24  Program counter EJTAG debug exception
-- PerfCnt  :: word            -- 25  Performance counter interface
   ErrCtl   :: word            -- 26  Error Control
-- CacheErr :: word            -- 27  Cache error and status register
-- TagLo    :: word            -- 28  Cache tag register
-- TagHi    :: word            -- 29  Cache tag register
   ErrorEPC :: dword           -- 30  Error exception program counter
-- KScratch :: word            -- 31  Scratch Registers for Kernel Mode
}

--================================================
-- The register state space
--================================================

declare
{
   gpr          :: reg -> dword   -- general purpose registers
   PC           :: dword          -- the program counter
   hi           :: dword option   -- multiply and divide register high result
   lo           :: dword option   -- multiply and divide register low result
   CP0          :: CP0            -- CP0 registers
   BranchDelay  :: dword option   -- Branch to be taken after instruction
   BranchTo     :: dword option   -- Requested branch
   LLbit        :: bool option    -- Load link flag
   exceptionSignalled :: bool     -- flag exceptions to pick up in branch delay
}

--------------------------------------------------
-- Exceptions
--------------------------------------------------

construct ExceptionType
   { Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr,
     XTLBRefill }

unit ExceptionCode (ExceptionType::ExceptionType) =
   when [ExceptionType] < 0n12 do
      CP0.Cause.ExcCode <-
         match ExceptionType
         {
            case Int  => 0x00 -- Interrupt
            case Mod  => 0x01 -- TLB modification exception
            case TLBL => 0x02 -- TLB exception (load or fetch)
            case TLBS => 0x03 -- TLB exception (store)
            case AdEL => 0x04 -- Address error (load or fetch)
            case AdES => 0x05 -- Address error (store)
            case Sys  => 0x08 -- Syscall
            case Bp   => 0x09 -- Breakpoint
            case ResI => 0x0a -- Reserved instruction
            case CpU  => 0x0b -- Coprocessor Unusable
            case Ov   => 0x0c -- Arithmetic overflow
            case Tr   => 0x0d -- Trap
            case _    => UNKNOWN
         }

unit SignalException (ExceptionType::ExceptionType) =
{
   when not CP0.Status.EXL do
   {
      if IsSome (BranchDelay) then
      {
         CP0.EPC <- PC - 4;
         CP0.Cause.BD <- true
      }
      else
      {
         CP0.EPC <- PC;
         CP0.Cause.BD <- false
      }
   };
   vectorOffset = if ExceptionType == XTLBRefill and not CP0.Status.EXL then
                     0x080`30
                  else
                     0x180;
   ExceptionCode (ExceptionType);
   CP0.Status.EXL <- true;
   vectorBase = if CP0.Status.BEV then
                   0xFFFF_FFFF_BFC0_0200`64
                else
                   0xFFFF_FFFF_8000_0000;
   BranchDelay <- None;
   BranchTo <- None;
   PC <- vectorBase<63:30> : (vectorBase<29:0> + vectorOffset);
   exceptionSignalled <- true
}

--------------------------------------------------
-- Memory access
--------------------------------------------------

construct IorD  { INSTRUCTION, DATA }
construct LorS  { LOAD, STORE }

bits(3) BYTE       = 0`3
bits(3) HALFWORD   = 1`3
bits(3) WORD       = 3`3
bits(3) DOUBLEWORD = 7`3

bool UserMode =
   CP0.Status.KSU == '10' and not (CP0.Status.EXL or CP0.Status.ERL)

bool SupervisorMode =
   CP0.Status.KSU == '01' and not (CP0.Status.EXL or CP0.Status.ERL)

bool KernelMode = CP0.Status.KSU == '00' or CP0.Status.EXL or CP0.Status.ERL

bool BigEndianMem = CP0.Config.BE
bits(1) ReverseEndian = [CP0.Status.RE and UserMode]
bits(1) BigEndianCPU  = [BigEndianMem] ?? ReverseEndian
