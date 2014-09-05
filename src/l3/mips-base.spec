---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

{-

val () = Runtime.LoadF "mips-base.spec, mips-pic.spec, mips-uart.spec,\
                       \mips-sml.spec, mips.spec, mips-encode.spec"

val () = SMLExport.setFunctor true
val () = SMLExport.setFunctor false
val () = SMLExport.spec ("mips-base.spec, mips-pic.spec, mips-uart.spec,\
                     \mips-sml.spec, mips.spec, mips-encode.spec", "sml/mips")

-}

type id     = bits(8)          -- max 256 cores
type reg    = bits(5)
type id_reg = bits(6)          -- width(id_reg) = width(id) + width(reg)
type CCA    = bits(3)
type byte   = bits(8)
type half   = bits(16)
type word   = bits(32)
type dword  = bits(64)
type vAddr  = bits(64)

exception UNPREDICTABLE :: string

--------------------------------------------------
-- Coprocessor 0 registers
--------------------------------------------------

register Index :: word
{
   31 : P         -- Probe failure
  7-0 : Index     -- TLB index
}

register Random :: word
{
  7-0 : Random    -- TLB random index
}

register Wired :: word
{
  7-0 : Wired     -- TLB wired boundary
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
     31 : M         -- Continuation (1 if config register 1 exists)
     15 : BE        -- Big endian
  14-13 : AT        -- Architecture type
  12-10 : AR        -- Architecture revision
    9-7 : MT        -- MMU type
    2-0 : K0        -- kseg0 cacheability and coherency attribute
}

register ConfigRegister1 :: word
{
     31 : M         -- Continuation (1 if config register 2 exists)
  30-25 : MMUSize   -- MMU has MMUSize+1 entries
  24-22 : IS        -- Icache sets per way
  21-19 : IL        -- Icache line size
  18-16 : IA        -- Icache associativity
  15-13 : DS        -- Dcache sets per way
  12-10 : DL        -- Dcache line size
    9-7 : DA        -- Dcache associativity
      6 : C2        -- Co-processor 2 implemented?
      5 : MD        -- MDMX ASE implemented?
      4 : PCR       -- Performance counter registers implemented?
      3 : WR        -- Watch registers implemented?
      2 : CA        -- Code compression (MIPS16) implemented?
      1 : EP        -- EJTAG implemented?
      0 : FP        -- FPU implemented?
}

register ConfigRegister2 :: word
{
     31 : M         -- Continuation (1 if config register 3 exists)
  30-28 : TU        -- Implementation specific tertiary cache control
  27-24 : TS        -- Tertiary cache sets per way
  23-20 : TL        -- Tertiary cache line size
  19-16 : TA        -- Tertiary cache associativity
  15-12 : SU        -- Implementation specific secondary cache control
   11-8 : SS        -- Secondary cache sets per way
    7-4 : SL        -- Secondary cache line size
    3-0 : SA        -- Secondary cache associativity
}

register ConfigRegister3 :: word
{
   31 : M           -- Continuation (1 if config register 4 exists)
   13 : ULRI        -- UserLocal register implemented?
   10 : DSPP        -- MIPS DSPASE implemented?
    7 : LPA         -- Large physical addr support and page grain reg present?
    6 : VEIC        -- External interrupt controller present?
    5 : VInt        -- Vectored interrupts implemented?
    4 : SP          -- Small (1kB) page support?
    2 : MT          -- MIPS MTASE implemented?
    1 : SM          -- SmartMIPS ASI implemented?
    0 : TL          -- Trace Logic implemented?
}

register ConfigRegister6 :: word
{
  31-16 : TLBSize  -- Size of TLB-1
      2 : LTLB     -- Enable large TLB?
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

register HWREna :: word
{
   0 : CPUNum      -- Enable CPU num register
   2 : CC          -- Enable high-res cycle count register
   3 : CCRes       -- Enable CC resolution register
  29 : UL          -- Enable UserLocal register
}

record CP0
{
   Index    :: Index           -- 0   Index to TLB array
   Random   :: Random          -- 1   Pseudorandom pointer to TLB array
   EntryLo0 :: EntryLo         -- 2   Low half of TLB entry for even VPN
   EntryLo1 :: EntryLo         -- 3   Low half of TLB entry for odd VPN
   Context  :: Context         -- 4   Kernel virtual page table entry (PTE)
   UsrLocal :: dword           -- 4   UserLocal register
   PageMask :: PageMask        -- 5   TLB page mask
   Wired    :: Wired           -- 6   Number of wired TLB entries
   HWREna   :: HWREna          -- 7   See RDHWR instruction
   BadVAddr :: dword           -- 8   Bad virtual address
   Count    :: word            -- 9   Timer count
   EntryHi  :: EntryHi         -- 10  High half of TLB entry
   Compare  :: word            -- 11  Timer compare
   Status   :: StatusRegister  -- 12  Status register
   Cause    :: CauseRegister   -- 13  Cause of last exception
   EPC      :: dword           -- 14  Exception program counter
   PRId     :: word            -- 15  Processor revision identifier
   Config   :: ConfigRegister  -- 16  Configuration register
   Config1  :: ConfigRegister1 -- 16  Configuration register 1
   Config2  :: ConfigRegister2 -- 16  Configuration register 2
   Config3  :: ConfigRegister3 -- 16  Configuration register 3
   Config6  :: ConfigRegister6 -- 16  Configuration register 6
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

-- Each piece of state is local to a core.

type RegFile = reg -> dword

declare
{
  c_gpr          :: id -> RegFile       -- general purpose registers
  c_PC           :: id -> dword         -- the program counter
  c_hi           :: id -> dword option  -- mul/div register high result
  c_lo           :: id -> dword option  -- mul/div register low result
  c_CP0          :: id -> CP0           -- CP0 registers
  c_BranchDelay  :: id -> dword option  -- Branch to be taken after instruction
  c_BranchTo     :: id -> dword option  -- Requested branch
  c_LLbit        :: id -> bool option   -- Load link flag
  c_exceptionSignalled :: id -> bool    -- flag exceptions to pick up
                                        -- in branch delay
}

-- Total amount of core(s)
declare totalCore :: nat

-- ID of the core executing current instruction
declare procID :: id

-- The following components provide read/write access to state of the
-- core whose id equals procID.  For example, writing "gpr(r)" refers
-- general purpose register "r" in the core whose id equals procID.

component gpr (n::reg) :: dword
{
   value = { m = c_gpr(procID); m(n) }
   assign value = { var m = c_gpr(procID)
                  ; m(n) <- value
                  ; c_gpr(procID) <- m }
}

component PC :: dword
{
   value = c_PC(procID)
   assign value = c_PC(procID) <- value
}

component hi :: dword option
{
   value = c_hi(procID)
   assign value = c_hi(procID) <- value
}

component lo :: dword option
{
   value = c_lo(procID)
   assign value = c_lo(procID) <- value
}

component CP0 :: CP0
{
   value = c_CP0(procID)
   assign value = c_CP0(procID) <- value
}

component BranchDelay :: dword option
{
   value = c_BranchDelay(procID)
   assign value = c_BranchDelay(procID) <- value
}

component BranchTo :: dword option
{
   value = c_BranchTo(procID)
   assign value = c_BranchTo(procID) <- value
}

component LLbit :: bool option
{
   value = c_LLbit(procID)
   assign value = c_LLbit(procID) <- value
}

component exceptionSignalled :: bool
{
   value = c_exceptionSignalled(procID)
   assign value = c_exceptionSignalled(procID) <- value
}

--------------------------------------------------
-- Exceptions
--------------------------------------------------

construct ExceptionType
   { Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr,
     XTLBRefillL, XTLBRefillS }

unit ExceptionCode (ExceptionType::ExceptionType) =
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
            case XTLBRefillL => 0x02
            case XTLBRefillS => 0x03
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

bool NotWordValue(value::dword) =
{  top = value<63:32>;
   if value<31> then
      top <> 0xFFFF_FFFF
   else
      top <> 0x0
}
