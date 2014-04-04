---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

type CCA   = bits(3)
type reg   = bits(5)
type byte  = bits(8)
type word  = bits(32)
type dword = bits(64)
type vAddr = bits(64)
type pAddr = bits(64)

exception UNPREDICTABLE :: string

--------------------------------------------------
-- Coprocessor 0 registers
--------------------------------------------------

register StatusRegister :: word
{
   28 : CU0       -- Allow access to CP0
   26 : FR        -- Floating-point data
   25 : RE        -- Reverse endianness
   22 : BEV       -- Controls location of exception vectors
    9 : IM1       -- Interrupt mask
    8 : IM0       -- Interrupt mask
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
}

register CauseRegister :: word
{
   31  : BD       -- In branch delay slot
   6-2 : ExcCode  -- Exception code
}

record CP0
{
-- Index    :: word            -- 0   Index to TLB array
-- Random   :: word            -- 1   Pseudorandom pointer to TLB array
-- EntryLo0 :: word            -- 2   Low half of TLB entry for even VPN
-- EntryLo1 :: word            -- 3   Low half of TLB entry for odd VPN
-- Context  :: word            -- 4   Kernel virtual page table entry (PTE)
-- PageMask :: word            -- 5   TLB page mask
-- Wired    :: word            -- 6   Number of wired TLB entries
-- HWREna   :: word            -- 7   See RDHWR instruction
   BadVAddr :: dword           -- 8   Bad virtual address
   Count    :: word            -- 9   Timer count
-- EntryHi  :: word            -- 10  High half of TLB entry
   Compare  :: word            -- 11  Timer compare
   Status   :: StatusRegister  -- 12  Status register
   Cause    :: CauseRegister   -- 13  Cause of last exception
   EPC      :: dword           -- 14  Exception program counter
   PRId     :: word            -- 15  Processor revision identifier
   Config   :: ConfigRegister  -- 16  Configuration register
   LLAddr   :: dword           -- 17  Load linked address
-- WatchLo  :: word            -- 18  Memory reference trap address low bits
-- WatchHi  :: word            -- 19  Memory reference trap address high bits
-- XContext :: word            -- 20  PTE entry in 64-bit mode
-- Reserved                    -- 21
-- Implementation dependent    -- 22
   Debug    :: word            -- 23  EJTAG Debug register
-- DEPC     :: word            -- 24  Program counter EJTAG debug exception
-- PerfCnt  :: word            -- 25  Performance counter interface
   ErrCtl   :: word            -- 26  Error Control
-- CacheErr :: word            -- 27  Cache error and status register
-- TagLo    :: word            -- 28  Cache tage register
-- TagHi    :: word            -- 29  Cache tage register
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

construct ExceptionType { AdEL, AdES, Sys, Bp, RI, CpU, Ov, Tr }

bits(5) ExceptionCode (ExceptionType::ExceptionType) =
   match ExceptionType
   {
      case AdEL => 0x04 -- Address error (load or fetch)
      case AdES => 0x05 -- Address error (store)
      case Sys  => 0x08 -- Syscall
      case Bp   => 0x09 -- Breakpoint
      case RI   => 0x0a -- Reserved instruction
      case CpU  => 0x0b -- Coprocessor Unusable
      case Ov   => 0x0c -- Arithmetic overflow
      case Tr   => 0x0d -- Trap
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
   vectorOffset = 0x180`30;
   CP0.Cause.ExcCode <- ExceptionCode (ExceptionType);
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

nat PSIZE = 64 -- 64-bit physical memory

bool KernelMode = CP0.Status.KSU == '00' or CP0.Status.EXL or CP0.Status.ERL

bool UserMode =
   CP0.Status.KSU == '10' and not CP0.Status.EXL and not CP0.Status.ERL

bool BigEndianMem = CP0.Config.BE
bits(1) ReverseEndian = [CP0.Status.RE and UserMode]
bits(1) BigEndianCPU  = [BigEndianMem] ?? ReverseEndian
