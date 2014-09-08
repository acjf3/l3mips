---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

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
   { Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr, C2E,
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
            case C2E  => 0x18 -- C2E coprocessor 2 exception
            case XTLBRefillL => 0x02
            case XTLBRefillS => 0x03
         }

bool UserMode =
   CP0.Status.KSU == '10' and not (CP0.Status.EXL or CP0.Status.ERL)

bool SupervisorMode =
   CP0.Status.KSU == '01' and not (CP0.Status.EXL or CP0.Status.ERL)

bool KernelMode = CP0.Status.KSU == '00' or CP0.Status.EXL or CP0.Status.ERL

bool BigEndianMem = CP0.Config.BE
bits(1) ReverseEndian = [CP0.Status.RE and UserMode]
bits(1) BigEndianCPU  = [BigEndianMem] ?? ReverseEndian
