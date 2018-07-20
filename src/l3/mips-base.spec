---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

declare done :: bool   -- Flag to request termination of the simulation

--================================================
-- The register state space
--================================================

-- Each piece of state is local to a core.

type RegFile = reg -> dword

record CoreStats
{
    branch_taken :: nat
    branch_not_taken :: nat
}

record procState {
  c_PC           :: dword         -- the program counter
  c_hi           :: dword option  -- mul/div register high result
  c_lo           :: dword option  -- mul/div register low result
  c_CP0          :: CP0           -- CP0 registers
  c_BranchDelay  :: dword option  -- Branch to be taken after instruction
  c_BranchTo     :: dword option  -- Requested branch
  c_LLbit        :: bool option   -- Load link flag
  c_CoreStats    :: CoreStats     -- core statistics
  c_exceptionSignalled :: bool    -- flag exceptions to pick up
}                                 -- in branch delay

declare {
  all_gpr     :: id -> RegFile
  all_state   :: id -> procState
  c_gpr       :: RegFile
  c_state     :: procState
  instCnt     :: nat             -- Instruction counter
  currentInst :: bits(32) option -- Current instruction
  totalCore   :: nat             -- Total amount of core(s)
  procID      :: id              -- ID of the core executing current instruction
}

-- To be initialised by run.sml, tells whether to warn on out of bounds
-- capabilities in the capability case
declare watchOOBCap :: bool

-- The following components provide read/write access to state of the
-- core whose id equals procID.  For example, writing "gpr(r)" refers
-- general purpose register "r" in the core whose id equals procID.

component gpr (n::reg) :: dword
{
   value = c_gpr(n)
   assign value = c_gpr(n) <- value
}

component GPR (n::reg) :: dword
{
   value = if n == 0 then 0 else gpr(n)
   assign value =
      when n <> 0 do
      { gpr(n) <- value;
        mark_log (2, log_w_gpr (n, value))
      }
}

component PC :: dword
{
   value = c_state.c_PC
   assign value = c_state.c_PC <- value
}

component hi :: dword option
{
   value = c_state.c_hi
   assign value = c_state.c_hi <- value
}

component lo :: dword option
{
   value = c_state.c_lo
   assign value = c_state.c_lo <- value
}

component CP0 :: CP0
{
   value = c_state.c_CP0
   assign value = c_state.c_CP0 <- value
}

component BranchDelay :: dword option
{
   value = c_state.c_BranchDelay
   assign value = c_state.c_BranchDelay <- value
}

component BranchTo :: dword option
{
   value = c_state.c_BranchTo
   assign value = c_state.c_BranchTo <- value
}

component LLbit :: bool option
{
   value = c_state.c_LLbit
   assign value = c_state.c_LLbit <- value
}

component exceptionSignalled :: bool
{
   value = c_state.c_exceptionSignalled
   assign value = c_state.c_exceptionSignalled <- value
}

bool UserMode =
   CP0.Status.KSU == '10' and not (CP0.Status.EXL or CP0.Status.ERL)

bool SupervisorMode =
   CP0.Status.KSU == '01' and not (CP0.Status.EXL or CP0.Status.ERL)

bool KernelMode = CP0.Status.KSU == '00' or CP0.Status.EXL or CP0.Status.ERL

bool BigEndianMem = CP0.Config.BE
bits(1) ReverseEndian = [CP0.Status.RE and UserMode]
bits(1) BigEndianCPU  = [BigEndianMem] ?? ReverseEndian

bool NotWordValue (value::dword) =
{
   top = value<63:31>;
   return top != 0 and top != 0x1_FFFF_FFFF
}

-- dump regs --

unit dumpRegs () =
{
    mark_log (0, "======   Registers   ======")
  ; mark_log (0, "DEBUG MIPS COREID " : [[procID]::nat])
  ; mark_log (0, "DEBUG MIPS PC\t" : hex (PC))
  ; for i in 0 .. 31 do
      mark_log (0, "DEBUG MIPS REG " : (if i < 10 then " " else "") : [i] : "\t" :
                hex (GPR([i])))
}

-- stats utils --

component coreStats :: CoreStats
{
   value = c_state.c_CoreStats
   assign value = c_state.c_CoreStats <- value
}

unit initCoreStats =
{
    coreStats.branch_taken <- 0;
    coreStats.branch_not_taken <- 0
}

string printCoreStats =
    PadRight (#" ", 16, "branch_taken") : " = " :
    PadLeft (#" ", 9, [coreStats.branch_taken]) : "\n" :
    PadRight (#" ", 16, "branch_not_taken") : " = " :
    PadLeft (#" ", 9, [coreStats.branch_not_taken])

string csvHeaderCoreStats = "branch_taken,branch_not_taken"
string csvCoreStats = [coreStats.branch_taken] : "," : [coreStats.branch_not_taken]

-- Handle unique UNKNOWN(next_unknown("foo")) values
declare unknown_counters :: string -> nat
nat * string next_unknown (s::string) =
{
	n = unknown_counters(s);
	unknown_counters(s) <- n + 1;
	(n,s)
}
