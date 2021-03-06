---------------------------------------------------------------------------
-- CHERI exception mechanism
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Exceptions
--------------------------------------------------

construct ExceptionType
{
    Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr,
    CTLBL, CTLBS, C2E, MCheck, XTLBRefillL, XTLBRefillS
}

bits(5) ExceptionCode (e::ExceptionType) = match e
{
    case Int         => 0x00 -- Interrupt
    case Mod         => 0x01 -- TLB modification exception
    case TLBL        => 0x02 -- TLB exception (load or fetch)
    case TLBS        => 0x03 -- TLB exception (store)
    case AdEL        => 0x04 -- Address error (load or fetch)
    case AdES        => 0x05 -- Address error (store)
    case Sys         => 0x08 -- Syscall
    case Bp          => 0x09 -- Breakpoint
    case ResI        => 0x0a -- Reserved instruction
    case CpU         => 0x0b -- Coprocessor Unusable
    case Ov          => 0x0c -- Arithmetic overflow
    case Tr          => 0x0d -- Trap
    case CTLBL       => 0x10 -- Capability TLB Load exception
    case CTLBS       => 0x11 -- Capability TLB Store exception
    case C2E         => 0x12 -- C2E coprocessor 2 exception
    case MCheck      => 0x18 -- Machine Check (i.e. TLB state is invalid)
    case XTLBRefillL => 0x02
    case XTLBRefillS => 0x03
}

construct CapExceptionType
{
    capExcNone,              -- None
    capExcLength,            -- Length Violation
    capExcTag,               -- Tag Violation
    capExcSeal,              -- Seal Violation
    capExcType,              -- Type Violation
    capExcCall,              -- Call Trap
    capExcRet,               -- Return Trap
    capExcUnderflowTSS,      -- Underflow of trusted system stack
    capExcUser,              -- User-defined Permision Violation
    capExcTLBNoStore,        -- TLB prohibits store capability
    capExcInexact,           -- Requested bounds cannot be represented exactly
    capExcGlobal,            -- Global Violation
    capExcPermExe,           -- Permit_Execute Violation
    capExcPermLoad,          -- Permit_Load Violation
    capExcPermStore,         -- Permit_Store Violation
    capExcPermLoadCap,       -- Permit_Load_Capability Violation
    capExcPermStoreCap,      -- Permit_Store_Capability Violation
    capExcPermStoreLocalCap, -- Permit_Store_Local_Capability Violation
    capExcPermSeal,          -- Permit_Seal Violation
    capExcAccessSysReg,      -- Access_System_Registers Violation
    capExcPermCCall,         -- Premit_CCall Violation
    capExcPermCCallIDC,      -- Premit_CCall IDC Violation
    capExcPermUnseal         -- Premit_Unseal Violation
}

bits(8) capExcCode (e::CapExceptionType) = match e
{
    case capExcNone              => 0x0
    case capExcLength            => 0x1
    case capExcTag               => 0x2
    case capExcSeal              => 0x3
    case capExcType              => 0x4
    case capExcCall              => 0x5
    case capExcRet               => 0x6
    case capExcUnderflowTSS      => 0x7
    case capExcUser              => 0x8
    case capExcTLBNoStore        => 0x9
    case capExcInexact           => 0x0a
    case capExcGlobal            => 0x10
    case capExcPermExe           => 0x11
    case capExcPermLoad          => 0x12
    case capExcPermStore         => 0x13
    case capExcPermLoadCap       => 0x14
    case capExcPermStoreCap      => 0x15
    case capExcPermStoreLocalCap => 0x16
    case capExcPermSeal          => 0x17
    case capExcAccessSysReg      => 0x18
    case capExcPermCCall         => 0x19
    case capExcPermCCallIDC      => 0x1a
    case capExcPermUnseal        => 0x1b
}

unit SignalException (ExceptionType::ExceptionType) =
{
    CP0.BadInstrP <- UNKNOWN(next_unknown("BadInstrP"));
    when not CP0.Status.EXL do
    {
        if IsSome (BranchDelay) or IsSome(BranchDelayPCC) then
        {
            mark_log (2, "EPC <- ":hex (PC - 4):" (in branch delay slot => PC - 4 )");
            CP0.EPC <- PC - 4;
            CP0.Cause.BD <- true;
            when IsSome(lastInst) do CP0.BadInstrP <- ValOf(lastInst)
        }
        else
        {
            mark_log (2, "EPC <- ":hex (PC));
            CP0.EPC <- PC;
            CP0.Cause.BD <- false
        }
    };
    vectorOffset =
        if (ExceptionType == XTLBRefillL or ExceptionType == XTLBRefillS)
            and not CP0.Status.EXL then
            0x080`30
        else if (ExceptionType == C2E and
            (capcause.ExcCode == 0x5 {-capExcCall-} or capcause.ExcCode == 0x6 {-capExcRet-})) then
            0x280
        else
            0x180;
    when IsSome(currentInst) do CP0.BadInstr <- ValOf(currentInst);
    CP0.Cause.ExcCode <- ExceptionCode (ExceptionType);
    vectorBase =
        if CP0.Status.BEV then
            0xFFFF_FFFF_BFC0_0200`64
        else
            0xFFFF_FFFF_8000_0000;
    BranchDelay        <- None;
    BranchTo           <- None;
    BranchDelayPCC     <- None;
    BranchToPCC        <- None;
    exceptionSignalled <- true;

    -- move PCC to EPCC
    var new_epcc = PCC;
    if not canRepOffset (PCC, PC) then
        new_epcc <- setOffset(nullCap, getBase(PCC) + PC)
    else
        new_epcc <- setOffset(new_epcc, PC);
    when not CP0.Status.EXL do EPCC <- new_epcc;
    -- move KCC to PCC
    PCC <- KCC;

    --PC <- vectorBase<63:30> : (vectorBase<29:0> + vectorOffset);
    PC <- (vectorBase<63:30> : (vectorBase<29:0> + vectorOffset)) - getBase(PCC);
    CP0.Status.EXL <- true;
    mark_log (2, log_sig_exception(ExceptionCode(ExceptionType)))
}

unit SignalCP1UnusableException = {CP0.Cause.CE <- '01'; SignalException(CpU)}

unit SignalCP2UnusableException = {CP0.Cause.CE <- '10'; SignalException(CpU)}

unit SignalCapException_internal (capException::CapExceptionType, regNum::bits(8)) =
{
    capcause.ExcCode <- capExcCode(capException);
    capcause.RegNum  <- regNum;
    mark_log (2, "Cap exception - cause: 0x" : ToLower ([capcause.ExcCode]) :
                 " (" : [capException] : "), reg: 0x" :
                 ToLower ([capcause.RegNum]));
    SignalException(C2E)
}

unit SignalCapException (capException::CapExceptionType, regNum::bits(5)) =
    SignalCapException_internal (capException, ZeroExtend(regNum))

unit SignalCapException_noReg (capException::CapExceptionType) =
    SignalCapException_internal (capException, 0xff)

unit CheckBranch =
   when IsSome (BranchDelay) or IsSome(BranchDelayPCC) do #UNPREDICTABLE("Not permitted in delay slot")

unit BranchNotTaken =
{
   CheckBranch;
   -- The instruction after a branch is considered to be in a branch delay slot,
   -- even if the branch is not taken.
   BranchTo <- Some (PC + 8)
}

unit BranchLikelyNotTaken =
{
   CheckBranch;
   PC <- PC + 4
}

-----------------------------------
-- ERET instruction
-----------------------------------
define ERET =
{
   CheckBranch;
   if getPerms(PCC).Access_System_Registers then
   {
      if CP0.Status.CU0 or KernelMode then
      {
         PC <- if CP0.Status.ERL then CP0.ErrorEPC - 4 else CP0.EPC - 4;
         CP0.Status.EXL <- false;
         LLbit <- Some (false);
         -- move EPCC to PCC
         PCC <- EPCC
      }
      else SignalException (CpU)
   }
   else SignalCapException_noReg(capExcAccessSysReg)
}
