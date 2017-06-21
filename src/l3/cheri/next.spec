---------------------------------------------------------------------------
-- Next state transition function
-- (c) Alexandre JOANNOU, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- The next state function
--================================================

string log_instruction (w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] : " " : dhex (PC + getBase(PCC)) :
    " : " : dhex (w) : "   " : instructionToString(inst)

unit Next =
{
    clear_watcher;
    clear_logs;
    currentInst <- None;
    currentInst <- Fetch;
    match currentInst
    {
        case Some (w) =>
        {
            inst = Decode (w);
            mark_log (1, log_instruction (w,inst));
            Run (inst)
        }
        case None => nothing
    };
    match BranchDelay, BranchTo, BranchDelayPCC, BranchToPCC
    {
        case None, None, None, None =>
            when not exceptionSignalled do PC <- PC + 4
        case Some (addr), None, None, None =>
        {
            BranchDelay <- None;
            CCallBranchDelay <- false;
            PC <- addr
        }
        case None, Some (addr), None, None =>
        {
            BranchDelay <- Some (addr);
            CCallBranchDelay <- CCallBranch;
            BranchTo <- None;
            CCallBranch <- false;
            PC <- PC + 4
        }
        case None, None, Some (addr, cap), None =>
        {
            BranchDelayPCC <- None;
            CCallBranchDelay <- false;
            PC <- addr;
            PCC <- cap
        }
        case None, None, None, Some (addr, cap) =>
        {
            BranchDelayPCC <- Some (addr, cap);
            CCallBranchDelay <- CCallBranch;
            BranchToPCC <- None;
            CCallBranch <- false;
            PC <- PC + 4
        }
        case _ => #UNPREDICTABLE("Branch follows branch")
    };
    exceptionSignalled <- false;
    CP0.Count <- CP0.Count + 1
}
