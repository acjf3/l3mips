---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- The next state function
--================================================

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] : " " :
    dhex (PC) : " : " : dhex (w) : "   " : instructionToString(inst)

unit Next =
{
    clear_watcher;
    clear_logs;
    Fetch;
    match currentInst
    {
        case Some (w) =>
        {
            inst = Decode (w);
            mark_log (1, log_instruction(w,inst));
            Run (inst)
        }
        case None => nothing
    };
    match BranchDelay, BranchTo
    {
        case None, None => when not exceptionSignalled do PC <- PC + 4
        case Some (addr), None =>
        {
            BranchDelay <- None;
            PC <- addr
        }
        case None, Some (addr) =>
        {
            BranchDelay <- Some (addr);
            BranchTo <- None;
            PC <- PC + 4
        }
        case _ => #UNPREDICTABLE("Branch follows branch")
    };
    exceptionSignalled <- false;
    CP0.Count <- CP0.Count + 1
}
