---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--================================================
-- The next state function
--================================================

unit Next =
{
   match Fetch
   {
      case Some (w) => Run (Decode (w))
      case None => nothing
   } ;
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
