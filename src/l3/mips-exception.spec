---------------------------------------------------------------------------
-- Standard MIPS exception mechanism
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

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
   vectorOffset = if (ExceptionType == XTLBRefillL or
                      ExceptionType == XTLBRefillS)
                      and not CP0.Status.EXL then
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

-----------------------------------
-- ERET instruction
-----------------------------------
define ERET =
   if CP0.Status.CU0 or KernelMode then
   {
      if CP0.Status.ERL then
      {
         PC <- CP0.ErrorEPC - 4;
         CP0.Status.ERL <- false
      }
      else
      {
         PC <- CP0.EPC - 4;
         CP0.Status.EXL <- false
      };
      LLbit <- Some (false)
   }
   else
     SignalException (CpU)
