---------------------------------------------------------------------------
-- MIPS TLB types
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

register EntryLo :: bits(64)
{
  33-6 : PFN      -- Page Frame Number
   5-3 : C        -- Cacheability and Coherency Attribute
     2 : D        -- Dirty bit
     1 : V        -- Valid bit
     0 : G        -- Global bit
}

construct IorD { INSTRUCTION, DATA }
construct AccessType { LOAD, STORE }
