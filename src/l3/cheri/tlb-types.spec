---------------------------------------------------------------------------
-- CHERI extra fields in the EntryLo type
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

register EntryLo :: bits(64)
{
    63 : S        -- Disable store capability instruction
    62 : L        -- Disable load capability instruction
  33-6 : PFN      -- Page Frame Number
   5-3 : C        -- Cacheability and Coherency Attribute
     2 : D        -- Dirty bit
     1 : V        -- Valid bit
     0 : G        -- Global bit
}

construct IorD { INSTRUCTION, DATA }
construct AccessType { LOAD, STORE, CLOAD, CSTORE }
