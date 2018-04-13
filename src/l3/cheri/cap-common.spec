---------------------------------------------------------------------------
-- CHERI capability common declarations
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

register UPerms :: bits (32)
{
    31-4 : Reserved
     3-0 : soft
}

register Perms :: bits (32)
{
    31-11 : Reserved
    10 : Access_System_Registers
     9 : Permit_Unseal
     8 : Permit_CCall
     7 : Permit_Seal
     6 : Permit_Store_Local_Capability
     5 : Permit_Store_Capability
     4 : Permit_Load_Capability
     3 : Permit_Store
     2 : Permit_Load
     1 : Permit_Execute
     0 : Global
}
