---------------------------------------------------------------------------
-- CHERI capability common declarations
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
{-
register Perms :: bits (64)
{
    63-19, 14-11, 9-8: Reserved
    18-15 : soft
       10 : Access_System_Registers
        7 : Permit_Seal
        6 : Permit_Store_Local_Capability
        5 : Permit_Store_Capability
        4 : Permit_Load_Capability
        3 : Permit_Store
        2 : Permit_Load
        1 : Permit_Execute
        0 : Global
}
-}

register UPerms :: bits (32)
{
    31-4 : Reserved
     3-0 : soft
}

register Perms :: bits (32)
{
    31-11, 9 : Reserved
    10 : Access_System_Registers
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
