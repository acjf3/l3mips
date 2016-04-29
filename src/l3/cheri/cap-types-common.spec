---------------------------------------------------------------------------
-- CHERI types shared by all capability implementations
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

register HwPerms :: bits (9)
{
        8 : Access_System_Registers
        7 : Permit_Seal
        6 : Permit_Store_Local_Capability
        5 : Permit_Store_Capability
        4 : Permit_Load_Capability
        3 : Permit_Store
        2 : Permit_Load
        1 : Permit_Execute
        0 : Global
}

bool allow_system_reg_access(p::HwPerms, r::reg) =
((r == 31 or r == 30 or r == 29 or r == 27 or r == 28) and not p.Access_System_Registers)
