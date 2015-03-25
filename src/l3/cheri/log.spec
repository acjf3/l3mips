---------------------------------------------------------------------------
-- CHERI logging utils
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

string hex24 (x::bits(24)) = PadLeft (#"0", 6, [x])
string hex31 (x::bits(31)) = PadLeft (#"0", 8, [x])
string hex40 (x::bits(40)) = PadLeft (#"0", 10, [x])

string log_cap_write (cap::Capability) =
    "u:":(if cap.sealed then "1" else "0"):
    " perms:0x":hex31(cap.perms):
    " type:0x":hex24(cap.otype):
    " offset:0x":hex64(cap.offset):
    " base:0x":hex64(cap.base):
    " length:0x":hex64(cap.length)

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[0x":hex40(pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[0x":hex40(pAddr):"]"
