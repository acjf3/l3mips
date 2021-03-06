---------------------------------------------------------------------------
-- CHERI types for 256-bits precise capability
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-----------------------
-- types definitions --
--------------------------------------------------------------------------------

register Capability :: bits (257)
{
        256 : tag
    255-192 : length
    191-128 : base
     127-64 : cursor
      63-56 : reserved
      55-32 : otype
      31-16 : uperms
       15-1 : perms
          0 : sealed
}

--------------------------------------
-- capability "typeclass" functions --
--------------------------------------------------------------------------------

Capability defaultCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- true;
    new_cap.sealed   <- false;
    new_cap.cursor   <- 0;
    new_cap.base     <- 0;
    new_cap.length   <- ~0;
    new_cap.otype    <- 0;
    new_cap.uperms   <- ~0;
    new_cap.perms    <- ~0;
    new_cap.reserved <- 0;
    new_cap
}

Capability nullCap =
{
    var new_cap :: Capability;
    new_cap.tag      <- false;
    new_cap.sealed   <- false;
    new_cap.cursor   <- 0;
    new_cap.base     <- 0;
    new_cap.length   <- ~0;
    new_cap.otype    <- 0;
    new_cap.uperms   <- 0;
    new_cap.perms    <- 0;
    new_cap.reserved <- 0;
    new_cap
}

bool canRepOffset(cap::Capability, newOffset::bits(64)) = true
bool canRepSeal(cap::Capability, newSeal::bool) = true
bool canRepBounds(cap::Capability, newLength::bits(64)) = true

------------------------------------
-- capability "typeclass" getters --
--------------------------------------------------------------------------------

bool     getTag    (cap::Capability) = cap.tag
bits(24) getType   (cap::Capability) = cap.otype
Perms    getPerms  (cap::Capability) = Perms(ZeroExtend(cap.perms))
UPerms   getUPerms (cap::Capability) = UPerms(ZeroExtend(cap.uperms))
bool     getSealed (cap::Capability) = cap.sealed
bits(64) getOffset (cap::Capability) = cap.cursor - cap.base
bits(64) getBase   (cap::Capability) = cap.base
bits(64) getLength (cap::Capability) = cap.length

bits(64) * bits(64) getBaseAndLength (cap::Capability) = (cap.base, cap.length)

------------------------------------
-- capability "typeclass" setters --
--------------------------------------------------------------------------------

Capability setTag    (cap::Capability, tag::bool)        = {var new_cap = cap; new_cap.tag    <- tag;    new_cap}
Capability setType   (cap::Capability, otype::bits(24))  = {var new_cap = cap; new_cap.otype  <- otype;  new_cap}
Capability setPerms  (cap::Capability, perms::Perms)     = {var new_cap = cap; new_cap.perms  <- &perms<14:0>; new_cap}
Capability setUPerms (cap::Capability, uperms::UPerms)   = {var new_cap = cap; new_cap.uperms <- &uperms<15:0>; new_cap}
Capability setSealed (cap::Capability, sealed::bool)     = {var new_cap = cap; new_cap.sealed <- sealed; new_cap}
Capability setOffset (cap::Capability, offset::bits(64)) = {var new_cap = cap; new_cap.cursor <- offset+cap.base; new_cap}
Capability setBounds (cap::Capability, length::bits(64)) =
{
    var new_cap = cap;
    new_cap.base   <- cap.cursor;
    new_cap.length <- length;
    new_cap
}

---------------
-- log utils --
--------------------------------------------------------------------------------

string log_cap_write (cap::Capability) =
    "t:":(if getTag(cap) then "1" else "0"):
    " s:":(if getSealed(cap) then "1" else "0"):
    " perms:":hex (cap.uperms:cap.perms): -- TODO report 2 fields
    " type:":hex (getType(cap)):
    " offset:":hex (getOffset(cap)):
    " base:":hex (getBase(cap)):
    " length:":hex (getLength(cap))

string log_cpp_write (cap::Capability) = "PCC <- ":log_cap_write(cap)
string log_creg_write (r::reg, cap::Capability) = "CapReg ":[[r]::nat]:" <- ":log_cap_write(cap)
string log_store_cap (pAddr::pAddr, cap::Capability) = "MEM[":hex (pAddr):"] <- ":log_cap_write(cap)
string log_load_cap (pAddr::pAddr, cap::Capability) =  log_cap_write(cap) : " <- MEM[":hex (pAddr):"]"
