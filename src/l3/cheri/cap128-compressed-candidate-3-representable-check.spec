---------------------------------------------------------------------------
-- CHERI accurate representability check for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool canRepCap( cap::Capability,
                newSeal::bool,
                newOffset::bits(64),
                newLength::bits(64)) =
{
    base = getBase(cap);
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base);
    test_cap <- setBounds(test_cap, newLength);
    test_cap <- setOffset(test_cap, newOffset);
    test_cap <- setSealed(test_cap, newSeal);
    if getBase(test_cap) == base and
       getOffset(test_cap) == newOffset and
       getLength(test_cap) == newLength and
       getSealed(test_cap) == newSeal then
       true else false
}
bool canRepOffset(cap::Capability, newOffset::bits(64)) =
    canRepCap(cap,getSealed(cap),newOffset,getLength(cap))
bool canRepSeal(cap::Capability, newSeal::bool) =
    canRepCap(cap,newSeal,getOffset(cap),getLength(cap))
bool canRepBounds(cap::Capability, newLength::bits(64)) =
    canRepCap(cap,getSealed(cap),getOffset(cap),newLength)
