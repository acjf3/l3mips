---------------------------------------------------------------------------
-- CHERI accurate representability check for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool isCapRepresentable(cap::Capability,
                        newSealed::bool,
                        newOffset::bits(64)) =
{
    base   = getBase(cap);
    length = getLength(cap);
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base);
    test_cap <- setBounds(test_cap, length);
    test_cap <- setOffset(test_cap, newOffset);
    test_cap <- setSealed(test_cap, newSealed);
    if getBase(test_cap)   == base   and
       getLength(test_cap) == length and
       getOffset(test_cap) == newOffset then
       true else false
}
