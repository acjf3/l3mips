---------------------------------------------------------------------------
-- CHERI fast representability check for 128-bits candidate 3
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool canRepCap( cap::Capability,
                newSealed::bool,
                newOffset::bits(64)) =
{
    tb, bb = match cap.sFields
    {
        case Unsealed(uf) => uf.topBits, uf.baseBits
        case Sealed(sf)   => sf.topBits:(0`12), sf.baseBits:(0`12)
    };
    sealOk = if newSealed then tb<11:0> == 0 and bb<11:0> == 0 else true;
    e::nat         = cap.exp;                     -- exponent
    i::bits(64)    = newOffset - getOffset(cap);  -- increment
    imid::bits(20) = i<e+19:e>;                   -- increment's mid bits (20 bits)
    addr::bits(20) = cap.cursor<e+19:e>;          -- addr field mid bits (20 bits)
    edge::bits(20) = bb - 0x1000`20; -- 2^12      -- representable region limit (20 bits)
    -- TODO ask anthony about this "all" function business
    mask::bits(64) = ~[2**(e+20) - 1];
    inRange = if i && mask == mask or i && mask == 0 then true else false;
    inLimits = if i >= 0 then imid <+ (edge - addr - 1)
               else imid >=+ (edge - addr) and edge != addr;
    return ((inRange and inLimits) or e >= 44) and sealOk
}
bool canRepOffset(cap::Capability, newOffset::bits(64)) =
    canRepCap(cap,getSealed(cap),newOffset)
bool canRepSeal(cap::Capability, newSeal::bool) =
    canRepCap(cap,newSeal,getOffset(cap))
bool canRepBounds(cap::Capability, newLength::bits(64)) =
{
    base = getBase(cap);
    offset = getOffset(cap);
    sealed = getSealed(cap);
    var test_cap = defaultCap;
    test_cap <- setOffset(test_cap, base + offset);
    test_cap <- setBounds(test_cap, newLength);
    test_cap <- setOffset(test_cap, 0);
    test_cap <- setSealed(test_cap, sealed);
    if getBase(test_cap) == base + offset and
       getOffset(test_cap) == 0 and
       getLength(test_cap) == newLength and
       getSealed(test_cap) == sealed then
       true else false
}
