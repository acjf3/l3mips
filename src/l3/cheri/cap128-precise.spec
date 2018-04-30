---------------------------------------------------------------------------
-- 128-bits wrapper for precice capabilities
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool isCapAligned  (addr::bits(64))  = addr<3:0> == 0

CAPRAWBITS capToBits (cap :: Capability) =
  &cap<127:64> ?? &nullCap<127:64> : &cap<191:128> ?? &nullCap<191:128>

Capability bitsToCap (raw :: CAPRAWBITS) = {
  var newCap = nullCap;
  newCap.cursor <- raw<127:64>;
  newCap.base   <- raw<63:0>;
  return newCap
}

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
if dwordAddr<0> then raw<127:64> else raw<63:0>

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
if dwordAddr<0> then
    (old_blob<127:64> && ~mask || data && mask) : old_blob<63:0>
else
    old_blob<127:64> : (old_blob<63:0> && ~mask || data && mask)
