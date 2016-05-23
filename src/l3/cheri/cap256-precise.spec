---------------------------------------------------------------------------
-- 256-bits wrapper for precice capabilities
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool isCapAligned  (addr::bits(64))  = addr<4:0> == 0

CAPRAWBITS capToBits (cap :: Capability) =
    &cap<63:0> : &cap<127:64> : &cap<191:128> : &cap<255:192>

Capability bitsToCap (raw :: CAPRAWBITS) =
    Capability('0' : raw<63:0> : raw<127:64> : raw<191:128> : raw<255:192>)

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) =
match dwordAddr<1:0>
{
    case '00' => raw<63:0>
    case '01' => raw<127:64>
    case '10' => raw<191:128>
    case '11' => raw<255:192>
}

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
match dwordAddr<1:0>
{
    case '00' => old_blob<255:64>  : (old_blob<63:0>    && ~mask || data && mask)
    case '01' => old_blob<255:128> : (old_blob<127:64>  && ~mask || data && mask) : old_blob<63:0>
    case '10' => old_blob<255:192> : (old_blob<191:128> && ~mask || data && mask) : old_blob<127:0>
    case '11' => (old_blob<255:192> && ~mask || data && mask) : old_blob<191:0>
}
