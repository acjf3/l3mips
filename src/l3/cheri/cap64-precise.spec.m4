---------------------------------------------------------------------------
-- 64-bits wrapper for precice capabilities
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-params.m4')dnl

bool isCapAligned  (addr::bits(64))  = addr<2:0> == 0

CAPRAWBITS capToBits (cap :: Capability) =
  &cap<127:64>

Capability bitsToCap (raw :: CAPRAWBITS) =
    Capability('0' : 0xFEEDF00D_DEADBABE_AA666AA_DEADBABE::bits(128) : raw : 0xFEEDBABE_DEADF00D::bits(64))

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) = raw

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
    (old_blob && ~mask || data && mask)
