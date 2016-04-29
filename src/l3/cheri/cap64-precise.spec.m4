---------------------------------------------------------------------------
-- 64-bits wrapper for precice capabilities
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------
dnl
include(`helpers.m4')dnl
include(`cap-common.m4')dnl

bool isCapAligned  (addr::bits(64))  = addr<2:0> == 0

CAPRAWBITS capToBits (cap :: Capability) =
  &cap<191:128>

Capability bitsToCap (raw :: CAPRAWBITS) =
    Capability('0' : 0xFEEDF00D_DEADBABE::bits(64) : raw : 0xFEEDBABE_DEADF00D::bits(128))

dword readDwordFromRaw (dwordAddr::bits(37), raw::CAPRAWBITS) = raw

CAPRAWBITS updateDwordInRaw (dwordAddr::bits(37), data::dword, mask::dword, old_blob::CAPRAWBITS) =
    (old_blob && ~mask || data && mask)
