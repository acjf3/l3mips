---------------------------------------------------------------------------
-- BERI caches
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- L1 compile time values (direct mapped L1)
---------------------------------------------------------------------------

-- L1 cache size in bytes (default 16KB)
nat `L1SIZE' = L1SIZE
-- L1 associativity (default direct mapped)
nat `L1WAYS' = L1WAYS
-- L1 line size in bytes (default 32B)
nat `L1LINESIZE' = L1LINESIZE

-- L2 compile time values
---------------------------------------------------------------------------

-- L2 cache size in bytes (default 64KB)
nat `L2SIZE' = L2SIZE
-- L2 associativity (default direct mapped)
nat `L2WAYS' = L2WAYS
-- L2 line size in bytes (default 32B)
nat `L2LINESIZE' = L2LINESIZE

ifelse(eval(L2LINESIZE%L1LINESIZE), 0, ,`errprint(`The L2 line size('L2LINESIZE` bytes) must be a multiple of the L1 line size('L1LINESIZE` bytes)') m4exit(1)')dnl
ifelse(eval(L2LINESIZE<L1LINESIZE), 0, ,`errprint(`The L2 line size('L2LINESIZE` bytes) must be  greater than or equal to the L1 line size('L1LINESIZE` bytes)') m4exit(1)')dnl
dnl
