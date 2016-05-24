---------------------------------------------------------------------------
-- CHERI caches
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

define(bw,ifelse(regexp(CAP,p64),0,8,regexp(CAP,c128c\|c128c3\|p128),0,16,32))dnl
nat CAPPERL1LINE = L1LINESIZE div bw
ifelse(eval(L1LINESIZE%bw), 0, ,`errprint(`The L1 line size('L1LINESIZE` bytes) must be a multiple of the capability size('bw` bytes)') m4exit(1)')dnl
nat CAPPERL2LINE = L2LINESIZE div bw
ifelse(eval(L2LINESIZE%bw), 0, ,`errprint(`The L2 line size('L2LINESIZE` bytes) must be a multiple of the capability size('bw` bytes)') m4exit(1)')dnl
undefine(bw)dnl
--define(`L1LINEPERL2LINE', eval(L2LINESIZE/L1LINESIZE))dnl
