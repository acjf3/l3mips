dnl---------------------------------------------------------------------------
dnl-- CHERI capability params
dnl-- (c) Alexandre Joannou, University of Cambridge
dnl---------------------------------------------------------------------------
dnl
ifdef(`m4_cap_params',,`define(`m4_cap_params',1)dnl multi-inclusion protection
ifelse(dnl
CAP,128,dnl params for 128 bits wide precise capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`NBPERMS', `31')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
,dnl
CAP,c128,dnl params for 128 bits wide compressed capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`NBPERMS', `23')dnl
define(`OTYPEWIDTH', `16')dnl
'dnl
,dnl
`dnl default params (256-bits wide capabilities)
define(`CAPBYTEWIDTH', 32)dnl
define(`CAPADDR', `bits(35)')dnl
define(`CAPRAWBITS', `bits(256)')dnl
define(`NBPERMS', `31')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
)dnl
')dnl
