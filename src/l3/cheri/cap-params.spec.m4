---------------------------------------------------------------------------
-- CHERI capability params
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

ifelse(dnl
CAP,c128posits1,dnl params for 128 bits wide posits compressed capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`UPERMS', `8')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
,dnl
CAP,c128c1,dnl params for 128 bits wide compressed capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`UPERMS', `8')dnl
define(`OTYPEWIDTH', `16')dnl
'dnl
,dnl
CAP,c128c3,dnl params for 128 bits wide candidate 3 capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`UPERMS', `4')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
,dnl
CAP,p64,dnl params for 64 bits wide precise capabilities
`dnl
define(`CAPBYTEWIDTH', 8)dnl
define(`CAPADDR', `bits(37)')dnl
define(`CAPRAWBITS', `bits(64)')dnl
define(`UPERMS', `16')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
,dnl
CAP,p128,dnl params for 128 bits wide precise capabilities
`dnl
define(`CAPBYTEWIDTH', 16)dnl
define(`CAPADDR', `bits(36)')dnl
define(`CAPRAWBITS', `bits(128)')dnl
define(`UPERMS', `16')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
,dnl
`dnl default params (256-bits wide precise capabilities)
define(`CAPBYTEWIDTH', 32)dnl
define(`CAPADDR', `bits(35)')dnl
define(`CAPRAWBITS', `bits(256)')dnl
define(`UPERMS', `16')dnl
define(`OTYPEWIDTH', `24')dnl
'dnl
)dnl
nat `CAPBYTEWIDTH'  = CAPBYTEWIDTH
type `CAPADDR'      = CAPADDR
type `CAPRAWBITS'   = CAPRAWBITS
nat `UPERMS'        = UPERMS
nat `OTYPEWIDTH'    = OTYPEWIDTH
