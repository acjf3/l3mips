dnl--------------------------------------------------------------------------------
dnl-- utils macro
dnl-- (c) Alexandre Joannou, University of Cambridge
dnl--------------------------------------------------------------------------------
dnl
ifdef(`m4_helpers',,`define(`m4_helpers',1)dnl
define(`log2', `ifelse($1, 1, 0, `eval(1 + log2(eval($1 / 2)))')')dnl compute log2
')dnl
