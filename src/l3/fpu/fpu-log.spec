string log_w_fgr (r::reg, data::dword) =
   "FP Reg " : [[r]::nat] : " <- 0x" : hex64 (data)

