---------------------------------------------------------------------------
-- Log utils
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Logging
--------------------------------------------------

string cpr (r::reg) =
   "c0_" :
   match r
   {
      case 0 => "index"
      case 1 => "random"
      case 2 => "entrylo0"
      case 3 => "entrylo1"
      case 4 => "context"
      case 5 => "pagemask"
      case 6 => "wired"
      case 7 => "hwrena"
      case 8 => "badvaddr"
      case 9 => "count"
      case 10 => "entryhi"
      case 11 => "compare"
      case 12 => "status"
      case 13 => "cause"
      case 14 => "epc"
      case 15 => "prid"
      case 16 => "config"
      case 17 => "lladdr"
      case 18 => "watchlo"
      case 19 => "watchhi"
      case 20 => "xcontext"
      case 21 => "21"
      case 22 => "22"
      case 23 => "debug"
      case 24 => "depc"
      case 25 => "perfcnt"
      case 26 => "errctl"
      case 27 => "cacheerr"
      case 28 => "taglo"
      case 29 => "taghi"
      case 30 => "errorepc"
      case 31 => "kscratch"
   }

string sig_exception (ExceptionCode::bits(5)) = "Exception 0x" : [ExceptionCode]
string w_gpr (r::reg, data::dword) = "Reg " : [[r]::nat] : " <- 0x" : [data]
string w_hi (data::dword) = "HI <- 0x" : [data]
string w_lo (data::dword) = "LO <- 0x" : [data]
string w_c0 (r::reg, data::dword) = cpr(r) : " <- 0x" : [data]

string w_mem (pAddr::pAddr, mask::bits(64), sz::bits(3), data::dword) =
   "Address 0x" : [pAddr] : " <- 0x" : [data] : " [" : [[sz]::nat] :
   " bytes], mask 0x" : [mask]

declare log :: nat -> string list   -- One log per "trace level"

unit mark (lvl::nat, s::string) = log(lvl) <- s @ log(lvl)
unit unmark (lvl::nat) = log(lvl) <- Tail (log(lvl))

string hex64 (x::dword) = PadLeft (#"0", 16, [x])
