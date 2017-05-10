string reg_name (n::reg) =
  "$" :
  match n
  {
     case 0 => "zero"
     case 1 => "at"
     case 2 => "v0"
     case 3 => "v1"
     case 4 => "a0"
     case 5 => "a1"
     case 6 => "a2"
     case 7 => "a3"
     case 8 => "t0"
     case 9 => "t1"
     case 10 => "t2"
     case 11 => "t3"
     case 12 => "t4"
     case 13 => "t5"
     case 14 => "t6"
     case 15 => "t7"
     case 16 => "s0"
     case 17 => "s1"
     case 18 => "s2"
     case 19 => "s3"
     case 20 => "s4"
     case 21 => "s5"
     case 22 => "s6"
     case 23 => "s7"
     case 24 => "t8"
     case 25 => "t9"
     case 26 => "k0"
     case 27 => "k1"
     case 28 => "gp"
     case 29 => "sp"
     case 30 => "fp"
     case 31 => "ra"
  }

inline string c (n::reg) = ", " : reg_name(n)
string ihex (n::bits(N)) = (if n <+ 10 then "" else "0x") : ToLower([n])
inline string ai (n::bits(N)) = ihex([n]`32 << 2)
inline string lai (n::bits(N)) = ihex([n + 1]`32 << 2)
inline string oi (n::bits(N)) = if n == 0 then "" else ihex(n)
inline string mnemonic (s::string) = PadRight (#" ", 12, s : " ")

string op1i (s::string, n::bits(N)) = mnemonic(s) : ihex(n)
string op1ai (s::string, n::bits(N)) = mnemonic(s) : ai(n)
string op1lai (s::string, n::bits(N)) = mnemonic(s): lai(n)
string op1r (s::string, n::reg) = mnemonic(s) : reg_name(n)
string op1ri (s::string, r1::reg, n::bits(N)) = op1r(s,r1) : ", " : ihex(n)
string op1rai (s::string, r1::reg, n::bits(N)) = op1r(s,r1) : ", " : ai(n)
string op1rlai (s::string, r1::reg, n::bits(N)) = op1r(s,r1) : ", " : lai(n)
string op2r (s::string, r1::reg, r2::reg) = op1r(s,r1) : c(r2)
string op2ri (s::string, r1::reg, r2::reg, n::bits(N)) =
  op2r(s,r1,r2) : ", " : ihex(n)
string op2rai (s::string, r1::reg, r2::reg, n::bits(N)) =
  op2r(s,r1,r2) : ", " : ai(n)
string op2rlai (s::string, r1::reg, r2::reg, n::bits(N)) =
  op2r(s,r1,r2) : ", " : lai(n)
string op3r (s::string, r1::reg, r2::reg, r3::reg) = op2r(s,r1,r2) : c(r3)
string op2roi (s::string, r1::reg, r2::reg, n::bits(N)) =
  op1r(s,r1) : ", " : cpr (r2) : oi(n)

string opmem (s::string, r1::reg, r2::reg, n::bits(N)) =
  op1ri(s,r1,n) : "(" : reg_name(r2) : ")"

inline string fp_reg_name (n::reg) = "$f" : [[n]::nat]
inline string fcc (n::bits(3)) = ", $fcc" : [[n]::nat]
inline string ofcc (n::bits(3)) =
  if n == 0 then "" else "$fcc" : [[n]::nat] : ", "
inline string fpc (n::reg) = ", " : fp_reg_name(n)

string op1fpr (s::string, n::reg) = mnemonic(s) : fp_reg_name(n)
string op1fpri (s::string, r1::reg, n::bits(N)) = op1fpr(s,r1) : ", " : ihex(n)
string op2fpr (s::string, r1::reg, r2::reg) = op1fpr(s,r1) : fpc(r2)
string op2rfpr (s::string, r1::reg, r2::reg) = op1r(s,r1) : fpc(r2)
string op2rcfpr (s::string, r1::reg, r2::reg) =
  op1r(s,r1) :  ", $" : [[r2]::nat]
string op2ccfpr (s::string, r1::reg, r2::reg, n::bits(3)) =
  mnemonic(s) : ofcc(n) : fp_reg_name(r1) : fpc(r2)
string op3fpr (s::string, r1::reg, r2::reg, r3::reg) = op2fpr(s,r1,r2) : fpc(r3)
string op4fpr (s::string, r1::reg, r2::reg, r3::reg, r4::reg) =
  op3fpr(s,r1,r2,r3) : fpc(r4)

string opfpmem (s::string, r1::reg, r2::reg, n::bits(N)) =
   op1fpri(s,r1,n) : "(" : reg_name(r2) : ")"

string opfpmem2 (s::string, r1::reg, r2::reg, r3::reg) =
   op1fpr(s,r1) : ", " : reg_name(r3) : "(" : reg_name(r2) : ")"

word form1 (rs::reg, rt::reg, rd::reg, imm5::bits(5), function::bits(6)) =
   '000000' : rs : rt : rd : imm5 : function

word form2 (rs::reg, function::bits(5), imm::bits(16)) =
   '000001' : rs : function : imm

word form3 (function::bits(5), rt::reg, rd::reg, sel::bits(3)) =
   '010000' : function : rt : rd : '00000000' : sel

word form4 (function::bits(6), rs::reg, rt::reg, imm::bits(16)) =
   function : rs : rt : imm

word form5 (rs::reg, rt::reg, rd::reg, function::bits(6)) =
   '011100' : rs : rt : rd : '00000' : function

word form6 (rt::reg, rd::reg, function::bits(6)) =
   '011111' : '00000' : rt : rd : '00000' : function
