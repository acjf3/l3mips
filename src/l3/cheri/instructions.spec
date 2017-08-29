---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-------------------
-- Helper functions
-------------------
bool register_inaccessible(cb::reg) = not allow_system_reg_access(getPerms(PCC), cb)

bool register_inaccessible_write_attempt(mask::bits(16)) =
{
    var ret = true;
    if mask<15> and register_inaccessible(31) then
        SignalCapException(capExcAccessSysReg,31)
    else if mask<14> and register_inaccessible(30) then
        SignalCapException(capExcAccessSysReg,30)
    else if mask<13> and register_inaccessible(29) then
        SignalCapException(capExcAccessSysReg,29)
    else if mask<11> and register_inaccessible(27) then
        SignalCapException(capExcAccessSysReg,27)
    else if mask<12> and register_inaccessible(28) then
        SignalCapException(capExcAccessSysReg,28)
    else ret <- false;
    ret
}

unit watchOOB(cap::Capability, pc::bits(64)) =
when watchOOBCap do
    if getBase(cap) + getOffset(cap) <+ getBase(cap) then
        mark_watcher("OOB cap @ PC = ":hex(pc):" - ":[[Abs(getOffset(cap))]::nat]:"B below base(":hex(getBase(cap)):") - cap : ":log_cap_write(cap))
    else if getBase(cap) + getOffset(cap) >=+ getBase(cap) + getLength(cap) then
        mark_watcher("OOB cap @ PC = ":hex(pc):" - ":[[getOffset(cap)-getLength(cap)]::nat]:"B above top(":hex(getBase(cap)+getLength(cap)):") - cap : ":log_cap_write(cap))
    else nothing

-- only works for non empty lists
bool list SignExtendBitString(w::nat, x::bool list) = PadLeft (Head(x), w, x)
bool list ZeroExtendBitString(w::nat, x::bool list) = PadLeft (false, w, x)

-----------------------------------
-- dump capability registers
-----------------------------------
define COP2 > CHERICOP2 > DumpCapReg = dumpCRegs()

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetBase (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
        GPR(rd) <- getBase(CAPR(cb))

-----------------------------------
-- CGetOffset rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetOffset (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
        GPR(rd) <- getOffset(CAPR(cb))

-----------------------------------
-- CGetLen rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetLen (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
        GPR(rd) <- getLength(CAPR(cb))

-----------------------------------
-- CGetTag rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetTag (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
        GPR(rd) <- ZeroExtend([getTag(CAPR(cb))])

-----------------------------------
-- CGetSealed rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetSealed (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
    {
        GPR(rd)<0> <- getSealed(CAPR(cb));
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetPerm rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPerm (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
    {
        var perms = 0;
        perms<14:0> <- &getPerms(CAPR(cb))<14:0>;
        perms<(UPERMS+14):15> <- &getUPerms(CAPR(cb))<(UPERMS-1):0>;
        GPR(rd) <- perms
    }

-----------------------------------
-- CGetType rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetType (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if getSealed(CAPR(cb)) then
        GPR(rd) <- ZeroExtend(getType(CAPR(cb)))
    else
        GPR(rd) <- ~0

-----------------------------------
-- CGetPCC cd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCC (cd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else
    {
        CAPR(cd) <- setOffset(PCC, PC);
        watchOOB(CAPR(cd), PC)
    }

-----------------------------------
-- CGetPCCSetOffset cd, rs
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCCSetOffset (cd::reg, rs::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if not canRepOffset (PCC, GPR(rs)) then
    {
        CAPR(cd) <- setOffset(nullCap, getBase(PCC) + GPR(rs));
        watchOOB(CAPR(cd), PC)
    }
    else
    {
        CAPR(cd) <- setOffset(PCC, GPR(rs));
        watchOOB(CAPR(cd), PC)
    }

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if not getPerms(PCC).Access_System_Registers then
        SignalCapException_noReg(capExcAccessSysReg)
    else
    {
        GPR(rd)<7:0> <- capcause.RegNum;
        GPR(rd)<15:8> <- capcause.ExcCode;
        GPR(rd)<63:16> <- 0
    }

-----------------------------------
-- CSetCause rt
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetCause (rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if not getPerms(PCC).Access_System_Registers then
        SignalCapException_noReg(capExcAccessSysReg)
    else
    {
        capcause.ExcCode <- GPR(rt)<15:8>;
        capcause.RegNum <- GPR(rt)<7:0>
    }

-----------------------------------
-- CIncOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if getTag(CAPR(cb)) and getSealed(CAPR(cb)) and GPR(rt) <> 0 then
        SignalCapException(capExcSeal,cb)
    else if not canRepOffset (CAPR(cb), getOffset(CAPR(cb)) + GPR(rt)) then
    {
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + getOffset(CAPR(cb)) + GPR(rt));
        watchOOB(CAPR(cd), PC)
    }
    else
    {
        CAPR(cd) <- setOffset(CAPR(cb), getOffset(CAPR(cb)) + GPR(rt));
        watchOOB(CAPR(cd), PC)
    }

-----------------------------------
-- CSetBounds
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetBounds (cd::reg, cb::reg, rt::reg) =
{
    base   = getBase(CAPR(cb));
    offset = getOffset(CAPR(cb));
    length = getLength(CAPR(cb));
    cursor = getBase(CAPR(cb))+getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if cursor <+ base then
        SignalCapException(capExcLength,cb)
    else if ('0':cursor) + ('0':GPR(rt)) >+ ('0':base) + ('0':length) then
        SignalCapException(capExcLength,cb)
    else
        CAPR(cd) <- setBounds(CAPR(cb), GPR(rt))
}

-----------------------------------
-- CSetBoundsExact
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetBoundsExact (cd::reg, cb::reg, rt::reg) =
{
    base   = getBase(CAPR(cb));
    offset = getOffset(CAPR(cb));
    length = getLength(CAPR(cb));
    cursor = getBase(CAPR(cb))+getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if cursor <+ base then
        SignalCapException(capExcLength,cb)
    else if ('0':cursor) + ('0':GPR(rt)) >+ ('0':base) + ('0':length) then
        SignalCapException(capExcLength,cb)
    else if not canRepBounds(CAPR(cb),GPR(rt)) then
        SignalCapException(capExcInexact,cb)
    else
        CAPR(cd) <- setBounds(CAPR(cb), GPR(rt))
}

-----------------------------------
-- CClearRegs
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearRegs (regset::bits(5), mask::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else match regset
    {
        case 0 => for i in  0 .. 15 do when mask<i> do GPR([i])     <- 0
        case 1 => for i in 16 .. 31 do when mask<i-16> do GPR([i])  <- 0
        case 2 => for i in  0 .. 15 do when mask<i> do CAPR([i])    <- nullCap
        case 3 => when not register_inaccessible_write_attempt(mask) do
                       for i in 16 .. 31 do when mask<i-16> do CAPR([i]) <- nullCap
        case _ => SignalException (ResI)
    }

-----------------------------------
-- CClearTag
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearTag (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else
        CAPR(cd) <- setTag(CAPR(cb), false)

-----------------------------------
-- CAndPerm
-----------------------------------
define COP2 > CHERICOP2 > CSet > CAndPerm (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap  <- setPerms(new_cap, Perms([&getPerms(CAPR(cb))<14:0> && GPR(rt)<14:0>]));
        new_cap  <- setUPerms(new_cap, UPerms([&getUPerms(CAPR(cb))<(UPERMS-1):0> && GPR(rt)<(UPERMS+14):15>]));
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CSetOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if getTag(CAPR(cb)) and getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not canRepOffset (CAPR(cb), GPR(rt)) then
    {
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + GPR(rt));
        watchOOB(CAPR(cd), PC)
    }
    else
    {
        CAPR(cd) <- setOffset(CAPR(cb), GPR(rt));
        watchOOB(CAPR(cd), PC)
    }

-----------------------------------
-- CSub
-----------------------------------
define COP2 > CHERICOP2 > CSub (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else
        GPR(rd) <- getBase(CAPR(cb)) + getOffset(CAPR(cb)) - getBase(CAPR(ct)) - getOffset(CAPR(ct))

-----------------------------------
-- CCheckPerm
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckPerm (cs::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    -- TODO spec should move to uperms in rt<63:32> and perms in rt<31:0>, and no bit-slicing
    else if &getPerms(CAPR(cs))<14:0> && GPR(rt)<14:0> <> GPR(rt)<14:0> then
        SignalCapException(capExcUser,cs)
    else if &getUPerms(CAPR(cs))<(UPERMS-1):0> && GPR(rt)<(UPERMS+14):15> <> GPR(rt)<(UPERMS+14):15> then
        SignalCapException(capExcUser,cs)
    else if GPR(rt)<63:(UPERMS+15)> <> 0 then
        SignalCapException(capExcUser,cs)
    else
        nothing

-----------------------------------
-- CChecType
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckType (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if not getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if getType(CAPR(cs)) <> getType(CAPR(cb)) then
        SignalCapException(capExcType,cs)
    else
        nothing

-----------------------------------
-- CFromPtr
-----------------------------------
define COP2 > CHERICOP2 > CSet > CFromPtr (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if GPR(rt) == 0 then
        CAPR(cd) <- nullCap
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not canRepOffset (CAPR(cb), GPR(rt)) then
    {
        CAPR(cd) <- setOffset(nullCap, getBase(CAPR(cb)) + GPR(rt));
        watchOOB(CAPR(cd), PC)
    }
    else
    {
        CAPR(cd) <- setOffset(CAPR(cb), GPR(rt));
        watchOOB(CAPR(cd), PC)
    }

-----------------------------------
-- CToPtr
-----------------------------------
define COP2 > CHERICOP2 > CGet > CToPtr (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if not getTag(CAPR(cb)) then
        GPR(rd) <- 0
    else
        GPR(rd) <- getBase(CAPR(cb)) + getOffset(CAPR(cb)) - getBase(CAPR(ct))

-----------------------------------
-- CPtrCmp
-----------------------------------
define COP2 > CHERICOP2 > CPtrCmp (rd::reg, cb::reg, ct::reg, t::bits(3)) =
    if not CP0.Status.CU2 then
      SignalCP2UnusableException
    else if register_inaccessible(cb) then
      SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
      SignalCapException(capExcAccessSysReg,ct)
    else
    {
        cap_cb = CAPR(cb);
        cap_ct = CAPR(ct);
        var equal = false;
        var greater;
        var greateru;
        var less;
        var lessu;
        if getTag(cap_cb) <> getTag(cap_ct) then
            if getTag(cap_cb) then
            {
                greater <- true;
                greateru <- true;
                less <- false;
                lessu <- false
            }
            else
            {
                greater <- false;
                greateru <- false;
                less <- true;
                lessu <- true
            }
        else
        {
            cursor1 = getBase(cap_cb) + getOffset(cap_cb); -- mod 2^64
            cursor2 = getBase(cap_ct) + getOffset(cap_ct); -- mod 2^64
            equal <- cursor1 == cursor2;
            greater <- cursor1 > cursor2;
            greateru <- cursor1 >+ cursor2;
            less <- cursor1 < cursor2;
            lessu <- cursor1 <+ cursor2
        };
        match t
        {
           case 0 => GPR(rd) <- [equal]
           case 1 => GPR(rd) <- [not equal]
           case 2 => GPR(rd) <- [less]
           case 3 => GPR(rd) <- [less or equal]
           case 4 => GPR(rd) <- [lessu]
           case 5 => GPR(rd) <- [lessu or equal]
           case 6 => GPR(rd) <- if cap_cb == cap_ct then 1 else 0
	   case 7 => GPR(rd) <- if cap_cb == cap_ct then 0 else 1
        }
    }

-----------------------------------
-- CBTU
-----------------------------------
define COP2 > CHERICOP2 > CBTU (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException(capExcAccessSysReg,cb)
        else if not getTag(CAPR(cb)) then
            BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CBTS
-----------------------------------
define COP2 > CHERICOP2 > CBTS (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException(capExcAccessSysReg,cb)
        else if getTag(CAPR(cb)) then
            BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Capability then
        SignalCapException(capExcPermStoreCap,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Local_Capability
            and getTag(CAPR(cs)) and not getPerms(CAPR(cs)).Global then
        SignalCapException(capExcPermStoreLocalCap,cb)
    else
    {
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb));
        extOff = offset:'0000';
        addr   = cursor + GPR(rt) + SignExtend(extOff);
        if addr + [CAPBYTEWIDTH] >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if addr <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned([addr]) then
        {
            CP0.BadVAddr <- [addr];
            SignalException(AdES)
        }
        else
        {
            _ = StoreCap([addr], CAPR(cs), false);
            LLbit <- None
        }
    }

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else
    {
        cursor = getBase(CAPR(cb)) + getOffset(CAPR(cb));
        extOff = offset:'0000';
        addr   = cursor + GPR(rt) + SignExtend(extOff);
        if addr + [CAPBYTEWIDTH] >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if addr <+ getBase(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if not isCapAligned([addr]) then
        {
            CP0.BadVAddr <- [addr];
            SignalException(AdEL)
        }
        else
        {
            var tmp = LoadCap([addr], false);
            when not getPerms(CAPR(cb)).Permit_Load_Capability do
                tmp <- setTag(tmp, false);
            when not exceptionSignalled do CAPR(cd) <- tmp;
            LLbit <- None
        }
    }

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else
    {
        cap_cb = CAPR(cb);
        cursor = getBase(cap_cb) + getOffset(cap_cb);
        extOff = (([offset<7>]::bits(1))^3:offset) << [t];
        addr   = cursor + GPR(rt) + SignExtend(extOff);
        var size;
        var access;
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2 : '0')
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU : '00')
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD
            }
        };
        if addr + size >+ getBase(cap_cb) + getLength(cap_cb) then
            SignalCapException(capExcLength,cb)
        else if addr <+ getBase(cap_cb) then
            SignalCapException(capExcLength,cb)
        else
        {
            data = LoadMemoryCap(access, true, [addr], false);
            when not exceptionSignalled do
            {
                data_list = [data]::bool list;
                bottom = ([bytesel]::nat)*8;
                top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
                final_data = data_list<top:bottom>;
                if s == 0 then GPR(rd) <- [ZeroExtendBitString(64, final_data)]
                else GPR(rd) <- [SignExtendBitString(64, final_data)]
            }
        }
    }

-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else
    {
        cap_cb = CAPR(cb);
        cursor = getBase(cap_cb) + getOffset(cap_cb); -- mod 2^64 ?
        extOff = (([offset<7>]::bits(1))^3:offset) << [t];
        addr   = cursor + GPR(rt) + SignExtend(extOff);
        var size;
        var access;
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2:'0')
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^1:'00')
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD
            }
        };
        if addr + size >+ getBase(cap_cb) + getLength(cap_cb) then
            SignalCapException(capExcLength,cb)
        else if addr <+ getBase(cap_cb) then
            SignalCapException(capExcLength,cb)
        else
        {
            _ = StoreMemoryCap(access, access, GPR(rs) << (0n8 * [bytesel]), true, [addr], false)
        }
    }

-----------------------------------
-- CLLC
-----------------------------------
define COP2 > CHERICOP2 > CLLC (cd::reg, cb::reg) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if addr + [CAPBYTEWIDTH] >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if not isCapAligned(addr) then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdEL)
    }
    else
    {
        var ret_cap = LoadCap(addr, true);
        when not getPerms(CAPR(cb)).Permit_Load_Capability do
            ret_cap <- setTag(ret_cap, false);
        when not exceptionSignalled do CAPR(cd) <- ret_cap
    }
}

-----------------------------------
-- CLLx
-----------------------------------
define COP2 > CHERICOP2 > CLLx (rd::reg, cb::reg, stt::bits(3)) =
{
    t = stt<1:0>;
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    size = ZeroExtend(1::bits(64) << [t]);
    access_length = if t == '00' then '000' else '1'^[t];
    bytesel = match t
        {
            case '00' => addr<2:0> ?? BigEndianCPU^3
            case '01' => addr<2:0> ?? (BigEndianCPU^2:'0')
            case '10' => addr<2:0> ?? (BigEndianCPU^1:'00')
            case '11' => '000'
        };
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else if addr + size >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        data = LoadMemoryCap(access_length, true, addr, true);
        when not exceptionSignalled do
        {
            data_list = [data]::bool list;
            bottom = ([bytesel]::nat)*8;
            top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
            final_data = data_list<[top]:[bottom]>;
            if not stt<2> then GPR(rd) <- [ZeroExtendBitString(64, [final_data])]
            else GPR(rd) <- [SignExtendBitString(64, [final_data])]
        }
    }
}

-----------------------------------
-- CSCC
-----------------------------------
define COP2 > CHERICOP2 > CSCC (cs::reg, cb::reg, rd::reg) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Capability then
        SignalCapException(capExcPermStoreCap,cb)
    else if not getPerms(CAPR(cb)).Permit_Store_Local_Capability
            and getTag(CAPR(cs)) and not getPerms(CAPR(cs)).Global then
        SignalCapException(capExcPermStoreLocalCap,cb)
    else if addr + [CAPBYTEWIDTH] >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if not isCapAligned(addr) then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdES)
    }
    else
    {
        ret = if StoreCap(addr, CAPR(cs), true) then 1 else 0;
        when not exceptionSignalled do GPR(rd) <- ret
    }
}

-----------------------------------
-- CSCx
-----------------------------------
define COP2 > CHERICOP2 > CSCx (rs::reg, cb::reg, rd::reg, t::bits(2)) =
{
    addr = getBase(CAPR(cb)) + getOffset(CAPR(cb));
    size = ZeroExtend(1::bits(64) << [t]);
    access_length = if t == '00' then '000' else '1'^[t];
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not getPerms(CAPR(cb)).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else if addr + size >+ getBase(CAPR(cb)) + getLength(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if addr <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else
    {
        ret = if StoreMemoryCap(access_length, access_length,
                                     GPR(rs), true, addr, true)
                                     then 1 else 0;
        when not exceptionSignalled do GPR(rd) <- ret
    }
}

-----------------------------------
-- CMOVN
-----------------------------------
define COP2 > CHERICOP2 > CMOVN (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else when not GPR(rt) == 0 do
        CAPR(cd) <- CAPR(cb)

-----------------------------------
-- CMOVZ
-----------------------------------
define COP2 > CHERICOP2 > CMOVZ (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else when GPR(rt) == 0 do
        CAPR(cd) <- CAPR(cb)

{-
-----------------------------------
-- CTestSubset
-----------------------------------
define COP2 > CHERICOP2 > CTestSubset (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else
    {
        cbase = CAPR(cb);
        ctgt  = CAPR(ct);
        GPR(rd) <- if getTag(cbase) <> getTag(ctgt) then 0x0
        else if getSealed(cbase) <> getSealed(ctgt) then 0x0
        else if getBase(ctgt) <+ getBase(cbase) then 0x0
        else if getBase(ctgt) + getLength(ctgt) >+ getBase(cbase) + getLength(cbase) then 0x0
        else if &getPerms(ctgt)<14:0> && &getPerms(cbase)<14:0> <> &getPerms(ctgt)<14:0> then 0x0
        else if &getUPerms(ctgt)<(UPERMS-1):0> && &getUPerms(cbase)<(UPERMS-1):0> <> &getUPerms(ctgt)<(UPERMS-1):0> then 0x0
        else 0x1
    }

-----------------------------------
-- CBuildCap
-----------------------------------
define COP2 > CHERICOP2 > CBuildCap (cd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if getBase(CAPR(ct)) <+ getBase(CAPR(cb)) then
        SignalCapException(capExcLength,cb)
    else if (getBase(CAPR(ct)) + getLength(CAPR(ct))) >+ (getBase(CAPR(cb)) + getLength(CAPR(cb))) then
        SignalCapException(capExcLength,cb)
    else if getLength(CAPR(ct)) <= 0 then
        SignalCapException(capExcLength,ct)
    else if &getPerms(CAPR(ct))<14:0> && &getPerms(CAPR(cb))<14:0> <> &getPerms(CAPR(ct))<14:0> then
        SignalCapException(capExcUser,cb)
    else if &getUPerms(CAPR(ct))<(UPERMS-1):0> && &getUPerms(CAPR(cb))<(UPERMS-1):0> <> &getUPerms(CAPR(ct))<(UPERMS-1):0> then
        SignalCapException(capExcUser,cb)
    else
    {
        var new_cap = CAPR(cb);
        raw = CAPR(ct);
        new_cap  <- setOffset(new_cap, getBase(raw));
        new_cap  <- setBounds(new_cap, getLength(raw));
        new_cap  <- setPerms(new_cap, getPerms(raw));
        new_cap  <- setUPerms(new_cap, getUPerms(raw));
        new_cap  <- setOffset(new_cap, getOffset(raw));
        new_cap  <- setSealed(new_cap, false);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CCopyType
-----------------------------------
define COP2 > CHERICOP2 > CCopyType (cd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if not canRepOffset (CAPR(cb), ZeroExtend(getType(CAPR(ct))) - getBase(CAPR(cb))) then
        SignalCapException(capExcInexact,cb)
    else if getSealed(CAPR(ct)) then
        CAPR(cd) <- setOffset(CAPR(cb), ZeroExtend(getType(CAPR(ct))) - getBase(CAPR(cb)))
    else
        CAPR(cd) <- setOffset(CAPR(cb), ~0)
-}

-----------------------------------
-- CJR
-----------------------------------
define COP2 > CHERICOP2 > CJR (cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException(capExcAccessSysReg,cb)
        else if not getTag(CAPR(cb)) then
            SignalCapException(capExcTag,cb)
        else if getSealed(CAPR(cb)) then
            SignalCapException(capExcSeal,cb)
        else if not getPerms(CAPR(cb)).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if getOffset(CAPR(cb)) + 4 >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if (getBase(CAPR(cb)) + getOffset(CAPR(cb)))<1:0> <> '00' then
        {
            CP0.BadVAddr <- (getBase(CAPR(cb)) + getOffset(CAPR(cb)));
            SignalException(AdEL)
        }
        else
        {
            BranchToPCC <- Some (getOffset(CAPR(cb)), CAPR(cb))
        }
    }

-----------------------------------
-- CJALR
-----------------------------------
define COP2 > CHERICOP2 > CJALR (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cd) then
            SignalCapException(capExcAccessSysReg,cd)
        else if register_inaccessible(cb) then
            SignalCapException(capExcAccessSysReg,cb)
        else if not getTag(CAPR(cb)) then
            SignalCapException(capExcTag,cb)
        else if getSealed(CAPR(cb)) then
            SignalCapException(capExcSeal,cb)
        else if not getPerms(CAPR(cb)).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if getOffset(CAPR(cb)) + 4 >+ getLength(CAPR(cb)) then
            SignalCapException(capExcLength,cb)
        else if (getBase(CAPR(cb)) + getOffset(CAPR(cb)))<1:0> <> '00' then
        {
            CP0.BadVAddr <- (getBase(CAPR(cb)) + getOffset(CAPR(cb)));
            SignalException(AdEL)
        }
        else
        {
            CAPR(cd) <- setOffset(PCC, PC + 8);
            watchOOB(CAPR(cd), PC);
            BranchToPCC <- Some (getOffset(CAPR(cb)), CAPR(cb))
        }
    }

-----------------------------------
-- CSeal
-----------------------------------
define COP2 > CHERICOP2 > CSeal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if getSealed(CAPR(ct)) then
        SignalCapException(capExcSeal,ct)
    else if not getPerms(CAPR(ct)).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if getOffset(CAPR(ct)) >=+ getLength(CAPR(ct)) then
        SignalCapException(capExcLength,ct)
    else if (getBase(CAPR(ct)) + getOffset(CAPR(ct))) >=+ [2**OTYPEWIDTH] then
        SignalCapException(capExcLength,ct)
    else if not canRepSeal (CAPR(cs), true) then
        SignalCapException(capExcInexact,cs)
    else
    {
        var new_cap = CAPR(cs);
        new_cap  <- setSealed(new_cap, true);
        new_cap  <- setType(new_cap, (getBase(CAPR(ct)) + getOffset(CAPR(ct)))<(OTYPEWIDTH-1):0>);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CUnseal
-----------------------------------
define COP2 > CHERICOP2 > CUnseal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException(capExcAccessSysReg,cd)
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(ct) then
        SignalCapException(capExcAccessSysReg,ct)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(ct)) then
        SignalCapException(capExcTag,ct)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if getSealed(CAPR(ct)) then
        SignalCapException(capExcSeal,ct)
    else if (getBase(CAPR(ct)) + getOffset(CAPR(ct))) <> ZeroExtend(getType(CAPR(cs))) then
        SignalCapException(capExcType,ct)
    else if not getPerms(CAPR(ct)).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if getOffset(CAPR(ct)) >=+ getLength(CAPR(ct)) then
        SignalCapException(capExcLength,ct)
    else
    {
        var new_cap = CAPR(cs);
        new_cap <- setSealed(new_cap, false);
        new_cap <- setType(new_cap, 0);
        var p::Perms = getPerms(new_cap);
        p.Global <- getPerms(CAPR(cs)).Global and getPerms(CAPR(ct)).Global;
        new_cap  <- setPerms(new_cap, p);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CCall selector 0
-----------------------------------
define COP2 > CHERICOP2 > CCall0 (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if not getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if getType(CAPR(cs)) <> getType(CAPR(cb)) then
        SignalCapException(capExcType,cs)
    else if not getPerms(CAPR(cs)).Permit_Execute then
        SignalCapException(capExcPermExe,cs)
    else if getPerms(CAPR(cb)).Permit_Execute then
        SignalCapException(capExcPermExe,cb)
    else if getOffset(CAPR(cs)) >=+ getLength(CAPR(cs)) then
        SignalCapException(capExcLength,cs)
    else SignalCapException(capExcCall,cs)

-----------------------------------
-- CCall selector 1
-----------------------------------
define COP2 > CHERICOP2 > CCall1 (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException(capExcAccessSysReg,cs)
    else if register_inaccessible(cb) then
        SignalCapException(capExcAccessSysReg,cb)
    else if not getTag(CAPR(cs)) then
        SignalCapException(capExcTag,cs)
    else if not getTag(CAPR(cb)) then
        SignalCapException(capExcTag,cb)
    else if not getSealed(CAPR(cs)) then
        SignalCapException(capExcSeal,cs)
    else if not getSealed(CAPR(cb)) then
        SignalCapException(capExcSeal,cb)
    else if getType(CAPR(cs)) <> getType(CAPR(cb)) then
        SignalCapException(capExcType,cs)
    else if not getPerms(CAPR(cs)).Permit_CCall then
        SignalCapException(capExcPermCCall,cs)
    else if not getPerms(CAPR(cb)).Permit_CCall then
        SignalCapException(capExcPermCCall,cb)
    else if not getPerms(CAPR(cs)).Permit_Execute then
        SignalCapException(capExcPermExe,cs)
    else if getPerms(CAPR(cb)).Permit_Execute then
        SignalCapException(capExcPermExe,cb)
    else if getOffset(CAPR(cs)) >=+ getLength(CAPR(cs)) then
        SignalCapException(capExcLength,cs)
    else
    {
        CheckBranch;
        new_PCC = setSealed(CAPR(cs), false);
        PCC <- setType(new_PCC, 0);
        BranchTo <- Some (getOffset(CAPR(cs)));
        CCallBranch <- true;
        new_IDC = setSealed(CAPR(cb), false);
        IDC <- setType(new_IDC, 0)
    }

-----------------------------------
-- CReturn
-----------------------------------
define COP2 > CHERICOP2 > CReturn =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalCapException_noReg(capExcRet)

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERICOP2 > UnknownCapInstruction =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalException (ResI)
