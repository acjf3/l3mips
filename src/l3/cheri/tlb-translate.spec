---------------------------------------------------------------------------
-- CHERI MIPS TLB address translation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, AccessType::AccessType) =
{
    unmapped, valid = CheckSegment (vAddr);
    if valid then
        match unmapped
        {
            case Some (pAddr, cca) => pAddr, cca
                case None =>
                match LookupTLB (vAddr<63:62>, vAddr<39:13>)
                {
                    case Nil =>
                    {
                        exc = if  AccessType == LOAD then XTLBRefillL
                              else XTLBRefillS;
                        SignalTLBException (exc, CP0.EntryHi.ASID, vAddr)
                    }
                    case list {(_, e)} =>
                    {
                        EvenOddBit = match e.Mask
                        {
                            case 0b0000_0000_0000 => 12
                                case 0b0000_0000_0011 => 14
                                case 0b0000_0000_1111 => 16
                                case 0b0000_0011_1111 => 18
                                case 0b0000_1111_1111 => 20
                                case 0b0011_1111_1111 => 22
                                case 0b1111_1111_1111 => 24
                                case _                => #UNPREDICTABLE ("TLB: bad mask")
                        };

                        S, L, PFN, C, D, V =
                        if vAddr<EvenOddBit> then
                            e.S1, e.L1, e.PFN1, e.C1, e.D1, e.V1
                        else
                            e.S0, e.L0, e.PFN0, e.C0, e.D0, e.V0;

                        if V then
                            if not D and AccessType == STORE then
                                SignalTLBException (Mod, e.ASID, vAddr)
                            else if L and AccessType == CLOAD then
                                SignalTLBException (CTLBL, e.ASID, vAddr)
                            else if S and AccessType == CSTORE then
                                SignalTLBException (CTLBS, e.ASID, vAddr)
                            else
                            {
                                PFN_     = [PFN]   :: bool list;
                                vAddr_   = [vAddr] :: bool list;
                                pAddr    = PFN_<27:EvenOddBit-12>
                                   : vAddr_<EvenOddBit-1:0>;
                                ([pAddr], C)
                            }
                        else
                        {
                            exc = if AccessType == LOAD then TLBL else TLBS;
                            SignalTLBException (exc, e.ASID, vAddr)
                        }
                    }
                    case _ => #UNPREDICTABLE ("TLB: multiple matches")
                }
        }
    else
    {
        CP0.BadVAddr <- PCC.base + vAddr;
        SignalException (if AccessType == LOAD then AdEL else AdES);
        UNKNOWN
    }
}
