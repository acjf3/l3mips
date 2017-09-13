---------------------------------------------------------------------------
-- CHERI MIPS TLB address translation
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

pAddr * CCA * bool * bool AddressTranslation (vAddr::vAddr, AccessType::AccessType) =
{
    unmapped, valid = CheckSegment (vAddr);
    if valid then
        match unmapped
        {
            case Some (addr, cca) =>
            {
                check_cca(cca);
                ([addr], cca, false, false)
            }
            case _ => ([vAddr], 0, false, false)
        }
    else
    {
        CP0.BadVAddr <- vAddr;
        SignalException (if AccessType == LOAD then AdEL else AdES);
        UNKNOWN(next_unknown)
    }
}

pAddr option tlbTryTranslation (vAddr::vAddr) = None

TLBEntry CP0TLBEntry () = UNKNOWN(next_unknown)

unit SignalTLBCapException (capException::CapExceptionType, asid::bits(8), vAddr::vAddr) =
{
   SignalTLBException_internal(asid,vAddr);
   SignalCapException_noReg(capException);
   UNKNOWN(next_unknown)
}
