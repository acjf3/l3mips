---------------------------------------------------------------------------
-- MIPS TLB address translation
-- (c) Anthony Fox, University of Cambridge
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

unit switchCoreTLB (n:nat) = nothing

pAddr * CCA AddressTranslation (vAddr::vAddr, IorD::IorD, AccessType::AccessType) =
{
    unmapped, valid = CheckSegment (vAddr);
    if valid then
        match unmapped
        {
            case Some (addr, cca) =>
            {
                check_cca(cca);
                return ([addr], cca)
            }
            case _ => return ([vAddr], 0)
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
