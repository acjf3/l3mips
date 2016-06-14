---------------------------------------------------------------------------
-- MIPS TLB address translation
-- (c) Anthony Fox, University of Cambridge
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
                     exc = if  AccessType == LOAD
                           then XTLBRefillL else XTLBRefillS;
                     SignalTLBException (exc, CP0.EntryHi.ASID, vAddr)
                  }
               case list {(_, e)} => match checkMask (e.Mask)
                  {
                     case Some(EvenOddBit) =>
                        {
                           PFN, C, D, V = if vAddr<EvenOddBit> then
                                             e.PFN1, e.C1, e.D1, e.V1
                                          else
                                             e.PFN0, e.C0, e.D0, e.V0;
                           if V then
                              if not D and AccessType == STORE then
                                 SignalTLBException (Mod, e.ASID, vAddr)
                              else
                              {
                                PFN_     = [PFN]   :: bool list;
                                vAddr_   = [vAddr] :: bool list;
                                pAddr    = PFN_<27:EvenOddBit-12>
                                         : vAddr_<EvenOddBit-1:0>;
                                check_cca(C);
                                ([pAddr], C)
                              }
                           else
                           {
                              exc = if AccessType == LOAD then TLBL else TLBS;
                              SignalTLBException (exc, e.ASID, vAddr)
                           }
                        }
                     case _ => #UNPREDICTABLE ("TLB: bad mask")
                  }
               case _ => #UNPREDICTABLE ("TLB: multiple matches")
            }
      }
   else
   {
      CP0.BadVAddr <- vAddr;
      SignalException (if AccessType == LOAD then AdEL else AdES);
      UNKNOWN
   }
}

pAddr option tlbTryTranslation (vAddr::vAddr) =
{
    var ret = None;
    unmapped, valid = CheckSegment (vAddr);
    when valid do
    match unmapped
    {
        case Some (pAddr, cca) => ret <- Some(pAddr)
        case None => match LookupTLB (vAddr<63:62>, vAddr<39:13>)
        {
            case list {(_, e)} => match checkMask (e.Mask)
            {
                case Some(EvenOddBit) =>
                {
                    PFN, C, D, V =
                    if vAddr<EvenOddBit> then
                        e.PFN1, e.C1, e.D1, e.V1
                    else
                        e.PFN0, e.C0, e.D0, e.V0;

                    when V do
                    {
                        PFN_     = [PFN]   :: bool list;
                        vAddr_   = [vAddr] :: bool list;
                        pAddr    = PFN_<27:EvenOddBit-12> : vAddr_<EvenOddBit-1:0>;
                        ret <- Some([pAddr])
                    }
                }
                case _ => #UNPREDICTABLE ("TLB: bad mask")
            }
            case _ => nothing
        }
    };
    ret
}

TLBEntry CP0TLBEntry () =
{
   eHi = CP0.EntryHi;
   eLo1 = CP0.EntryLo1;
   eLo0 = CP0.EntryLo0;
   var e::TLBEntry;
   e.Mask <- CP0.PageMask.Mask;
   e.R <- eHi.R;
   e.VPN2 <- eHi.VPN2;
   e.ASID <- eHi.ASID;
   e.PFN1 <- eLo1.PFN;
   e.C1 <- eLo1.C;
   e.D1 <- eLo1.D;
   e.V1 <- eLo1.V;
   e.G <- eLo1.G and eLo0.G;
   e.PFN0 <- eLo0.PFN;
   e.C0 <- eLo0.C;
   e.D0 <- eLo0.D;
   e.V0 <- eLo0.V;
   return e
}
