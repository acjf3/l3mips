---------------------------------------------------------------------------
-- Simple memory model
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

-- utils functions
---------------------------------------------------------------------------

bool Aligned (vAddr::vAddr, MemType::bits(3)) = [vAddr] && MemType == 0

pAddr AdjustEndian (MemType::bits(3), pAddr::pAddr) =
  match MemType
  {
     case 0 => pAddr ?? [ReverseEndian^3]
     case 1 => pAddr ?? [ReverseEndian^2 : '0']
     case 3 => pAddr ?? [ReverseEndian : '00']
     case 7 => pAddr
     case _ => #UNPREDICTABLE ("bad access length")
  }

-- dummy stubs
---------------------------------------------------------------------------

declare watchPaddr :: pAddr option
unit initMemAccessStats = nothing
unit initMemStats = nothing

string printMemAccessStats = "No MemAccessStats implemented"
string csvHeaderMemAccessStats = "No csvHeaderAccessStats implemented"
string csvMemAccessStats = "No csvMemAccessStats implemented"
string printMemStats = "No MemStats implemented"
string csvHeaderMemStats = "No csvHeaderMemStats implemented"
string csvMemStats = "No csvMemStats implemented"
unit clearDynamicMemStats () = nothing

-- memory types and declarations / sml stubs
---------------------------------------------------------------------------

declare mem :: bits(37) -> dword

unit InitMEM = mem <- InitMap (UNKNOWN)
unit WriteDWORD (pAddr::bits(37), data::dword) = mem(pAddr) <- data
unit Write256 (pAddr::bits(35), data::bits(256)) =
{
    mem(pAddr:'00') <- data<63:0>;
    mem(pAddr:'01') <- data<127:64>;
    mem(pAddr:'10') <- data<191:128>;
    mem(pAddr:'11') <- data<255:192>
}

-- memory API
---------------------------------------------------------------------------

-- virtual address computation
vAddr getVirtualAddress (addr::bits(64)) = addr

-- memory load

dword LoadMemory
   (MemType::bits(3), AccessLength::bits(3), needAlign::bool, vAddr::vAddr, link::bool) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    CP0.BadVAddr <- vAddr;
    SignalException (AdEL);
    return UNKNOWN
  }
  else
  {
    pAddr = AdjustEndian (MemType, [vAddr]);
    if link then
    {
      LLbit <- Some (true);
      CP0.LLAddr <- [pAddr]
    }
    else
      LLbit <- None;
    mem(pAddr<39:3>)
  }

-- memory store

bool StoreMemory
   (MemType::bits(3), AccessLength::bits(3), needAlign::bool, MemElem::dword,
    vAddr::vAddr, cond::bool) =
  if needAlign and not Aligned (vAddr, MemType) then
  {
    CP0.BadVAddr <- vAddr;
    SignalException (AdES);
    return false
  }
  else
  {
    pAddr = AdjustEndian (MemType, [vAddr]);
    sc_success = if cond then match LLbit
      {
        case None => #UNPREDICTABLE ("conditional store: LLbit not set")
        case Some (false) => false
        case Some (true) =>
          if CP0.LLAddr == [pAddr] then
            true
          else
            #UNPREDICTABLE
              ("conditional store: address doesn't match previous LL address")
      } else true;
    b = [AccessLength] + 0n1;
    l = 64 - (b + [vAddr<2:0>]) * 0n8;
    mask`64 = [2 ** (l + b * 0n8) - 2 ** l];
    when sc_success do
    {
       for core in 0 .. totalCore - 1 do
       {   i = [core];
           st = all_state (i);
           when i <> procID and st.c_LLbit == Some (true) and
                st.c_CP0.LLAddr<39:3> == pAddr<39:3> do
                   all_state(i).c_LLbit <- Some (false)
       };
       mem(pAddr<39:3>) <- mem(pAddr<39:3>) && ~mask || MemElem && mask
    };
    LLbit <- None;
    sc_success
  }

-- instruction fetch

unit Fetch =
{
   currentInst <- None;

   CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired then
                           [TLBAssocEntries - 1]
                        else
                           CP0.Random.Random - 1;

   when CP0.Compare == CP0.Count do
   {
      CP0.Cause.IP<7> <- true
      -- TI not in MIPS R4000
      -- CP0.Cause.TI <- true
   };

   when CP0.Status.IE and not (CP0.Status.EXL or CP0.Status.ERL) do
   {
      -- If any interrupts pending, raise an exception
      when CP0.Status.IM<7:2> && CP0.Cause.IP<7:2> <> 0 do
        SignalException (Int)
   };

   if exceptionSignalled then
      nothing
   else if PC<1:0> == 0 then
   {
      currentInst <- Some (if PC<2> then mem (PC<39:3>)<31:0> else mem (PC<39:3>)<63:32>)
   }
   else
   {
      CP0.BadVAddr <- PC;
      SignalException (AdEL)
   }
}
