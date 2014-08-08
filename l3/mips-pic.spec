------------------------------------------------------------
-- A model of BERI's PIC (Programmable Interrupt Controller)
------------------------------------------------------------

register PIC_Config_Reg :: dword
{
   31 : EN       -- Enable/disable this interrupt source
  2-0 : IRQ      -- MIPS interrupt number to which this interrupt
                   -- source will be delivered.
}

-- A PIC containing 128 interrupt sources.
-- Sources 0-63 are "hard" external device interrupts.
-- Sources 64-127 are "soft" software triggered interrupts.

record PIC
{
  base_address      :: bits(37)
  config_regs       :: bits(7) -> PIC_Config_Reg
  ip_bits           :: bits(128)
  external_intrs    :: bits(64)

  -- Bits 0-5 to be fed into MIPS IP2-IP7
  mips_ip_bits      :: bits(6)
}

declare PIC :: PIC

-- To be called whenever there are modifications to:
--   * PIC.config_regs, or
--   * PIC.ip_bits, or
--   * PIC.external_intrs.

unit PIC_update () =
{
  ext`128 = ZeroExtend(PIC.external_intrs);
  var ip :: bits(8);
  ip <- 0;
  when PIC.ip_bits <> 0 or ext <> 0 do
    for i in 0 .. 127 do
      when PIC.ip_bits<i> or ext<i> do 
      {
        reg = PIC.config_regs([i]);
        ip<[reg.IRQ]> <- ip<[reg.IRQ]> or reg.EN
      };
  PIC.mips_ip_bits <- ip<5:0>     -- (IRQs 6 and 7 ignored as in BERI manual)
}

-- Initialisation

unit PIC_initialise (pic::nat) =
{
  PIC.base_address <- [[pic]`40 >>+ 3];
  for i in 0 .. 127 do
    PIC.&config_regs([i]) <- 0;
  for i in 0 .. 4 do
  {
    PIC.config_regs([i]).EN  <- true;
    PIC.config_regs([i]).IRQ <- [i]
  };
  PIC.ip_bits <- 0;
  PIC.external_intrs <- 0;
  PIC_update ()
}

-- Memory-mapped interface

dword PIC_load(addr :: bits(37)) =
{
  offset = addr - PIC.base_address;
  var ret :: dword;
  if offset < 128 then
    ret <- PIC.&config_regs([offset])
  else if offset == 1024 then
    ret <- PIC.ip_bits<63:0> || PIC.external_intrs
  else if offset == 1025 then
    ret <- PIC.ip_bits<127:64>
  else
    ret <- UNKNOWN;
  return ret
}

unit PIC_store(addr :: bits(37), mask :: dword, data :: dword) =
{
  offset = addr - PIC.base_address;
  if offset < 128 then
    PIC.&config_regs([offset]) <- data
  else if offset < 1040 then
    nothing
  else if offset == 1040 then
    PIC.ip_bits<63:0> <- PIC.ip_bits<63:0> || (data && mask)
  else if offset == 1041 then
    PIC.ip_bits<127:64> <- PIC.ip_bits<127:64> || (data && mask)
  else if offset == 1056 then
    PIC.ip_bits<63:0> <- PIC.ip_bits<63:0> && ~(data && mask)
  else if offset == 1057 then
    PIC.ip_bits<127:64> <- PIC.ip_bits<127:64> && ~(data && mask)
  else
    nothing;
  PIC_update ()
}
