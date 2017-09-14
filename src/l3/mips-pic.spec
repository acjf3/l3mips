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

type PICSource = bits(7)

declare
{
  PIC_base_address      :: id -> bits(37)
  PIC_config_regs       :: id -> (PICSource -> PIC_Config_Reg)
  PIC_ip_bits           :: id -> bits(128)
  PIC_external_intrs    :: id -> bits(64)
}

-- To be called whenever there are modifications to:
--   * config_regs, or
--   * ip_bits, or
--   * external_intrs.

unit PIC_update (id :: id) =
{
  config_regs = PIC_config_regs(id);
  ext`128 = ZeroExtend(PIC_external_intrs(id));
  ip_bits = PIC_ip_bits(id) || ext;
  var ip = 0`8;
  when ip_bits <> 0 do
  {
    var i = 0n127;
    foreach b in [ip_bits] do
    {
      when b do
      {
        reg = config_regs([i]);
        ip<[reg.IRQ]> <- ip<[reg.IRQ]> or reg.EN
      };
      i <- i - 1
    }
  };
  -- (IRQs 5, 6 and 7 currently ignored)
  if id == procID then
    c_state.c_CP0.Cause.IP<6:2> <- ip<4:0>
  else
    all_state(id).c_CP0.Cause.IP<6:2> <- ip<4:0>
}

-- Initialisation

unit PIC_initialise (pic::nat) =
{
  id = procID;
  PIC_base_address(id) <- [[pic]`40 >>+ 3];
  var config_regs = PIC_config_regs(id);
  for i in 0 .. 127 do &config_regs([i]) <- 0;
  for i in 0 .. 4 do
  {
    config_regs([i]).EN  <- true;
    config_regs([i]).IRQ <- [i]
  };
  PIC_config_regs(id) <- config_regs;
  PIC_ip_bits(id) <- 0;
  PIC_external_intrs(id) <- 0;
  PIC_update (id)
}

-- Memory-mapped interface (assumes double-word access)

dword PIC_load(id :: id, addr :: bits(37)) =
{
  offset = addr - PIC_base_address(id);
  config_regs = PIC_config_regs(id);
  if offset < 128 then
    &config_regs([offset])
  else if offset == 1024 then
    PIC_ip_bits(id)<63:0> || PIC_external_intrs(id)
  else if offset == 1025 then
    PIC_ip_bits(id)<127:64>
  else
    UNKNOWN(next_unknown("pic-data"))
}

unit PIC_store(id :: id, addr :: bits(37), mask :: dword, data :: dword) =
{
  offset = addr - PIC_base_address(id);
  var config_regs = PIC_config_regs(id);
  if offset < 128 then
    &config_regs([offset]) <- data
  else if offset < 1040 then
    nothing
  else if offset == 1040 then
    PIC_ip_bits(id)<63:0> <- PIC_ip_bits(id)<63:0> || data
  else if offset == 1041 then
    PIC_ip_bits(id)<127:64> <- PIC_ip_bits(id)<127:64> || data
  else if offset == 1056 then
    PIC_ip_bits(id)<63:0> <- PIC_ip_bits(id)<63:0> && ~data
  else if offset == 1057 then
    PIC_ip_bits(id)<127:64> <- PIC_ip_bits(id)<127:64> && ~data
  else
    nothing;
  PIC_config_regs(id) <- config_regs;
  PIC_update (id)
}
