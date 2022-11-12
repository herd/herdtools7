
enum fault_type_t {
  FaultMMUAddressSize = 0,
  FaultMMUTranslation,
  FaultMMUAccessFlag,
  FaultMMUPermission,
  FaultTagCheck,
  FaultUnsupported,
  NoFault,
  FaultTypes,
};

static const char *fault_type_names[] = {
  "MMU:AddressSize",
  "MMU:Translation",
  "MMU:AccessFlag",
  "MMU:Permission",
  "TagCheck",
  "Unsupported",
  "NoFault"
};

static enum fault_type_t get_fault_type(unsigned int esr)
{
  unsigned int dfsc = esr & 0x3fU;
  int fault_type = dfsc >> 2;
  if (esr == -1)
    return NoFault;

  if (fault_type >= FaultUnsupported)
    return FaultUnsupported;
  else
    return fault_type;
}
