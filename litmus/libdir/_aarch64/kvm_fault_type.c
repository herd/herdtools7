enum fault_type_t {
  NoFault,
  FaultUndefinedInstruction,
  FaultMMUAddressSize,
  FaultMMUTranslation,
  FaultMMUAccessFlag,
  FaultMMUPermission,
  FaultTagCheck,
  FaultUnsupported,
  FaultTypes,
};

static const char *fault_type_names[] = {
  "NoFault",
  "UndefinedInstruction",
  "MMU:AddressSize",
  "MMU:Translation",
  "MMU:AccessFlag",
  "MMU:Permission",
  "TagCheck",
  "Unsupported",
};

static enum fault_type_t get_fault_type(unsigned int esr)
{
  unsigned int ec;
  unsigned int dfsc;
  int fault_type;

  if (esr == -1)
    return NoFault;

  ec = esr >> ESR_EL1_EC_SHIFT;
  if (ec == ESR_EL1_EC_UNKNOWN) {
    return FaultUndefinedInstruction;
  } else {
    dfsc = esr & 0x3fU;
    fault_type = (dfsc >> 2) + FaultMMUAddressSize;
    if (fault_type >= FaultUnsupported)
      return FaultUnsupported;
    else
      return fault_type;
  }
}
