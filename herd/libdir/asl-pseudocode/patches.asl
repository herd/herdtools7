


// GenMPAMatEL()
// =============
// Returns MPAMinfo for the specified EL.
// May be called if MPAM is not implemented (but in an version that supports
// MPAM), MPAM is disabled, or in AArch32.  In AArch32, convert the mode to
// EL if can and use that to drive MPAM information generation.  If mode
// cannot be converted, MPAM is not implemented, or MPAM is disabled return
// default MPAM information for the current security state.

// MPAMinfo GenMPAMatEL(AccessType acctype, bits(2) el)
func GenMPAMatEL(acctype:: AccessType, el::bits(2)) => MPAMinfo
begin
  return UNKNOWN :: MPAMinfo;
end

getter TSTATE[] => TMState
begin
  return UNKNOWN :: TMState;
end


// IsAligned

func IsAligned(x :: bits(N), y::integer) => boolean
begin
  return TRUE;
end

func IsAligned(x::integer, y::integer) => boolean
begin
  return TRUE;
end

// BigEndian

func BigEndian(acctype:: AccessType) => boolean
begin
  return FALSE;
end

func IsFault(addrdesc:: AddressDescriptor) => boolean
begin
  return FALSE;
end

func AArch64_TranslateAddress(address::bits(64), accdesc::AccessDescriptor, aligned::boolean, size::integer) => AddressDescriptor
begin
  return CreateAddressDescriptor(address, UNKNOWN :: FullAddress, NormalNCMemAttr());
end

func ELStateUsingAArch32K(el::bits(2), secure::boolean) => (boolean, boolean)
begin
    return (TRUE, FALSE);
end
