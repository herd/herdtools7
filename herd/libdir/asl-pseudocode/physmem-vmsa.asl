//=====================================//
// Physical accesses in the VMSA case  //
// ====================================//

func PhysMemWrite(
  desc::AddressDescriptor,
  size::integer,
  accdesc::AccessDescriptor,
  value::bits(8*size)
) => PhysMemRetStatus
begin
  write_memory_gen (desc.paddress.address, size*8, value,accdesc);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
end

// =============================================================================

func PhysMemRead(
  desc::AddressDescriptor,
  size::integer,
  accdesc::AccessDescriptor
) => (PhysMemRetStatus, bits(8*size))
begin
  let value =
    read_memory_gen (desc.paddress.address,size*8,accdesc)[8*size-1:0];
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
  return (ret_status, value);
end
