//=====================================//
// Physical accesses in the VMSA case  //
// ====================================//

func PhysMemWrite{N}(
  desc:AddressDescriptor,
  accdesc:AccessDescriptor,
  value:bits(N*8)
) => PhysMemRetStatus
begin
  write_memory_gen {N*8}(desc.paddress.address, value,accdesc);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
end;

// =============================================================================

func PhysMemRead{N}(
  desc:AddressDescriptor,
  accdesc:AccessDescriptor
) => (PhysMemRetStatus, bits(N*8))
begin
  let value =
    read_memory_gen {N*8}(desc.paddress.address,accdesc);
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
  return (ret_status, value);
end;