//=====================================//
// Physical accesses in the VMSA case  //
// ====================================//

func PhysMemWrite{N}(
  desc:AddressDescriptor,
  accdesc:AccessDescriptor,
  value:bits(N*8)
) => PhysMemRetStatus
begin
// Event level access, they differ for standard (virtual) addresses
// and PTE's.
//   We cannot use conditional expresssion,
// because  write_memory_gen  needs a concrete argument.
  var eventaccess : EventAccess;
  if IsVirtual(desc.vaddress) then
    eventaccess = PHY;
  else
    eventaccess = PHY_PTE;
  end;
// Now, we can write, physically.
  write_memory_gen{N*8}(desc.paddress.address, value,accdesc,eventaccess);
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
  var eventaccess : EventAccess;
  if IsVirtual(desc.vaddress) then
    eventaccess = PHY;
  else
    eventaccess = PHY_PTE;
  end;
  let value =
    read_memory_gen{N*8}(desc.paddress.address,accdesc,eventaccess);
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
  return (ret_status, value);
end;
