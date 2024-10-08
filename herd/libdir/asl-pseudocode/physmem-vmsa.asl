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
// Event level access, there differ for standard (virtual) addresses
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
  write_memory_gen (desc.paddress.address, size*8, value,accdesc,eventaccess);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
end;

// =============================================================================

func PhysMemRead(
  desc::AddressDescriptor,
  size::integer,
  accdesc::AccessDescriptor
) => (PhysMemRetStatus, bits(8*size))
begin
  var eventaccess : EventAccess;
  if IsVirtual(desc.vaddress) then
    eventaccess = PHY;
  else
    eventaccess = PHY_PTE;
  end;
  let value =
    read_memory_gen
      (desc.paddress.address,size*8,accdesc,eventaccess)[8*size-1:0];
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
  return (ret_status, value);
end;
