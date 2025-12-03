//=====================================//
// Physical accesses in the VMSA case  //
// ====================================//

type PhysMemSize of integer{8, 16, 32, 64, 128, 256, 512};

func PhysMemWrite{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor, value : bits(size))
  => PhysMemRetStatus
begin
// No write when CAS reduces to a read
  CheckProp(accdesc.write);
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
  CheckExclusiveDuplicatedTranslate(desc.paddress, ProcessorID(), size);
// Now, we can write, physically.
  write_memory_gen{size}(desc.paddress.address, value,accdesc,eventaccess);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
end;

// =============================================================================

func PhysMemRead{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor)
  => (PhysMemRetStatus, bits(size))
begin
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };

  if accdesc.acctype == AccessType_GPR then
    var eventaccess : EventAccess;
    if IsVirtual(desc.vaddress) then
      eventaccess = PHY;
    else
      eventaccess = PHY_PTE;
    end;
    CheckExclusiveDuplicatedTranslate(desc.paddress, ProcessorID(), size);
    let value = read_memory_gen{size}(desc.paddress.address,accdesc,eventaccess);
    return (ret_status, value);

  elsif accdesc.acctype == AccessType_TTW then

    let value = ReadPtePrimitive{size}(desc.paddress.address);
    return (ret_status, value);

  else unreachable;
  end;
end;

// =========================================
// We simulate the exclusives "global" monitor

var RESADDR : bits(56) = Zeros{56};
var _RegisteredAddress: bits(56);
var _CheckRegisteredAddress: boolean = FALSE;
var _SuccessPA : boolean;

func DoRegisterAddress(address:bits(56))
begin
  _RegisteredAddress = address;
  _CheckRegisteredAddress = TRUE;
end;


func DoCheckRegisteredAddress(address:bits(56))
begin
  let registered = _RegisteredAddress;
  CheckProp(registered == address);
end;

// MarkExclusiveGlobal()
// =====================
// Record the physical address region of size bytes starting at paddress in
// the global Exclusives monitor for processorid.

func MarkExclusiveGlobal
  (paddress : FullAddress,
  processorid : integer,
  size : integer)
begin
  let paddr = paddress.address;
  RESADDR = paddr;
  DoRegisterAddress(paddr);
//  __debug__(RESADDR,paddr);
end;


// IsExclusiveGlobal()
// ===================
// Return TRUE if the global Exclusives monitor for processorid includes all of
// the physical address region of size bytes starting at paddress.


func IsExclusiveGlobal (paddress : FullAddress, processorid : integer, size : integer) => boolean
begin
  _SuccessPA = SomeBoolean();
//  __debug__(_SuccessPA);

  if _SuccessPA then
    let reserved = RESADDR;
    let paddr = paddress.address;
    let cond_exclusive_global = paddr == reserved;
//    __debug__(paddr,reserved,cond_exclusive_global);
    CheckProp(cond_exclusive_global);
    DoRegisterAddress(paddr);
  end;
  RESADDR=Zeros{56};
  return _SuccessPA;
end;

func ExclusiveMonitorsStatus() => bit
begin
//  __debug__(_SuccessPA);
  return if _SuccessPA then '0' else '1';
end;


func CheckExclusiveDuplicatedTranslate(paddress : FullAddress, processorid: integer, size: integer)
begin
  // __debug__(_CheckRegisteredAddress);
  if _CheckRegisteredAddress then
     DoCheckRegisteredAddress(paddress.address);
  end;
end;

