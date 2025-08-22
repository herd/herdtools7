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
  CheckExclusiveDuplicatedTranslate(desc.paddress, ProcessorID(), N);
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
  CheckExclusiveDuplicatedTranslate(desc.paddress, ProcessorID(), N);
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


// ==============================
// The remaining "Exclusive" functions are no-op's


func AArch64_MarkExclusiveVA
(address : bits(64), processorid : integer, size : integer)
begin
  pass;
end;

func AArch64_IsExclusiveVA
(address : bits(64), processorid : integer, size : integer) => boolean
begin
  return TRUE;
end;

func MarkExclusiveLocal
  (paddress : FullAddress,
  processorid : integer,
  size : integer)
begin
  return;
end;

func IsExclusiveLocal
   (paddress : FullAddress,
    processorid : integer,
    size : integer) => boolean
begin
  return TRUE;
end;

func ClearExclusiveLocal(processorid : integer)
begin
  pass;
end;
