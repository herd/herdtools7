//==================================================//
// So called physical accesses in the standard case //
// =================================================//

func PhysMemWrite{N}(
  desc:AddressDescriptor,
  accdesc:AccessDescriptor,
  value:bits(N*8)
) => PhysMemRetStatus
begin
  write_memory_gen{N*8}(desc.vaddress,value,accdesc,VIR);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
end;

// =============================================================================

func PhysMemRead{N} (
  desc:AddressDescriptor,
  accdesc:AccessDescriptor
) => (PhysMemRetStatus, bits(N*8))
begin
  let value = read_memory_gen{N*8}(desc.vaddress,accdesc,VIR);
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros{64}
  };
  return (ret_status, value);
end;

// =============================================================================
// Exclusive access on based on virtual addresses
//

// =============================================================================

// AArch64.MarkExclusiveVA()
// =========================
// Optionally record an exclusive access to the virtual address region of size bytes
// starting at address for processorid.

var RESADDR : bits(64);

func AArch64_MarkExclusiveVA
(address : bits(64), processorid : integer, size : integer)
begin
  RESADDR = address;
end;

// =============================================================================

// AArch64.IsExclusiveVA()
// =======================
// An optional IMPLEMENTATION DEFINED test for an exclusive access to a virtual
// address region of size bytes starting at address.
//
// It is permitted (but not required) for this function to return FALSE and
// cause a store exclusive to fail if the virtual address region is not
// totally included within the region recorded by MarkExclusiveVA().
//
// It is always safe to return TRUE which will check the physical address only.

var SuccessVA : boolean = FALSE ;

func AArch64_IsExclusiveVA
(address : bits(64), processorid : integer, size : integer) => boolean
begin
  // Try both possibilities: write or not write
  SuccessVA = SomeBoolean();
   // Read RESADDR localy because we want a read event in all cases.
  let reserved = RESADDR;
  // If write succeeds then effective address and reservation coincide.
  if SuccessVA then CheckProp(address == reserved); end;
  return SuccessVA;
end;

// =============================================================================

// ExclusiveMonitorsStatus()
// =========================
// Returns '0' to indicate success if the last memory write by this PE was to
// the same physical address region endorsed by ExclusiveMonitorsPass().
// Returns '1' to indicate failure if address translation resulted in a different
// physical address.

func ExclusiveMonitorsStatus() => bit
begin
  return if SuccessVA then '0' else '1';
end;

// ===================================
// All other functions related to exclusive access are No-Op

func MarkExclusiveGlobal
  (paddress : FullAddress,
  processorid : integer,
  size : integer)
begin
  return;
end;

func IsExclusiveGlobal
   (paddress : FullAddress,
    processorid : integer,
    size : integer) => boolean
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