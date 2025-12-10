/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    physmem-vmsa.asl
    ----------------

This file is a list of implementations of ASL functions from the ARM Reference
Manual. They are written by hand.

There are 2 main parts in this file: Physical memory accesses, and an
implementation of an exclusive monitor.

This file is included with the shared_pseudocode only when the stage 1
translation is activated.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

 */

// =============================================================================
// Physical accesses in the VMSA case
// =============================================================================

// Those are mainly calls to primitives, with correct return status

// We create a simple type to simplify signatures
type PhysMemSize of integer{8, 16, 32, 64, 128, 256, 512};

// Constructs the correct physical access type for herd7. They distinguish
// between standard virtual addresses and page table descriptors.
func _PhysEventAccess(vaddr: bits(64)) => EventAccess
begin
  // Note that we can't use a conditional expression because we need a concrete
  // value for the memory primitives
  if IsVirtual(vaddr) then
    return PHY;
  else
    return PHY_PTE;
  end;
end;

// Our implementation of PhysMemWrite creates a Physical Write Memory Effect
// with the correct annotation. It also has to take into account:
// - Ensuring that the double translation in case of store exclusive give the
//   same result (cf comment in the operational code of STXR saying 'This
//   atomic write will be rejected if it does not refer to the same physical
//   locations after address translation.')
// - Checking that the access is indeed a write. It is important for CAS, as a
//   non-deterministic choice has been made when creating the access
//   descriptor.

func PhysMemWrite{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor, value : bits(size))
  => PhysMemRetStatus
begin
  if accdesc.acctype == AccessType_GPR then
    // No write when CAS reduces to a read
    CheckProp(accdesc.write);

    // Event access for herd7
    let eventaccess = _PhysEventAccess(desc.vaddress);

    // In the case of exclusive monitors, we force the equality of the double
    // translation by checking that the physical address here is the same as the
    // one that was stored by the exclusive monitor.
    // In the Arm ARM, there is a comment in the operation code of STXR saying:
    //   This atomic write will be rejected if it does not refer to the same
    //   physical locations after address translation.
    // See: https://developer.arm.com/documentation/ddi0602/2025-09/Base-Instructions/STXR--Store-exclusive-register-?lang=en
    // This does nothing if it isn't in a store exclusive instruction.
    CheckExclusiveDuplicatedTranslate(desc.paddress, _ProcessorID, size);

    // Generate the Physical Write Memory Effects
    write_memory_gen{size}(desc.paddress.address, value,accdesc,eventaccess);

  elsif accdesc.acctype == AccessType_TTW then
   WritePtePrimitive{size}(desc.paddress.address, value, accdesc.write);

  else unreachable;
  end;

  return PhysMemRetStatus_NoFault;
end;

// =============================================================================

// Underlining storage for the value read in the case of hardware updates.
// This is reset by herd between 2 instructions.

var PteRead64 : bits(64) = Zeros{64};
var PteRead128 : bits(128) = Zeros{128};

// Our implementation of PhysMemRead creates a Physical Read Memory Effect
// with the correct annotation. It also has to take into account:
// Our implementation of PhysMemWrite has to take into account:
// - Treat the case of PTE reads differently from general memory reads, as
//   herd7
// - Ensuring that the double translation in case of load exclusive give the
//   same result

func PhysMemRead{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor)
  => (PhysMemRetStatus, bits(size))
begin
  if accdesc.acctype == AccessType_GPR then
    // Event access for herd7
    let eventaccess = _PhysEventAccess(desc.vaddress);

    // In the case of exclusive monitors, we force the equality of the double
    // translation by checking that the physical address here is the same as
    // the one that was stored by the exclusive monitor. This does nothing if
    // it isn't in a load exclusive instruction.
    CheckExclusiveDuplicatedTranslate(desc.paddress, _ProcessorID, size);

    // Generate the Physical Read Memory Effects
    let value = read_memory_gen{size}(desc.paddress.address,accdesc,eventaccess);

    return (PhysMemRetStatus_NoFault, value);

  elsif accdesc.acctype == AccessType_TTW then
    if accdesc.atomicop then
      let value = ReadPteAgainPrimitive{size}(desc.paddress.address, accdesc.write);

      // Does not type-check, and pteread128 tests do not pass until we store
      // size information with symbolic bitvectors
      let pte_read = if size == 128 then PteRead128 /* as bits(size) */
                                    else PteRead64 /* as bits(size) */;
      CheckEq(value, pte_read);

      return (PhysMemRetStatus_NoFault, pte_read);

    else
      let value = ReadPtePrimitive{size}(desc.paddress.address);

      if size == 128 then
        PteRead128 = value /* as bits(128) */;
      else
        assert size == 64;
        PteRead64 = value /* as bits(64) */;
      end;

      return (PhysMemRetStatus_NoFault, value);
    end;
  else unreachable;
  end;
end;

// =============================================================================
// Exclusive access on based on physical addresses
// =============================================================================

// We simulate the exclusives "global" monitor

// RESADDR is our backing storage for the exclusive monitor.
// It is stored between different instructions by herd, but it is
// thread-specific storage (i.e. it is implemented as a register).
// It is set by MarkExclusiveVA, and read and reset by IsExclusiveVA.
// After an exclusive store instruction, it will contain the address of the
// exclusive store, as set by MarkExclusiveVA.

// During an exclusive load instruction, IsExclusiveVA reads the value stored
// in RESADDR and compare it to the address of the current load. If the values
// are different, IsExclusiveVA returns FALSE, otherwise it can return TRUE.
// IsExclusiveVA also stores its return value into SuccessVA.

var RESADDR : bits(56) = Zeros{56};

// _RegisteredAddress is a backing storage in an instruction, to check that the
// two translations that are performed for exclusive instructions. It is not
// stored between different instructions, and is reset to zero at the begining
// of each instruction.

var _RegisteredAddress: bits(56);

// CheckRegisteredAddress is a flag to signal to PhysMemRead/PhysMemWrite to
// check that the address that is passed is equal to the one stored in
// _RegisteredAddress.

var _CheckRegisteredAddress: boolean = FALSE;

// SuccessPA is an intra-instruction storage for the status of the exclusive
// monitor. It is not stored between different instructions, and is reset to
// FALSE at the begining of each instruction.

var SuccessPA : boolean;

// RegisterAddress() is an accessor that sets both _RegisteredAddress and
// _CheckRegisteredAddress. It is called to ensure that the two translations
// for exclusive loads and stores return the same value.

func RegisterAddress(address:bits(56))
begin
  _RegisteredAddress = address;
  _CheckRegisteredAddress = TRUE;
end;

// CheckExclusiveDuplicatedTranslate is the function that is called by
// PhysMemRead and PhysMemWrite to check that the second translation returns
// the same physical address than the first translation for the exclusive
// monitors.
// If a previous translation in the exclusive monitor has happened previously
// in the same instruction, it ensures that the address passed as argument in
// paddress is the same as the one in _RegisteredAddress. Otherwise, it does
// nothing.
// Unsupported now is using the length.
// Because RESADDR is local to a single processor, we do not use processorid.

func CheckExclusiveDuplicatedTranslate(paddress : FullAddress, processorid: integer, size: integer)
begin
  if _CheckRegisteredAddress then
    CheckProp(_RegisteredAddress == paddress.address);
  end;
end;

// =============================================================================

// MarkExclusiveGlobal()
// =====================
// Record the physical address region of size bytes starting at paddress in
// the global Exclusives monitor for processorid.

// Our implementation simply updates the backing storage to store the address
// passed as argument.

func MarkExclusiveGlobal
  (paddress : FullAddress, processorid : integer, size : integer)
begin
  RESADDR = paddress.address;
  RegisterAddress(paddress.address);
end;

// =============================================================================

// IsExclusiveGlobal()
// ===================
// Return TRUE if the global Exclusives monitor for processorid includes all of
// the physical address region of size bytes starting at paddress.

func IsExclusiveGlobal (paddress : FullAddress, processorid : integer, size : integer) => boolean
begin
  let reserved = RESADDR;
  RESADDR=Zeros{56};

  SuccessPA = SomeBoolean();

  if SuccessPA then
    CheckEq(paddress.address, reserved);
    RegisterAddress(paddress.address);
  end;

  return SuccessPA;
end;

// =============================================================================

// ExclusiveMonitorsStatus()
// =========================
// Returns '0' to indicate success if the last memory write by this PE was to
// the same physical address region endorsed by ExclusiveMonitorsPass().
// Returns '1' to indicate failure if address translation resulted in a different
// physical address.

// Return the same result than IsExclusiveVA, as stored in SuccessVA.

func ExclusiveMonitorsStatus() => bit
begin
  return if SuccessPA then '0' else '1';
end;


