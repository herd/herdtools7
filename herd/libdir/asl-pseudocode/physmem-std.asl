/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    physmem-std.asl
    ---------------

This file is a list of implementations of ASL functions from the ARM Reference
Manual. They are written by hand.

There are 2 main parts in this file: Physical memory accesses, and an
implementation of an exclusive monitor.

This file is included with the shared_pseudocode only when the stage 1
translation is *not* activated.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

 */

// =============================================================================
// So called physical accesses in the standard case
// =============================================================================

// Those are mainly calls to primitives, with correct return status

// We create a simple type to simplify signatures
type PhysMemSize of integer{8, 16, 32, 64, 128, 256, 512};

func PhysMemWrite{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor, value : bits(size))
  => PhysMemRetStatus
begin
  write_memory_gen{size}(desc.vaddress,value,accdesc,VIR);

  return PhysMemRetStatus_NoFault;
end;

// =============================================================================

func PhysMemRead{size : PhysMemSize}
  (desc : AddressDescriptor, accdesc : AccessDescriptor)
  => (PhysMemRetStatus, bits(size))
begin
  let value = read_memory_gen{size}(desc.vaddress,accdesc,VIR);

  return (PhysMemRetStatus_NoFault, value);
end;


// =============================================================================
// Exclusive access on based on virtual addresses
// =============================================================================

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

var RESADDR : bits(64);

// SuccessVA is an intra-instruction storage for the status of the exclusive
// monitor. It is not stored between different instructions, and is reset to
// FALSE at the begining of each instruction.

var SuccessVA : boolean = FALSE ;

// =============================================================================

// AArch64.MarkExclusiveVA()
// =========================
// Optionally record an exclusive access to the virtual address region of size bytes
// starting at address for processorid.

// Our implementation simply updates the backing storage to store the address
// passed as argument.

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

// Our implementation functionally does the following:
//  - reads the backing storage RESADDR into local variable `reserved`.
//  - resets RESADDR to zero
//  - sets SuccessVA to the boolean value representing whether address in
//    argument is equal to the one in reserved and some
//    constraint-unpredictable boolean.
//  - returns SuccessVA

func AArch64_IsExclusiveVA
(address : bits(64), processorid : integer, size : integer) => boolean
begin
  // Read RESADDR localy because we want a read event in all cases.
  let reserved = RESADDR;
  RESADDR = Zeros{64};

  SuccessVA = SomeBoolean();

  if SuccessVA then
    CheckEq(address, reserved);
  end;

  return SuccessVA;
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
  return if SuccessVA then '0' else '1';
end;

