/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    implementations0.asl
    -------------------

This file contains two ASL0 implementations which cannot be found in the ARM
Reference Manual, but are required for use in herd. In particular, they
interface with herd primitives. They cannot be implemented in ASL1, as ASL1 now
forbids getters/setters without any arguments.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

*/

// =============================================================================

bits(64) _PC
  return read_pc();

_PC = bits(64) value
  write_pc(value);
  return;

// =============================================================================

PhysMemRetStatus PhysMemWrite(AddressDescriptor desc, integer size, AccessDescriptor accdesc,
                              bits(8*size) value)
  write_memory_gen(desc.vaddress, size*8, value,accdesc);
  PhysMemRetStatus res;
  res.statuscode = Fault_None;
  res.extflag = '0';
  res.merrorstate = ErrorState_CE;  // ??
  res.store64bstatus = Zeros(64);
  return res;

// =============================================================================

(PhysMemRetStatus, bits(8*size)) PhysMemRead(AddressDescriptor desc, integer size,
                                             AccessDescriptor accdesc)
  value = read_memory_gen(desc.vaddress,size*8,accdesc)[8*size-1:0];
  PhysMemRetStatus ret_status;
  ret_status.statuscode = Fault_None;
  ret_status.extflag = '0';
  ret_status.merrorstate = ErrorState_CE;  // ??
  ret_status.store64bstatus = Zeros(64);
  return (ret_status, value);

// =============================================================================
// AltDecodeBitMasks()
// ===================
// Alternative but logically equivalent implementation of DecodeBitMasks() that
// uses simpler primitives to compute tmask and wmask.
// Luc: Overridden for avoiding a warning, not called anyway

(bits(M), bits(M)) AltDecodeBitMasks(bit immN, bits(6) imms, bits(6) immr,
                                     boolean immediate, integer M)
  assert FALSE;
  return (Zeros(M), Zeros(M));

