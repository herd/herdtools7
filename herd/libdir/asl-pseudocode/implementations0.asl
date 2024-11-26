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
  return PhysMemWriteV1{size}(desc,accdesc,value);

// =============================================================================

(PhysMemRetStatus, bits(8*size)) PhysMemRead(AddressDescriptor desc, integer size,
                                             AccessDescriptor accdesc)
    (ret_status,value) = PhysMemReadV1{size}(desc,accdesc);
    return (ret_status,value);
