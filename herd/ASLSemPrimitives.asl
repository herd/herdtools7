/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*
    ASLSemPrimitives.asl
    --------------------

This file contains a description of the OCaml function that can be called by
the ASL environment. They are implemented in `ASLSem.ml`.

*/

// Barriers
// --------

// primitive_isb() generates an ISB effect.

// It is called by the function InstructionSynchronizationBarrier(), that is
// implementation-defined in the Arm ARM, and implemented by hand in
// `implementations.asl`.

func primitive_isb()
begin pass; end;

// primitive_dmb() generates a DMB effect, parametrised by d which represents
// the domain and t which represents the type of the data memory barrier.

// It is called by the function DataMemoryBarrier(), which is
// implementation-defined in the Arm ARM, and implemented by hand in
// `implementations.asl`.

func primitive_dmb(d: MBReqDomain, t: MBReqTypes)
begin pass; end;


// primitive_dsb() generates a DSB effect, parametrised by d which represents
// the domain and t which represents the type of the data synchronisation
// barrier.

// It is called by the function DataSynchronisationBarrier(), which is
// implementation-defined in the Arm ARM, and implemented by hand in
// `implementations.asl`.

func primitive_dsb(d: MBReqDomain, t: MBReqTypes)
begin pass; end;

// -----------------------------------------------------------------------------

// General purpose register accesses
// ---------------------------------

// Those functions are called by our re-implementations to the X[] accessor

// They are needed because the current implementation in the Arm ARM introduces
// synchronisation between the different general purpose registers by putting
// them in an array.

// read_register() returns the value in the register numbered by the argument.
// The argument is supposed to be between 0 and 30 (included).

func read_register(reg: integer) => bits(64)
begin return ARBITRARY: bits(64); end;

// write_register() writes the value passed as argument in the correct
// register. The register number is supposed to be between 0 and 30.

func write_register(reg: integer, data: bits(64))
begin pass; end;

// -----------------------------------------------------------------------------

// Memory accesses
// ---------------

// read_memory_gen() is called by our implementation of PhysMemRead() in
// physmem-{std,vmsa}.asl

// It create a Memory Read Effect, with the annotation inferred by the settings
// in accdesc and access. The value returned is a symbolic value, i.e. it can
// for now be any N-bits bitvector. This value will be resolved later by herd,
// in a phase where it matches it with all possible writes.

// TODO describe the translation between access, accdesc and annotation

func read_memory_gen{N}(addr: bits(64), accdesc: AccessDescriptor, access: EventAccess) => bits(N)
begin return ARBITRARY: bits(N); end;

// write_memory_gen() is called by our implementation of PhysMemWrite() in
// physmem-{std,vmsa}.asl

// It creates a Memory Write Effect, with the annotation inferred by the
// settings in accdesc and access.

func write_memory_gen{N}(addr: bits(56), data: bits(N), accdesc: AccessDescriptor, access: EventAccess)
begin pass; end;

// With VMSA activated

// ReadPtePrimitive() is called by our implementation of PhysMemRead() in
// physmem-vmsa.asl. It creates an Implicit Memory Read Effect, without an
// atomic annotation. It is only called in the case of an implicit TTD read,
// i.e. when the acctype field of accdesc is set to AccessType_TTW. It is only
// called for the first TTD read of an address translation, that is the
// non-atomic one.

func ReadPtePrimitive{N}(addr: bits(56)) => bits(N)
begin return ARBITRARY: bits(N); end;

// ReadPteAgainPrimitive() is called by our re-implementation of the function
// AArch64.MemSwapTableDesc() in patches-vmsa.asl. It performs an atomic
// version of ReadPtePrimitive(), i.e. it creates an Implicit Memory Read
// effect, with an atomic annotation. As MemSwapTableDesc(), it is called when
// the translation system finds out that the page table descriptor for an
// address needs to be updated, in our case only when the AF or nDB bits need
// to be changed in memory.

func ReadPteAgainPrimitive{N}(addr: bits(56), is_write: boolean) => bits(N)
begin pass; end;

// WritePtePrimitive() is the counterpart of the ReadPteAgainPrimitive(). It is
// called by AArch64.MemSwapTableDesc() in patches-vmsa.asl, and is to write
// back the updated page table descriptor in memory. It creates an Implicit
// Memory Write Effect, with an atomic annotation.

func WritePtePrimitive{N}(addr: bits(56), data: bits(N), is_write: boolean)
begin pass; end;

// Not used by AArch64, simplifications for the `pseudo-arch` mode.

func read_memory{N}(addr: bits(64)) => bits(N)
begin return ARBITRARY: bits(N); end;

func write_memory{N}(addr: bits(64), data: bits(N))
begin pass; end;


// -----------------------------------------------------------------------------


// Symbolic addresses handling
// ---------------------------

// ComputePtePrimitive() is used by our re-implementation of the function
// AArch64.S1SLTTEntryAddress() in patches-vmsa.asl. It computes the address of
// the page table descriptor of the address passed as argument. Because addresses in herd are
// symbols, this often simply looks like `pte(x)` for an address `x`.

func ComputePtePrimitive(addr: bits(64)) => bits(56)
begin return ARBITRARY: bits(56); end;

// GetOAPrimitive() is used by our re-implementation of AArch64.S1LeafBase() in
// patches-vmsa.asl. For a page table descriptor of a virtual address `x`, this
// will return the physical address associated with `x`. Because addresses in
// herd are symbols, this often simply looks like `oa(x)`.

func GetOAPrimitive{N}(addr: bits(N)) => bits(56)
begin return ARBITRARY: bits(56); end;

// OffsetPrimitive() is used by our re-implementations of StageOA() in
// patches-vmsa.asl. For a virtual address `x`, this returns the offset to the
// start of the block. For example, for a virtual address `y+8`, this would
// return `8`, because symbolic addresses in herd are aligned with the start of
// the block.

func OffsetPrimitive(addr: bits(64)) => integer
begin return ARBITRARY: integer; end;

// IsVirtual is used by our implementation of PhysMemRead() and PhysMemWrite()
// in order to distinguish between physical and virtual accesses. It is based
// on herd's representation of addresses as symbols.

readonly func IsVirtual(addr: bits(64)) => boolean
begin return ARBITRARY: boolean; end;


// Translation configuration
// -------------------------

readonly func GetHaPrimitive() => bits(1)
begin return ARBITRARY: bits(1); end;

readonly func GetHdPrimitive() => bits(1)
begin return ARBITRARY: bits(1); end;

// Fault handling
// --------------

// DataAbortPrimitive() is called by our re-implementation of DataAbort(). It
// creates a Fault Effect from the arguments. It does not have any implication
// on the ASL control-flow, although it will instruct herd to handle that fault
// at the end of the ASL execution of the current instruction.

func DataAbortPrimitive(addr: bits(64), write: boolean, statuscode: Fault, accdesc: AccessDescriptor)
begin pass; end;

// Arithmetic speedups
// -------------------

// Because we have symbolic bitvector, we have to specialise those conversion
// functions, otherwise the implementation in stdlib.asl would introduce a
// combinatorial explosion equivalent to bit-blasting a N-bit bitvector.

pure func UInt{N}(x: bits(N)) => integer {0..(2^N)-1}
begin return 0; end;

pure func SInt{N} (x: bits(N))
  => integer{(if N == 0 then 0 else -(2^(N-1))) .. (if N == 0 then 0 else (2^(N-1))-1)}
begin return 0; end;


// Symbolic Interpreter control
// ----------------------------

// CheckProp() add the assumption that prop is TRUE. This will silently discard
// all executions where the argument is FALSE.

func CheckProp(prop: boolean)
begin pass; end;

// CheckEq(a, b) is a more performant shortcut for `CheckProp(a == b);`

func CheckEq{N}(d1: bits(N), d2: bits(N))
begin pass; end;
