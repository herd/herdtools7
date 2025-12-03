/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    implementations.asl
    -------------------

This file is a list of implementations for use in herd of functions left non-
-implemented in the ARM Reference Manual. We copy the explanations from it.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

The first two type declarations have been extracted from the ARM Reference
manual with a regex search.
We suppose that they are enough for our experiments.

The rest of the file are hand-written implementations: they are mostly the
smallest AST that would type-check, but sometimes also call some logic relative
to herd primitives.


*/


// =============================================================================

type MAIRType of MAIR_EL1_Type;
type S2PIRType of S2PIR_EL2_Type;
type S1PIRType of S2PIRType;
type SCRType of SCR_Type;

// Our extractor for the system register types does not yet support those.
type BRBINF_EL1_Type of bits(64);
type BRBTGT_EL1_Type of bits(64);
type BRBSRC_EL1_Type of bits(64);

// =============================================================================

func _SetUpRegisters ()
begin
  // Value found on Rasberry 4B, ArmBian
  // uname -a:
  // Linux cheilly 5.4.0-1089-raspi #100-Ubuntu SMP PREEMPT Thu Jun 22 09:59:38 UTC 2023 aarch64 aarch64 aarch64 GNU/Linux
  _TCR_EL1 = '0000000000000000000000000000010011110101100100000111010100010000';

  _SCTLR_EL1 =
    // Bit number 2 -> cache enabled, the rest probably is inaccurate.
    // '0000000000000000000000000000000000000000000000000000000000000100'
    // Value found on Rasberry 4B, Ubuntu 20.04.2
    // uname -a:
    // Linux cheilly 5.4.0-1115-raspi #127-Ubuntu SMP PREEMPT Wed Aug 7 14:38:47 UTC 2024 aarch64 aarch64 aarch64 GNU/Linux
       '0000000000000000000000000000000000000000110001010001100000111101'
    // Another value from the same machine
    // '0000000000000000000000000000000000110000110100000001100110000101'
    ;
end;

// =============================================================================

var _SP_EL0: bits(64);

accessor SP_EL0() <=> v: bits(64)
begin
  getter
    return _SP_EL0;
  end;

  setter
    _SP_EL0 = v;
  end;
end;

// =============================================================================

func ConstrainUnpredictableBool(which:Unpredictable) => boolean
begin
  return ARBITRARY: boolean;
end;

// =============================================================================

// Not declared in shared_pseudocode

// We only implement the minimum necessary

readonly func ImpDefBool(s: string) => boolean
begin
  case s of
    when "Secure-only implementation" => return FALSE;
    otherwise =>
      println "Unknown ImpDef: " ++ s;
      assert FALSE;
  end;
end;

readonly func ImpDefInt(s: string) => boolean
begin
  case s of
  when "Maximum Physical Address Size" => return 48;
  otherwise =>
      println "Unknwon ImpDef: " ++ s;
      assert FALSE;
  end;
end;


// =============================================================================

// Not declared in shared_pseudoocode

// We only implement the mininum required features.

func IsFeatureImplemented(f : Feature) => boolean
begin
  case f of
    when FEAT_AA64EL0 => return TRUE;
    otherwise => return FALSE;
  end;
end;

// =============================================================================

// InstructionSynchronizationBarrier()
// ===================================
func InstructionSynchronizationBarrier()
begin
  primitive_isb();
end;

// =============================================================================

// DataMemoryBarrier()
// ===================

func DataMemoryBarrier(domain : MBReqDomain, types : MBReqTypes)
begin
  primitive_dmb(domain, types);
end;

// DataSynchronizationBarrier()
// ============================

// nXS is not implemented in herd

func DataSynchronizationBarrier
  (domain : MBReqDomain,
   types : MBReqTypes,
   nXS : boolean)
begin
  primitive_dsb(domain, types);
end;

// =============================================================================

// ThisInstrLength()
// =================

// In herd, instructions are always 32-bits long

func ThisInstrLength() => integer
begin
  return 32;
end;

// =============================================================================

// ExternalInvasiveDebugEnabled()
// ==============================
// The definition of this function is IMPLEMENTATION DEFINED.
// In the recommended interface, this function returns the state of the DBGEN signal.

// We do not support external debug.

func ExternalInvasiveDebugEnabled() => boolean
begin
    return FALSE;
end;

// =============================================================================

// ProcessorID()
// =============
// Return the ID of the currently executing PE.

// We override a impdef declaration in shared_pseudocode. The processor id is
// set directly by herd as an integer, in the variable _ProcesorID.

var _ProcessorID: integer = 0;

func ProcessorID() => integer
begin
  return _ProcessorID;
end;

// =============================================================================

// Code used by our interface with herd, in either `physmem-std.asl` or
// `physmem-vmsa.asl`

// Type of underlying accesses (same order as lib/access.mli),
// as recorder un events.

type EventAccess of enumeration {
     REG,
     VIR,
     PHY,
     PTE,
     TLB,
     TAG,
     PHY_PTE,
};
