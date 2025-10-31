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

var SP_EL0: bits(64);

// =============================================================================

func ConstrainUnpredictableBool(which:Unpredictable) => boolean
begin
  return ARBITRARY: boolean;
end;

// =============================================================================

func IsFeatureImplemented(f : Feature) => boolean
begin
  return FALSE;
end;

// =============================================================================

func HaveAArch32() => boolean
begin
  return FALSE;
end;

// =============================================================================

func HaveAArch64() => boolean
begin
  return TRUE;
end;

// =============================================================================

// HaveEL()
// ========
// Return TRUE if Exception level 'el' is supported

func HaveEL(el: bits(2)) => boolean
begin
    if el IN {EL1,EL0} then
        return TRUE;                             // EL1 and EL0 must exist
    else
        return FALSE; // boolean IMPLEMENTATION_DEFINED;
    end;
end;

// =============================================================================

// ClearExclusiveByAddress()
// =========================
// Clear the global Exclusives monitors for all PEs EXCEPT processorid if they
// record any part of the physical address region of size bytes starting at paddress.
// It is IMPLEMENTATION DEFINED whether the global Exclusives monitor for processorid
// is also cleared if it records any part of the address region.

func ClearExclusiveByAddress(paddress : FullAddress, processorid : integer, size : integer)
begin
  pass;
end;

// =============================================================================

accessor _R (n : integer) <=> value: bits(64)
begin
  getter
    return read_register(n);
  end;

  setter
    write_register(n, value);
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

// We use our own integer codings of enumerations
// to guard against enumeration type change

func MBReqDomainToInteger(domain : MBReqDomain) => integer
begin
  case domain of
    when MBReqDomain_Nonshareable => return 0;
    when MBReqDomain_InnerShareable => return 1;
    when MBReqDomain_OuterShareable => return 2;
    when MBReqDomain_FullSystem => return 3;
  end;
end;

func MBReqTypesToInteger(types : MBReqTypes) => integer
begin
  case types of
    when MBReqTypes_Reads => return 0;
    when MBReqTypes_Writes => return 1;
    when MBReqTypes_All => return 2;
  end;
end;

func DataMemoryBarrier(domain : MBReqDomain, types : MBReqTypes)
begin
  primitive_dmb(MBReqDomainToInteger(domain),MBReqTypesToInteger(types));
end;

// DataSynchronizationBarrier()
// ============================

func DataSynchronizationBarrier
  (domain : MBReqDomain,
   types : MBReqTypes,
   nXS : boolean)
begin
  primitive_dsb(MBReqDomainToInteger(domain),MBReqTypesToInteger(types));
end;

// =============================================================================

// Hint_Branch()
// =============
// Report the hint passed to BranchTo() and BranchToAddr(), for consideration when processing
// the next instruction.

func Hint_Branch(hint : BranchType)
begin
  return;
end;

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
