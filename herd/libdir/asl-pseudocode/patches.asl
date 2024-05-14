/*
 * SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
 * SPDX-License-Identifier: BSD-3-Clause
 */

/*

    patches.asl
    -----------

This file is a list of re-implementations of ASL functions from the ARM
Reference Manual. They are completely re-written or simply edited by hand. When
re-written completely, this is often time the minimal code that type-checks.
The code is also translated from ASLv0 to ASLv1 by hand.

The ARM Reference Manual is available here:
    https://developer.arm.com/documentation/ddi0602/2023-09/

*/


// =============================================================================

// GenMPAMatEL()
// =============
// Returns MPAMinfo for the specified EL.
// May be called if MPAM is not implemented (but in an version that supports
// MPAM), MPAM is disabled, or in AArch32.  In AArch32, convert the mode to
// EL if can and use that to drive MPAM information generation.  If mode
// cannot be converted, MPAM is not implemented, or MPAM is disabled return
// default MPAM information for the current security state.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-mpam?lang=en#impl-shared.GenMPAMatEL.2
// The whole logic is too complex for our simple use, so we return the base value of the return type.

// MPAMinfo GenMPAMatEL(AccessType acctype, bits(2) el)
func GenMPAMatEL(acctype: AccessType, el:bits(2)) => MPAMinfo
begin
  var x : MPAMinfo;
  return x;
end;

// =============================================================================

// IsAligned()
// ===========

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-common?lang=en#impl-shared.IsAligned.2
// We disable alignment checks.

func IsAligned{N}(x : bits(N), y:integer) => boolean
begin
  return TRUE;
end;

func IsAligned(x:integer, y:integer) => boolean
begin
  return TRUE;
end;

// =============================================================================

// BigEndian()
// ===========

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-memory?lang=en#impl-shared.BigEndian.1
// We only use small-endian

func BigEndian(acctype: AccessType) => boolean
begin
  return FALSE;
end;

// =============================================================================

// AArch64.TranslateAddress()
// ==========================
// Main entry point for translating an address

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/aarch64-translation-vmsa-translation?lang=en#AArch64.TranslateAddress.4
// We disable address translation

func AArch64_TranslateAddress(address:bits(64), accdesc:AccessDescriptor, aligned:boolean, size:integer) => AddressDescriptor
begin
  var full_addr : FullAddress;
  return CreateAddressDescriptor(address, full_addr, NormalNCMemAttr());
end;

// =============================================================================

// ELStateUsingAArch32K()
// ======================
// Returns (known, aarch32):
//   'known'   is FALSE for EL0 if the current Exception level is not EL0 and EL1 is
//             using AArch64, since it cannot determine the state of EL0; TRUE otherwise.
//   'aarch32' is TRUE if the specified Exception level is using AArch32; FALSE otherwise.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-system?lang=en#impl-shared.ELStateUsingAArch32K.2
// We are always on AArch64

func ELStateUsingAArch32K(el:bits(2), secure:boolean) => (boolean, boolean)
begin
    return (TRUE, FALSE);
end;

// =============================================================================

// ProcState
// =========
// Armv8 processor state bits.
// There is no significance to the field order.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-system?lang=en#ProcState
// Rewritten from record to bitfields

type ProcState of bits(64) {
    [3] N,        // Negative condition flag
    [2] Z,        // Zero condition flag
    [1] C,        // Carry condition flag
    [0] V,        // Overflow condition flag
    [4] D,        // Debug mask bit                     [AArch64 only]
    [5] A,        // SError interrupt mask bit
    [6] I,        // IRQ mask bit
    [7] F,        // FIQ mask bit
    [8] EXLOCK,   // Lock exception return state
    [9] PAN,      // Privileged Access Never Bit        [v8.1]
    [10] UAO,      // User Access Override               [v8.2]
    [11] DIT,      // Data Independent Timing            [v8.4]
    [12] TCO,      // Tag Check Override                 [v8.5, AArch64 only]
    [13] PM,       // PMU exception Mask
    [14] PPEND,     // synchronous PMU exception to be_observed
    [16:15] BTYPE,    // Branch Type                        [v8.5]
    [17] ZA,       // Accumulation array enabled         [SME]
    [18] SM,       // Streaming SVE mode enabled         [SME]
    [19] ALLINT,   // Interrupt mask bit
    [20] SS,       // Software step bit
    [21] IL,       // Illegal Execution state bit
    [23:22] EL,       // Exception level
    [24] nRW,      // Execution state: 0=AArch64, 1=AArch32
    [25] SP,       // Stack pointer select: 0=SP0, 1=SPx [AArch64 only]
    [26] Q,        // Cumulative saturation flag         [AArch32 only]
    [30:27] GE,       // Greater than or Equal flags        [AArch32 only]
    [31] SSBS,     // Speculative Store Bypass Safe
    [39:32] IT,       // If-then bits, RES0 in CPSR         [AArch32 only]
    [40] J,        // J bit, RES0                        [AArch32 only, RES0 in SPSR and CPSR]
    [41] T,        // T32 bit, RES0 in CPSR              [AArch32 only]
    [42] E,        // Endianness bit                     [AArch32 only]
    [47:42] M         // Mode field                         [AArch32 only]
};

// =============================================================================

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-system?lang=en#PSTATE
// Not really modified

var PSTATE : ProcState;

// =============================================================================

// GenerateAddress()
// =================
// Generate and address by adding a pointer with an offset and returning the result.
// If FEAT_CPA2 is implemented, the pointer arithmetic is checked.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-system?lang=en#impl-shared.GenerateAddress.3
// We don't want the checked pointer arithmetic.
// LUC simplify because failure of slice operatin on symbolic address.

func GenerateAddress(base:bits(64), offset:bits(64), accdesc:AccessDescriptor) => bits(64)
begin
  return base + offset;
end;

// =============================================================================

// AArch64.BranchAddr()
// ====================
// Return the virtual address with tag bits removed.
// This is typically used when the address will be stored to the program counter.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-memory?lang=en
// We don't do that here, we want to keep the address "as is".

func AArch64_BranchAddr
  (vaddress:bits(64), el:bits(2)) => bits(64)
begin
  return vaddress;
end;

// =============================================================================

// BranchNotTaken()
// ================
// Called when a branch is not taken.
// Patched to add PC self assignment

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-trace-tracebranch?lang=en
// We add the increment to _PC

func BranchNotTaken(branchtype:BranchType, branch_conditional:boolean)
begin
    _PC() = _PC()+4;
   let branchtaken = FALSE;
   if IsFeatureImplemented(FEAT_SPE) then
     SPEBranch{64}
       (ARBITRARY:bits(64), branchtype, branch_conditional, branchtaken);
    end;
    return;
end;

// =============================================================================

// UsingAArch32()
// ==============
// Return TRUE if the current Exception level is using AArch32, FALSE if using AArch64.
// Let us return FALSE, called by BranchTo(...) for checking tgt address size.

func UsingAArch32() => boolean
begin
  return FALSE;
end;
