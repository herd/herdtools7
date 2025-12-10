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

// GenMPAMAtEL()
// =============
// Returns MPAMinfo for the specified EL.
// May be called if MPAM is not implemented (but in an version that supports
// MPAM), MPAM is disabled, or in AArch32.  In AArch32, convert the mode to
// EL if can and use that to drive MPAM information generation.  If mode
// cannot be converted, MPAM is not implemented, or MPAM is disabled return
// default MPAM information for the current security state.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-mpam?lang=en#impl-shared.GenMPAMatEL.2
// The whole logic is too complex for our simple use, so we return the base value of the return type.

func GenMPAMAtEL(acctype: AccessType, el: bits(2)) => MPAMinfo
begin
  var x : MPAMinfo;
  return x;
end;

// =============================================================================

// CheckSPAlignment()
// ==================
// Check correct stack pointer alignment for AArch64 state.

// By construction in herd7 this is always the case.

func CheckSPAlignment()
begin
  return;
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

// GenerateAddress()
// =================
// Generate and address by adding a pointer with an offset and returning the result.
// If FEAT_CPA2 is implemented, the pointer arithmetic is checked.

// From https://developer.arm.com/documentation/ddi0602/2023-09/Shared-Pseudocode/shared-functions-system?lang=en#impl-shared.GenerateAddress.3
// We don't want the checked pointer arithmetic.
// LUC simplify because failure of slice operatin on symbolic address.

func AddressAdd(base:bits(64), offset:bits(64), accdesc:AccessDescriptor) => bits(64)
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
    _PC = _PC+4;
   let branchtaken = FALSE;
   if IsFeatureImplemented(FEAT_SPE) then
     SPEBranch{64}
       (ARBITRARY:bits(64), branchtype, branch_conditional, branchtaken);
    end;
    return;
end;

// =============================================================================

// CheckOriginalSVEEnabled()
// =========================
// Checks for traps on SVE instructions and instructions that access SVE System
// registers.
// LUC, just allow

func CheckOriginalSVEEnabled()
begin
  return;
end;

// =============================================================================

// X - accessor
// ============

// We override the original declaration in shared_pseudocode to substitute the
// accesses to a backing array to calls to our read_register and write_register
// primitives.
// This does not make any difference functionally but avoid the synchronisation
// between the different register accesses: with a backing array, a read to
// X[4] would be asl-data after a previous write to X[5], where our primitive
// guarantee independence of those registers.

accessor X{width}(n : integer) <=> value : bits(width)
begin
    // Write a 32-bit or 64-bit value to a general-purpose register.
    setter
        assert n >= 0 && n <= 31;
        assert width IN {32,64};
        if n != 31 then
            write_register(n, ZeroExtend{64}(value));
        end;
        return;
    end;

    // Read the least-significant 8, 16, 32, or 64 bits from a general-purpose register.
    getter
        assert n >= 0 && n <= 31;
        let rw : integer{} = width as integer{8, 16, 32, 64};
        if n != 31 then
            return read_register(n)[rw-1:0];
        else
            return Zeros{rw};
        end;
    end;
end;

// =============================================================================

// IsExclusiveLocal()
// ==================
// Return TRUE if the local Exclusives monitor for processorid includes all of
// the physical address region of size bytes starting at paddress.

// We only rely on the global exclusive monitor, so we want to leave the local
// exclusive monitor as permissive as possible.
// The impdef in shared_pseudocode always return FALSE, we always return TRUE.

func IsExclusiveLocal
   (paddress : FullAddress,
    processorid : integer,
    size : integer) => boolean
begin
  return TRUE;
end;

// =============================================================================

// Optimisations: those functions are very used all over the reference, and so
// we experience a significant slow-down if we don't override them.

// EL2Enabled()
// ============
// Returns TRUE if EL2 is present and executing
// - with the PE in Non-secure state when Non-secure EL2 is implemented, or
// - with the PE in Realm state when Realm EL2 is implemented, or
// - with the PE in Secure state when Secure EL2 is implemented and enabled, or
// - when EL3 is not implemented.

readonly func EL2Enabled() => boolean
begin
    return FALSE;
end;

// HaveAArch32()
// =============
// Return TRUE if AArch32 state is supported at at least EL0.

func HaveAArch32() => boolean
begin
  return FALSE;
end;

// HaveAArch64()
// =============
// Return TRUE if the highest Exception level is using AArch64 state.

readonly func HaveAArch64() => boolean
begin
    return TRUE;
end;

// HaveEL()
// ========
// Return TRUE if Exception level 'el' is supported

func HaveEL(el: bits(2)) => boolean
begin
    return el == EL0 || el == EL1;
end;

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

// UsingAArch32()
// ==============
// Return TRUE if the current Exception level is using AArch32, FALSE if using AArch64.
// Let us return FALSE, called by BranchTo(...) for checking tgt address size.

func UsingAArch32() => boolean
begin
  return FALSE;
end;

