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

// MPAMinfo GenMPAMAtEL(AccessType acctype, bits(2) el)
func GenMPAMAtEL(acctype: AccessType, el: bits(2)) => MPAMinfo
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

func NormalWBISHMemAttr() => MemoryAttributes
begin
  return MemoryAttributes {
    memtype = MemType_Normal,
    inner = MemAttrHints {
      attrs = MemAttr_WB,
      hints = MemHint_No, // ??
      transient = FALSE // Only applies to cacheable memory
    },
    outer = MemAttrHints {
      attrs = MemAttr_WB,
      hints = MemHint_No, // ??
      transient = FALSE // Only applies to cacheable memory
    },
    shareability = Shareability_ISH,
    tags = MemTag_Untagged, // ??
    device = DeviceType_GRE, // Not relevant for Normal
    notagaccess = TRUE, // Not used in shared_pseudocode
    xs = '0' // If I understand correctly WalkMemAttrs
  };
end;

func AArch64_TranslateAddress(address:bits(64), accdesc:AccessDescriptor, aligned:boolean, size:integer) => AddressDescriptor
begin
  var full_addr : FullAddress;
  return CreateAddressDescriptor(address, full_addr, NormalWBISHMemAttr(), accdesc);
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
// Processor state bits.
// There is no significance to the field order.

// From https://developer.arm.com/documentation/ddi0602/2023-12/Shared-Pseudocode/shared-functions-system?lang=en#ProcState
// Rewritten from ASLv0 record to ASLv1 collection

var PSTATE: collection {
    N: bits (1),        // Negative condition flag
    Z: bits (1),        // Zero condition flag
    C: bits (1),        // Carry condition flag
    V: bits (1),        // Overflow condition flag
    D: bits (1),        // Debug mask bit                     [AArch64 only]
    A: bits (1),        // SError interrupt mask bit
    I: bits (1),        // IRQ mask bit
    F: bits (1),        // FIQ mask bit
    EXLOCK: bits (1),   // Lock exception return state
    PAN: bits (1),      // Privileged Access Never Bit        [v8.1]
    UAO: bits (1),      // User Access Override               [v8.2]
    DIT: bits (1),      // Data Independent Timing            [v8.4]
    TCO: bits (1),      // Tag Check Override                 [v8.5, AArch64 only]
    PM: bits (1),       // PMU exception Mask
    PPEND: bits (1),     // synchronous PMU exception to be observed
    BTYPE: bits (2),    // Branch Type                        [v8.5]
    PACM: bits (1),     // PAC instruction modifier
    ZA: bits (1),       // Accumulation array enabled         [SME]
    SM: bits (1),       // Streaming SVE mode enabled         [SME]
    ALLINT: bits (1),   // Interrupt mask bit
    UINJ: bits (1),     // Undefined Exception Injection
    SS: bits (1),       // Software step bit
    IL: bits (1),       // Illegal Execution state bit
    EL: bits (2),       // Exception level
    nRW: bits (1),      // Execution state: 0=AArch64, 1=AArch32
    SP: bits (1),       // Stack pointer select: 0=SP0, 1=SPx [AArch64 only]
    Q: bits (1),        // Cumulative saturation flag         [AArch32 only]
    GE: bits (4),       // Greater than or Equal flags        [AArch32 only]
    SSBS: bits (1),     // Speculative Store Bypass Safe
    IT: bits (8),       // If-then bits, RES0 in CPSR         [AArch32 only]
    J: bits (1),        // J bit, RES0                        [AArch32 only, RES0 in SPSR and CPSR]
    T: bits (1),        // T32 bit, RES0 in CPSR              [AArch32 only]
    E: bits (1),        // Endianness bit                     [AArch32 only]
    M: bits (5)         // Mode field                         [AArch32 only]
};

// =============================================================================

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

// MemSingleGranule()
// ==================
// When FEAT_LSE2 is implemented, for some memory accesses if all bytes
// of the accesses are within 16-byte quantity aligned to 16-bytes and
// satisfy additional requirements - then the access is guaranteed to
// be single copy atomic.
// However, when the accesses do not all lie within such a boundary, it
// is CONSTRAINED UNPREDICTABLE if the access is single copy atomic.
// In the pseudocode, this CONSTRAINED UNPREDICTABLE aspect is modeled via
// MemSingleGranule() which is IMPLEMENTATION DEFINED and, is at least 16 bytes
// and at most 4096 bytes.
// This is a limitation of the pseudocode.
//
// LUC Granule size set to 32, why not!
func MemSingleGranule() => integer
  begin
    let size = 32;
    // access is assumed to be within 4096 byte aligned quantity to
    // avoid multiple translations for a single copy atomic access.
    assert (size >= 16) && (size <= 4096);
    return size;
  end;

// CheckOriginalSVEEnabled()
// =========================
// Checks for traps on SVE instructions and instructions that access SVE System
// registers.
// LUC, just allow

func CheckOriginalSVEEnabled()
begin
  return;
end;

// Here because it is defined 2 times in the release

// SecurityState
// =============
// The Security state of an execution context

type SecurityState of enumeration {
    SS_NonSecure,
    SS_Root,
    SS_Realm,
    SS_Secure
};

constant VMID_NONE : bits(16) = Zeros{16};
constant ASID_NONE : bits(16) = Zeros{16};
