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

/*
   Got with the following command line in the ARM Reference Manual XML Download folder:
      rg '.*(FEAT_\w+).*' --replace '    $1,' --no-line-number --no-filename | sort | uniq
*/

type Feature of enumeration {
    FEAT_AA32EL0,
    FEAT_AA32EL1,
    FEAT_AA32EL2,
    FEAT_AA32EL3,
    FEAT_AA32HPD,
    FEAT_AA64EL0,
    FEAT_AA64EL1,
    FEAT_AA64EL2,
    FEAT_AA64EL3,
    FEAT_ABLE,
    FEAT_AES,
    FEAT_AFP,
    FEAT_AIE,
    FEAT_ASID2,
    FEAT_ASMv8p2,
    FEAT_ATS1A,
    FEAT_BBM,
    FEAT_BBML1,
    FEAT_BF16,
    FEAT_BRBE,
    FEAT_BRBEv1p1,
    FEAT_BTI,
    FEAT_BTIE,
    FEAT_BWE,
    FEAT_BWE2,
    FEAT_CHK,
    FEAT_CLRBHB,
    FEAT_CMOW,
    FEAT_CNTSC,
    FEAT_CONSTPACFIELD,
    FEAT_CPA,
    FEAT_CPA2,
    FEAT_CRC32,
    FEAT_CSSC,
    FEAT_D128,
    FEAT_Debugv8p2,
    FEAT_Debugv8p4,
    FEAT_Debugv8p8,
    FEAT_Debugv8p9,
    FEAT_DGH,
    FEAT_DIT,
    FEAT_DoPD,
    FEAT_DotProd,
    FEAT_DoubleFault,
    FEAT_DoubleFault2,
    FEAT_DoubleLock,
    FEAT_DPB,
    FEAT_DPB2,
    FEAT_E0PD,
    FEAT_E2H0,
    FEAT_E3DSE,
    FEAT_EAESR,
    FEAT_EBEP,
    FEAT_EBF16,
    FEAT_ECV_POFF,
    FEAT_ECV,
    FEAT_EDHSR,
    FEAT_EPAC,
    FEAT_ETMv4,
    FEAT_ExS,
    FEAT_F32MM,
    FEAT_F64MM,
    FEAT_FAMINMAX,
    FEAT_FCMA,
    FEAT_FDIT,
    FEAT_FGT,
    FEAT_FGT2,
    FEAT_FHM,
    FEAT_FlagM,
    FEAT_FlagM2,
    FEAT_FP16,
    FEAT_FP8,
    FEAT_FP8DOT2,
    FEAT_FP8DOT4,
    FEAT_FP8FMA,
    FEAT_FPAC,
    FEAT_FPACCOMBINE,
    FEAT_FPMR,
    FEAT_FPRCVT,
    FEAT_FRINTTS,
    FEAT_GCIE,
    FEAT_GCS,
    FEAT_GTG,
    FEAT_HACDBS,
    FEAT_HAFDBS,
    FEAT_HAFT,
    FEAT_HBC,
    FEAT_HCX,
    FEAT_HDBSS,
    FEAT_HF_hints,
    FEAT_HPDS,
    FEAT_HPMN0,
    FEAT_I8MM,
    FEAT_IDST,
    FEAT_IESB,
    FEAT_ITE,
    FEAT_JSCVT,
    FEAT_LOR,
    FEAT_LPA,
    FEAT_LPA2,
    FEAT_LRCPC,
    FEAT_LRCPC2,
    FEAT_LRCPC3,
    FEAT_LS64_ACCDATA,
    FEAT_LS64_V,
    FEAT_LS64,
    FEAT_LS64WB,
    FEAT_LSE,
    FEAT_LSE128,
    FEAT_LSE2,
    FEAT_LSMAOC,
    FEAT_LUT,
    FEAT_LVA,
    FEAT_LVA3,
    FEAT_MEC,
    FEAT_MOPS,
    FEAT_MPAM,
    FEAT_MPAMv0p1,
    FEAT_MPAMv1p1,
    FEAT_MPAMv2_ALT_ID,
    FEAT_MPAMv2_VID,
    FEAT_MPAMv2,
    FEAT_MTE_ASYM_FAULT,
    FEAT_MTE_ASYNC,
    FEAT_MTE_CANONICAL_TAGS,
    FEAT_MTE_EIRG,
    FEAT_MTE_NO_ADDRESS_TAGS,
    FEAT_MTE_PERM,
    FEAT_MTE_STORE_ONLY,
    FEAT_MTE_TAGGED_FAR,
    FEAT_MTE,
    FEAT_MTE2,
    FEAT_MTE4,
    FEAT_NMI,
    FEAT_NV,
    FEAT_NV2,
    FEAT_NV2p1,
    FEAT_NV3,
    FEAT_PACIMP,
    FEAT_PACQARMA3,
    FEAT_PACQARMA5,
    FEAT_PAN,
    FEAT_PAN2,
    FEAT_PAN3,
    FEAT_PAuth_EnhCtl,
    FEAT_PAuth_LR,
    FEAT_PAuth,
    FEAT_PAuth2,
    FEAT_PCSRv8p9,
    FEAT_PFAR,
    FEAT_PMULL,
    FEAT_PMUv3_EDGE,
    FEAT_PMUv3_EXT32,
    FEAT_PMUv3_EXT64,
    FEAT_PMUv3_EXTPMN,
    FEAT_PMUv3_ICNTR,
    FEAT_PMUv3_SME,
    FEAT_PMUv3_SS,
    FEAT_PMUv3_TH,
    FEAT_PMUv3_TH2,
    FEAT_PMUv3,
    FEAT_PMUv3p1,
    FEAT_PMUv3p5,
    FEAT_PMUv3p7,
    FEAT_PMUv3p9,
    FEAT_PRFMSLC,
    FEAT_RAS,
    FEAT_RASv2,
    FEAT_RDM,
    FEAT_RME_GDI,
    FEAT_RME_GPC2,
    FEAT_RME_GPC3,
    FEAT_RME,
    FEAT_RPRES,
    FEAT_RPRFM,
    FEAT_S1PIE,
    FEAT_S1POE,
    FEAT_S2FWB,
    FEAT_S2PIE,
    FEAT_S2POE,
    FEAT_S2TGran16K,
    FEAT_S2TGran4K,
    FEAT_S2TGran64K,
    FEAT_SB,
    FEAT_SCTLR2,
    FEAT_SEBEP,
    FEAT_Secure,
    FEAT_SEL2,
    FEAT_SHA1,
    FEAT_SHA256,
    FEAT_SHA3,
    FEAT_SHA512,
    FEAT_SM3,
    FEAT_SM4,
    FEAT_SME_F16F16,
    FEAT_SME_F64F64,
    FEAT_SME_F8F16,
    FEAT_SME_F8F32,
    FEAT_SME_FA64,
    FEAT_SME_I16I64,
    FEAT_SME_LUTv2,
    FEAT_SME,
    FEAT_SME2,
    FEAT_SME2p1,
    FEAT_SME2p2,
    FEAT_SPE_CRR,
    FEAT_SPE_DPFZS,
    FEAT_SPE_EFT,
    FEAT_SPE_ERnd,
    FEAT_SPE_EXC,
    FEAT_SPE_FDS,
    FEAT_SPE_FnE,
    FEAT_SPE_nVM,
    FEAT_SPE_PBT,
    FEAT_SPE_SME,
    FEAT_SPE,
    FEAT_SPECRES,
    FEAT_SPECRES2,
    FEAT_SPEv1p1,
    FEAT_SPEv1p2,
    FEAT_SPEv1p4,
    FEAT_SPEv1p5,
    FEAT_SRMASK,
    FEAT_SRMASK2,
    FEAT_SSBS,
    FEAT_SSVE_AES,
    FEAT_SSVE_BitPerm,
    FEAT_SSVE_FEXPA,
    FEAT_SSVE_FP8DOT2,
    FEAT_SSVE_FP8DOT4,
    FEAT_SSVE_FP8FMA,
    FEAT_STEP2,
    FEAT_SVE_AES,
    FEAT_SVE_B16B16,
    FEAT_SVE_BitPerm,
    FEAT_SVE_PMULL128,
    FEAT_SVE_SHA3,
    FEAT_SVE_SM4,
    FEAT_SVE,
    FEAT_SVE2,
    FEAT_SVE2p1,
    FEAT_SYSINSTR128,
    FEAT_SYSREG128,
    FEAT_TCR2,
    FEAT_TGran16K,
    FEAT_TGran4K,
    FEAT_TGran64K,
    FEAT_THE,
    FEAT_TIDCP1,
    FEAT_TLBID,
    FEAT_TLBIOS,
    FEAT_TLBIRANGE,
    FEAT_TLBIW,
    FEAT_TME,
    FEAT_TRBE_EXC,
    FEAT_TRBE_EXT,
    FEAT_TRBE_MPAM,
    FEAT_TRBE,
    FEAT_TRBEv1p1,
    FEAT_TRF,
    FEAT_TTCNP,
    FEAT_TTL,
    FEAT_TTST,
    FEAT_TWED,
    FEAT_UAO,
    FEAT_UINJ,
    FEAT_VHE,
    FEAT_VMID16,
    FEAT_WFxT,
    FEAT_XNX,
    FEAT_XS,
};

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
