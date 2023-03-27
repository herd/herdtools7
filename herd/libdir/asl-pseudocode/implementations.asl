
type Feature of enumeration {
    FEAT_AA32BF16,
    FEAT_AA32HPD,
    FEAT_AA32I8MM,
    FEAT_AES,
    FEAT_AFP,
    FEAT_AIE,
    FEAT_B16B16,
    FEAT_BBM,
    FEAT_BF16,
    FEAT_BRBE,
    FEAT_BRBEv1p1,
    FEAT_BTI,
    FEAT_CCIDX,
    FEAT_CLRBHB,
    FEAT_CMOW,
    FEAT_CNTSC,
    FEAT_CONSTPACFIELD,
    FEAT_CRC32,
    FEAT_CSSC,
    FEAT_D128,
    FEAT_DGH,
    FEAT_DIT,
    FEAT_Debugv8p2,
    FEAT_Debugv8p4,
    FEAT_Debugv8p8,
    FEAT_Debugv8p9,
    FEAT_DoPD,
    FEAT_DotProd,
    FEAT_DoubleFault,
    FEAT_DoubleFault2,
    FEAT_DoubleLock,
    FEAT_E0PD,
    FEAT_EBEP,
    FEAT_EBF16,
    FEAT_ECV,
    FEAT_EPAC,
    FEAT_ETE,
    FEAT_F32MM,
    FEAT_F64MM,
    FEAT_FCMA,
    FEAT_FGT,
    FEAT_FHM,
    FEAT_FP16,
    FEAT_FPAC,
    FEAT_FPACCOMBINE,
    FEAT_FRINTTS,
    FEAT_FlagM,
    FEAT_FlagM2,
    FEAT_GCS,
    FEAT_GTG,
    FEAT_HAFDBS,
    FEAT_HAFT,
    FEAT_HBC,
    FEAT_HCX,
    FEAT_HPDS,
    FEAT_HPDS2,
    FEAT_HPMN0,
    FEAT_I8MM,
    FEAT_IDST,
    FEAT_IESB,
    FEAT_ITE,
    FEAT_JSCVT,
    FEAT_LPA,
    FEAT_LPA2,
    FEAT_LRCPC3,
    FEAT_LS64,
    FEAT_LS64_ACCDATA,
    FEAT_LS64_V,
    FEAT_LSE,
    FEAT_LSE128,
    FEAT_LSE2,
    FEAT_LSMAOC,
    FEAT_LVA,
    FEAT_LVA3,
    FEAT_MEC,
    FEAT_MOPS,
    FEAT_MPAM,
    FEAT_MPAMv0p1,
    FEAT_MPAMv1p1,
    FEAT_MTE,
    FEAT_MTE2,
    FEAT_MTE4,
    FEAT_MTE_CANONICAL_TAGS,
    FEAT_MTE_PERM,
    FEAT_MTE_STORE_ONLY,
    FEAT_NMI,
    FEAT_NV,
    FEAT_NV2,
    FEAT_PACIMP,
    FEAT_PACQARMA3,
    FEAT_PACQARMA5,
    FEAT_PAN,
    FEAT_PAN3,
    FEAT_PAuth,
    FEAT_PAuth2,
    FEAT_PFAR,
    FEAT_PMULL,
    FEAT_PMUv3,
    FEAT_PMUv3_EDGE,
    FEAT_PMUv3_ICNTR,
    FEAT_PMUv3_TH,
    FEAT_PMUv3p1,
    FEAT_PMUv3p4,
    FEAT_PMUv3p5,
    FEAT_PMUv3p7,
    FEAT_PMUv3p9,
    FEAT_RAS,
    FEAT_RASv2,
    FEAT_RDM,
    FEAT_RME,
    FEAT_RNG,
    FEAT_RPRES,
    FEAT_S1PIE,
    FEAT_S1POE,
    FEAT_S2FWB,
    FEAT_S2PIE,
    FEAT_S2POE,
    FEAT_SB,
    FEAT_SCTLR2,
    FEAT_SEL2,
    FEAT_SHA1,
    FEAT_SHA256,
    FEAT_SHA3,
    FEAT_SHA512,
    FEAT_SM3,
    FEAT_SM4,
    FEAT_SME,
    FEAT_SME2,
    FEAT_SME2p1,
    FEAT_SME_F16F16,
    FEAT_SME_F64F64,
    FEAT_SME_FA64,
    FEAT_SME_I16I64,
    FEAT_SPE,
    FEAT_SPE_FDS,
    FEAT_SPEv1p1,
    FEAT_SPEv1p2,
    FEAT_SPEv1p4,
    FEAT_SSBS,
    FEAT_SVE,
    FEAT_SVE2,
    FEAT_SVE2p1,
    FEAT_SVE_AES,
    FEAT_SVE_BitPerm,
    FEAT_SVE_PMULL128,
    FEAT_SVE_SHA3,
    FEAT_SVE_SM4,
    FEAT_SYSINSTR128,
    FEAT_SYSREG128,
    FEAT_TCR2,
    FEAT_THE,
    FEAT_TIDCP1,
    FEAT_TME,
    FEAT_TRBE,
    FEAT_TRBE_EXT,
    FEAT_TRF,
    FEAT_TTCNP,
    FEAT_TTST,
    FEAT_TWED,
    FEAT_UAO,
    FEAT_VHE,
    FEAT_VMID16,
    FEAT_WFxT,
    FEAT_XNX,
    FEAT_XS,
};

func IsFeatureImplemented(f :: Feature) => boolean
begin
    return FALSE;
end

func PhysMemWrite(
  desc::AddressDescriptor,
  size::integer,
  accdesc::AccessDescriptor,
  value::bits(8*size)
) => PhysMemRetStatus
begin
  write_memory (desc.vaddress, size*8, value);
  return PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
end

func PhysMemRead(
  desc::AddressDescriptor,
  size::integer,
  accdesc::AccessDescriptor
) => (PhysMemRetStatus, bits(8*size))
begin
  value = read_memory (desc.vaddress, size*8);
  ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
  return (ret_status, value);
end

func HaveAArch32() => boolean
begin
  return FALSE;
end

func HaveAArch64() => boolean
begin
  return FALSE;
end

// HaveEL()
// ========
// Return TRUE if Exception level 'el' is supported

func HaveEL(el:: bits(2)) => boolean
begin
    if el IN {EL1,EL0} then
        return TRUE;                             // EL1 and EL0 must exist
    else
        return FALSE; // boolean IMPLEMENTATION_DEFINED;
    end
end

// ClearExclusiveByAddress()
// =========================
// Clear the global Exclusives monitors for all PEs EXCEPT processorid if they
// record any part of the physical address region of size bytes starting at paddress.
// It is IMPLEMENTATION DEFINED whether the global Exclusives monitor for processorid
// is also cleared if it records any part of the address region.

func ClearExclusiveByAddress(paddress :: FullAddress, processorid :: integer, size :: integer)
begin
  pass;
end
