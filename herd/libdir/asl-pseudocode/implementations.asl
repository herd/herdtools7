type Unpredictable of enumeration {
// VMSR on MVFR
                           Unpredictable_VMSR,
// Writeback/transfer register overlap (load)
                           Unpredictable_WBOVERLAPLD,
// Writeback/transfer register overlap (store)
                           Unpredictable_WBOVERLAPST,
// Load Pair transfer register overlap
                           Unpredictable_LDPOVERLAP,
// Store-exclusive base/status register overlap
                           Unpredictable_BASEOVERLAP,
// Store-exclusive data/status register overlap
                           Unpredictable_DATAOVERLAP,
// Load-store alignment checks
                           Unpredictable_DEVPAGE2,
// Instruction fetch from Device memory
                           Unpredictable_INSTRDEVICE,
// Reserved CPACR value
                           Unpredictable_RESCPACR,
// Reserved MAIR value
                           Unpredictable_RESMAIR,
// Effect of SCTLR_ELx.C on Tagged attribute
                           Unpredictable_S1CTAGGED,
// Reserved Stage 2 MemAttr value
                           Unpredictable_S2RESMEMATTR,
// Reserved TEX:C:B value
                           Unpredictable_RESTEXCB,
// Reserved PRRR value
                           Unpredictable_RESPRRR,
// Reserved DACR field
                           Unpredictable_RESDACR,
// Reserved VTCR.S value
                           Unpredictable_RESVTCRS,
// Reserved TCR.TnSZ value
                           Unpredictable_RESTnSZ,
// Reserved SCTLR_ELx.TCF value
                           Unpredictable_RESTCF,
// Tag stored to Device memory
                           Unpredictable_DEVICETAGSTORE,
// Out-of-range TCR.TnSZ value
                           Unpredictable_OORTnSZ,
 // IPA size exceeds PA size
                           Unpredictable_LARGEIPA,
// Syndrome for a known-passing conditional A32 instruction
                           Unpredictable_ESRCONDPASS,
// Illegal State exception: zero PSTATE.IT
                           Unpredictable_ILZEROIT,
// Illegal State exception: zero PSTATE.T
                           Unpredictable_ILZEROT,
// Debug: prioritization of Vector Catch
                           Unpredictable_BPVECTORCATCHPRI,
// Debug Vector Catch: match on 2nd halfword
                           Unpredictable_VCMATCHHALF,
// Debug Vector Catch: match on Data Abort
// or Prefetch abort
                           Unpredictable_VCMATCHDAPA,
// Debug watchpoints: nonzero MASK and non-ones BAS
                           Unpredictable_WPMASKANDBAS,
// Debug watchpoints: non-contiguous BAS
                           Unpredictable_WPBASCONTIGUOUS,
// Debug watchpoints: reserved MASK
                           Unpredictable_RESWPMASK,
// Debug watchpoints: nonzero MASKed bits of address
                           Unpredictable_WPMASKEDBITS,
// Debug breakpoints and watchpoints: reserved control bits
                           Unpredictable_RESBPWPCTRL,
// Debug breakpoints: not implemented
                           Unpredictable_BPNOTIMPL,
// Debug breakpoints: reserved type
                           Unpredictable_RESBPTYPE,
// Debug breakpoints and watchpoints: reserved MDSELR_EL1.BANK
                           Unpredictable_RESMDSELR,
// Debug breakpoints: not-context-aware breakpoint
                           Unpredictable_BPNOTCTXCMP,
// Debug breakpoints: match on 2nd halfword of instruction
                           Unpredictable_BPMATCHHALF,
// Debug breakpoints: mismatch on 2nd halfword of instruction
                           Unpredictable_BPMISMATCHHALF,
// Debug breakpoints: a breakpoint is linked to that is not
// programmed with linking enabled
                           Unpredictable_BPLINKINGDISABLED,
// Debug breakpoints: reserved MASK
                           Unpredictable_RESBPMASK,
// Debug breakpoints: MASK is set for a Context matching
// breakpoint or when DBGBCR_EL1[n].BAS != '1111'
                           Unpredictable_BPMASK,
// Debug breakpoints: nonzero MASKed bits of address
                           Unpredictable_BPMASKEDBITS,
// Debug breakpoints: A linked breakpoint is
// linked to an address matching breakpoint
                           Unpredictable_BPLINKEDADDRMATCH,
// Debug: restart to a misaligned AArch32 PC value
                           Unpredictable_RESTARTALIGNPC,
// Debug: restart to a not-zero-extended AArch32 PC value
                           Unpredictable_RESTARTZEROUPPERPC,
// Zero top 32 bits of X registers in AArch32 state
                           Unpredictable_ZEROUPPER,
// Zero top 32 bits of PC on illegal return to
// AArch32 state
                           Unpredictable_ERETZEROUPPERPC,
// Force address to be aligned when interworking
// branch to A32 state
                           Unpredictable_A32FORCEALIGNPC,
// SMC disabled
                           Unpredictable_SMD,
// FF speculation
                           Unpredictable_NONFAULT,
// Zero top bits of Z registers in EL change
                           Unpredictable_SVEZEROUPPER,
// Load mem data in NF loads
                           Unpredictable_SVELDNFDATA,
// Write zeros in NF loads
                           Unpredictable_SVELDNFZERO,
// SP alignment fault when predicate is all zero
                           Unpredictable_CHECKSPNONEACTIVE,
// Zero top bits of ZA registers in EL change
                           Unpredictable_SMEZEROUPPER,
// Watchpoint match of last rounded up memory access in case of
// 16 byte rounding
                           Unpredictable_16BYTEROUNDEDUPACCESS,
// Watchpoint match of first rounded down memory access in case of
// 16 byte rounding
                           Unpredictable_16BYTEROUNDEDDOWNACCESS,
// HCR_EL2.<NV,NV1> == '01'
                           Unpredictable_NVNV1,
// Reserved shareability encoding
                           Unpredictable_Shareability,
// Access Flag Update by HW
                           Unpredictable_AFUPDATE,
// Dirty Bit State Update by HW
                           Unpredictable_DBUPDATE,
// Consider SCTLR_ELx[].IESB in Debug state
                           Unpredictable_IESBinDebug,
// Bad settings for PMSFCR_EL1/PMSEVFR_EL1/PMSLATFR_EL1
                           Unpredictable_BADPMSFCR,
// Zero saved BType value in SPSR_ELx/DPSR_EL0
                           Unpredictable_ZEROBTYPE,
// Timestamp constrained to virtual or physical
                           Unpredictable_EL2TIMESTAMP,
                           Unpredictable_EL1TIMESTAMP,
 // Reserved MDCR_EL3.<NSTBE,NSTB> or MDCR_EL3.<NSPBE,NSPB> value
                            Unpredictable_RESERVEDNSxB,
// WFET or WFIT instruction in Debug state
                           Unpredictable_WFxTDEBUG,
// Address does not support LS64 instructions
                           Unpredictable_LS64UNSUPPORTED,
// Misaligned exclusives, atomics, acquire/release
// to region that is not Normal Cacheable WB
                           Unpredictable_MISALIGNEDATOMIC,
// 128-bit Atomic or 128-bit RCW{S} transfer register overlap
                           Unpredictable_LSE128OVERLAP,
// Clearing DCC/ITR sticky flags when instruction is in flight
                           Unpredictable_CLEARERRITEZERO,
// ALUEXCEPTIONRETURN when in user/system mode in
// A32 instructions
                           Unpredictable_ALUEXCEPTIONRETURN,
// Trap to register in debug state are ignored
                           Unpredictable_IGNORETRAPINDEBUG,
// Compare DBGBVR.RESS for BP/WP
                           Unpredictable_DBGxVR_RESS,
// Inaccessible event counter
                           Unpredictable_PMUEVENTCOUNTER,
// Reserved PMSCR.PCT behavior
                           Unpredictable_PMSCR_PCT,
// MDCR_EL2.HPMN or HDCR.HPMN is larger than PMCR.N or
// FEAT_HPMN0 is not implemented and HPMN is 0.
                           Unpredictable_CounterReservedForEL2,
// Generate BRB_FILTRATE event on BRB injection
                           Unpredictable_BRBFILTRATE,
// Generate PMU_SNAPSHOT event in Debug state
                           Unpredictable_PMUSNAPSHOTEVENT,
// Reserved MDCR_EL3.EPMSSAD value
                           Unpredictable_RESEPMSSAD,
// Reserved PMECR_EL1.SSE value
                           Unpredictable_RESPMSSE,
// Enable for PMU exception and PMUIRQ
                           Unpredictable_RESPMEE,
// Operands for CPY*/SET* instructions overlap or
// use 0b11111 as a register specifier
                           Unpredictable_MOPSOVERLAP31,
// Store-only Tag checking on a failed Atomic Compare and Swap
                           Unpredictable_STOREONLYTAGCHECKEDCAS,
// Reserved MDCR_EL3.ETBAD value
                           Unpredictable_RES_ETBAD,
// accessing DBGDSCRint via MRC in debug state
                           Unpredictable_MRC_APSR_TARGET,
// Reserved PMEVTYPER<n>_EL0.TC value
                           Unpredictable_RESTC
};

func ConstrainUnpredictableBool(which:Unpredictable) => boolean
begin
  return FALSE;
end

type signal of integer;

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
  do_write_memory (desc.vaddress, size*8, value,accdesc);
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
  let value = read_memory (desc.vaddress, size*8);
  let ret_status = PhysMemRetStatus {
    statuscode = Fault_None,
    extflag = '0',
    merrorstate = ErrorState_CE,  // ??
    store64bstatus = Zeros(64)
  };
  return (ret_status, (value as bits((8*size))));
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


getter _R [n :: integer] => bits(64)
begin
  return read_register(n);
end

setter _R [n :: integer] = value :: bits(64)
begin
  write_register(n, value);
end

type SCTLRType of bits(64) {
  [0] M,
  [1] A,
  [2] C,
  [3] SA,
  [4] SA0,
  [5] CP15BEN,
  [6] nAA,
  [7] ITD,
  [8] SED,
  [9] UMA,
  [10] EnRCTX,
  [11] EOS,
  [12] I,
  [13] EnDB,
  [14] DZE,
  [15] UCT,
  [16] nTWI,
  [17] RES0,
  [18] nTWE,
  [19] WXN,
  [20] TSCXT,
  [21] IESB,
  [22] EIS,
  [23] SPAN,
  [24] EOE,
  [25] EE,
  [26] UCI,
  [27] EnDA,
  [28] nTSLMD,
  [29] LSMAOE,
  [30] EnIB,
  [31] EnIA,

  [35] BT0,
  [36] BT1,
  [37] ITFSB,
  [39:38] TCF0,
  [41:40] TCF,
  [42] ATA0,
  [43] ATA,
  [44] DSSBS,
  [45] TWEDEn,
  [49:46] TWEDEL,

  [54] EnASR,
  [55] EnAS0,
  [56] EnALS,
  [57] EPAN,
};

getter SCTLR_EL1[] => SCTLRType
begin
  return Zeros(64);
end
