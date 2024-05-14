// Not so sure about it, the is some affectation of the mair fied: `walkparams.mair = MAIR_EL1;`
// And MAIR_ELI is a ARMv8 64bit register.
type MAIRType of bits(64);

var MAIR_EL1 : MAIRType ;

// Inferred from manual...
type TranslationControl of bits(64) {
 [5:0] T0SZ,
 [7] EPD0,
 [9:8] IRGN0,
 [11:10] ORGN0,
 [13:12] SH0,
 [15:14] TG0,
 [21:16] T1SZ,
 [22] A1,
 [23] EPD1,
 [25:24] IRGN1,
 [27:26] ORGN1,
 [29:28] SH1,
 [31:30] TG1,
 [34:32] IPS,
 [36] AS,
 [37] TBI0,
 [38] TBI1,
 [39] HA,
 [40] HD,
 [41] HPD0,
 [42] HPD1,
 [43] HWU059,
 [44] HWU060,
 [45] HWU061,
 [46] HWU062,
 [47] HWU159,
 [48] HWU160,
 [49] HWU161,
 [50] HWU162,
 [51] TBID0,
 [52] TBID1,
 [53] NFD0,
 [54] NFD1,
 [55] E0PD0,
 [56] E0PD1,
 [57] TCMA0,
 [58] TCMA1,
 [59] DS,
 [60] MTX0,
 [61] MTX1,
};

// Value found on Rasberry 4B, ArmBian
// uname -a:
// Linux cheilly 5.4.0-1089-raspi #100-Ubuntu SMP PREEMPT Thu Jun 22 09:59:38 UTC 2023 aarch64 aarch64 aarch64 GNU/Linux

let TCR_EL1 : TranslationControl =
  '0000000000000000000000000000010011110101100100000111010100010000';

// Not used normaly, here for typing
var TCR_EL2 : TranslationControl;

// Inferred from manual...
type ExtendedTranslationControl of bits(64) {
 [0] PnCH,
 [1] PIE,
 [2] P0POE,
 [3] POE,
 [4] AIE,
 [5] D128,
 [10] PTTWI,
 [11] HAFT,
 [14] DisCH0,
 [15] DisCH1,
 [16] A2,
 [17] FNG0,
 [18] FNG1,
};

var TCR2_EL1 : ExtendedTranslationControl;

// Inferred from manual
type ExtendedSystemControl of bits(64) {
  [1] EMEC,
  [2] NMEA,
  [3] EnADERR,
  [4] EnANERR,
  [5] EASE,
  [6] EnIDCP128,
  [7] EnPACM,
  [8] EnPACM0,
  [9] CPTA,
  [10] CPTA0,
  [11] CPTM,
  [12] CPTM0,
};

var SCTLR2_EL2 : ExtendedSystemControl;

// From manual

type LockStatus of bits(64) {
  [1] OSLK,
  [2] nTT,
  [4:3] OSLM, // Normaly, OSLM is made of bits 0 and 3..
};

var OSLSR_EL1 : LockStatus;

// Have a try

type ExternalDebugStatus of bits(32) {
 [5:0] STATUS,
};

var EDSCR : ExternalDebugStatus;

// Translation table registers
type TTType of bits(64);

// Probably temporary...
let TTBR1_EL1 = Zeros{64};
let TTBR0_EL1 = Zeros{64};

//From manual, limited to what is used

type HypervisorConfiguration of bits(64) {
  [0] VM,
  [12] DC,
};

let HCR_EL2 : HypervisorConfiguration = Zeros{64};

func SynchronizeContext()
begin
 return;
end;

func ThisInstrLength() => integer
begin
  return 32;
end;

var SPSR_EL1 : bits(64);
var SPSR_EL2 : bits(64);

func IsPhysicalSErrorPending() => boolean
begin
  return FALSE;
end;


// IsExternalAbortTakenSynchronously()
// ===================================
// Return an implementation specific value:
// TRUE if the fault returned for the access can be taken synchronously,
// FALSE otherwise.
//
// This might vary between accesses, for example depending on the error type
// or memory type being accessed.
// External aborts on data accesses and translation table walks on data accesses
// can be either synchronous or asynchronous.
//
// When FEAT_DoubleFault is not implemented, External aborts on instruction
// fetches and translation table walks on instruction fetches can be either
// synchronous or asynchronous.
// When FEAT_DoubleFault is implemented, all External abort exceptions on
// instruction fetches and translation table walks on instruction fetches
// must be synchronous.
// Luc: Dubious, FALSE or TRUE ?
func
  IsExternalAbortTakenSynchronously
    (memstatus:PhysMemRetStatus,
     iswrite:boolean,
     desc:AddressDescriptor,
     size:integer,
     accdesc:AccessDescriptor) => boolean
begin
  return FALSE;
end;

// PendSErrorInterrupt()
// =====================
// Pend the SError Interrupt.

func PendSErrorInterrupt(fault:FaultRecord)
begin
  return;
end;


// WatchpointRelatedSyndrome()
// ===========================
// Update common Watchpoint related fields.

func
  WatchpointRelatedSyndrome(fault:FaultRecord,vaddress:bits(64))
    => bits(24)
begin
  return  Zeros(24);
end;
