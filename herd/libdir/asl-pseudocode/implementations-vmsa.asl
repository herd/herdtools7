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
let TTBR1_EL1 = Zeros(64);
let TTBR0_EL1 = Zeros(64);

//From manual, limited to what is used

type HypervisorConfiguration of bits(64) {
  [0] VM,
  [12] DC,
};

let HCR_EL2 : HypervisorConfiguration = Zeros(64);
