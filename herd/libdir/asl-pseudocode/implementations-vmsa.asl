// Not so sure about it, the is some affectation of the mair fied: `walkparams.mair = MAIR_EL1;`
// And MAIR_ELI is a ARMv8 64bit register.
type MAIRType of bits(64);

var MAIR_EL1 : MAIRType ;

// Inferred from manual...
type TranslationControl of bits(64) {
 [5:0] T0SZ,
 [7] EDP0,
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

var TCR_EL1 : TranslationControl;