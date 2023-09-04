type CPTR_EL2_Type of bits(64) {
  // common across formats
  [31] TCPAC,
  [30] TAM,

  // View when E2H register has value '0'
  [29:0] E2H0 {
      [20] TTA,
      [10] TFP,
      [8]  TZ
  },

  // View when E2H register has value '1'
  [29:0] E2H1 {
      [28]    TTA,
      [20+:2] FPEN,
      [16+:2] ZEN }
};


// Other syntax not in the LRM example
type E2H0Type of bits(30) {
      [20] TTA,
      [10] TFP,
      [8]  TZ
};

type CPTR_EL2_Type_bis of bits(64) {
  [31] TCPAC,
  [30] TAM,
  [29:0] E2H0: E2H0Type,
  [29:0] E2H1: bits(30) {
      [28]    TTA,
      [20+:2] FPEN,
      [16+:2] ZEN
  },
};

func main() => integer
begin
  var E2H: bit;
  var CPTR: CPTR_EL2_Type;

  // Select TTA depending on the value of E2H
  let TTA: bit = if E2H == '0' then CPTR.E2H0.TTA else CPTR.E2H1.TTA;

  var E2H_bis: bit;
  var CPTR_bis: CPTR_EL2_Type_bis;

  // Select TTA depending on the value of E2H
  let TTA_bis: bit = if E2H_bis == '0' then CPTR_bis.E2H0.TTA else CPTR_bis.E2H1.TTA;

  return 0;
end
