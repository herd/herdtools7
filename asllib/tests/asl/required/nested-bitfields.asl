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

func main() => integer
begin
  var E2H: bit;
  var CPTR: CPTR_EL2_Type;

  // Select TTA depending on the value of E2H
  let TTA: bit = if E2H == '0' then CPTR.E2H0.TTA else CPTR.E2H1.TTA;

  return 0;
end
