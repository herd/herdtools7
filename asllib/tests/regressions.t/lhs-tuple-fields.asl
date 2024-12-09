type BV of bits (8) {
  [1:0] fldA,
  [7:5] fldB
};

func main() => integer
begin
  var bv : BV;
  bv.(fldA, -, fldB) = ('11', TRUE, '111');
  assert (bv.fldA == '11');
  assert (bv.fldB == '111');

  return 0;
end;
