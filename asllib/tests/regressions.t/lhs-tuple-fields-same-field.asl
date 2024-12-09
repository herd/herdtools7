type BV of bits (8) {
  [1:0] fld,
};

func main() => integer
begin
  var bv : BV;
  bv.(fld, -, fld) = ('11', TRUE, '11');

  assert FALSE;
  return 0;
end;
