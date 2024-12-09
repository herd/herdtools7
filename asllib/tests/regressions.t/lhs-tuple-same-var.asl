type BV of bits (8) {
  [1:0] fld
};

func main() => integer
begin
  var bv : BV;
  (bv[7], -, bv.fld) = ('1', TRUE, '11');

  assert FALSE;
  return 0;
end;
