type MyBV of bits(16) {
  [3:0] fld1,
  [15:12] fld2
};

func main() => integer
begin
  var x: MyBV;
  x.(fld1, -, fld2) = ('0000', '0101', '1111');

  // The above left-hand side desugars into
  (x.fld1, -, x.fld2) = ('0000', '0101', '1111');

  return 0;
end;
