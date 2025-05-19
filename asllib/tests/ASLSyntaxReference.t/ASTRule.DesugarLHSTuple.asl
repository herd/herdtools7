type MyBV of bits(16) {
  [3:0] fld
};

func main() => integer
begin
  var x: array[[4]] of integer;
  var y: MyBV;
  var z: bits(4);
  (x[[0]], y.fld, -, z[:1]) = (0, '1111', TRUE, '0');

  // The above left-hand side desugars to the following `lexpr`:
  // LE_Destructuring (
  //   LE_SetArray(LE_Var("x"), 0),
  //   LE_SetField(LE_Var("y"), "fld"),
  //   LE_Discard,
  //   LE_Slice(LE_Var("z"), [:1])
  // )

  return 0;
end;
