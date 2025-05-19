type MyBV of bits(16) {
  [3:0] fld1 : bits(4) {
    [3:0] fld2
  }
};

var x: array[[4]] of MyBV;

func main() => integer
begin
  x[[0]].fld1.fld2[:1] = '0';

  // The above left-hand side desugars to the following `lexpr`:
  // LE_Slice( LE_SetField( LE_SetField( LE_SetArray( LE_Var("x"),
  //   0), "fld1"), "fld2"), [:1])

  return 0;
end;
