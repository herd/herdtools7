func foo {N, M} (bv: bits(N), bv2: bits(M)) => integer {N..M}
begin
  var x: integer {N..M};
  return x;
end;

func main () => integer
begin
  let x0 = foo ('00', '0000');
  assert x0 == 2;

  let x1 = foo ('00', '00');
  assert x1 == 2;

  let x2 = foo ('00', '0');
  assert FALSE; // Should have failed earlier!
end;
