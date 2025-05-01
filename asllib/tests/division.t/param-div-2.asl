func foo {N} (bv: bits(N)) => bits(N)
begin
  let y = Ones {N DIV 2};
  return Zeros{N DIV 2} :: y;
end;

func main () => integer
begin
  // This one should work
  assert foo{2}('10') == '01';

  // This one should fail at runtime
  - = foo{3}('101');

  return 0;
end;
