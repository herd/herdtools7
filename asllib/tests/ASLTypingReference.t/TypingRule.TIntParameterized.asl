func foo {N} (x: bits(N)) => integer
begin
  return N;
end;

func bar{M: integer}() => bits(M)
begin
  return Zeros{M};
end;

func main() => integer
begin
  assert 3 == foo{3}('101');
  assert bar{3} == '000';

  return 0;
end;
