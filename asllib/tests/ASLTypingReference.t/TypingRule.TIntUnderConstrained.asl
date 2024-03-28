func foo {N} (x: bits(N)) => integer
begin
  return N;
end

func bar (N: integer) => bits(N)
begin
  return Zeros(N);
end

func main() => integer
begin
  assert 3 == foo ('101');
  assert bar(3) == '000';

  return 0;
end

