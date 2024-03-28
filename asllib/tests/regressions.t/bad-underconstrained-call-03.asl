func foo {N} (x: bits(N), i: integer {N})
begin
  assert Len(x) == i;
end

func bar {M} (x: bits(M))
begin
  foo (x, M + 1);
end

func main () => integer
begin
  bar ('101010');
  return 0;
end

