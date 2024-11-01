func foo {N} (x: bits(N))
begin
  let y = 5 DIV (N + 1);
end;

func main () => integer
begin
  foo ('1');
  foo ('');
  return 0;
end;

