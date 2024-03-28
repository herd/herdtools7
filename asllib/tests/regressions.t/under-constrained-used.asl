func foo {N} (x: bits(N)) => integer {0..2*N}
begin
  return N as integer {0..2*N};
end

func main () => integer
begin
  assert foo ('100') == 3;

  return 0;
end
