// return value must be within the constraints of the return type
func negative6{N}(x: bits(N)) => integer {0..N}
begin
  return N + 1; // illegal
end;
