constant N: integer {1,3} = 1;

func MyBits{N}(x: bits(N)) => integer {N+1}
begin
  return N + 1;
end;
