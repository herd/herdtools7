func omit_too_many_parameters()
begin
  let x : bits(64) = Extend('1111', TRUE);
end;

func main() => integer
begin
  return 0;
end;
