var global_var: integer = 0;

func side_effecting() => integer
begin
  global_var = global_var + 1;
  return global_var;
end;

func main () => integer
begin
  var x: bits(64);
  let y = x[side_effecting()];

  return 0;
end;
