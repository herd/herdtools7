func main () => integer
begin

  let my_unconstrained_integer = 3 as integer;
  assert my_unconstrained_integer == 3;

  let my_constrained_integer = 3 as integer {3..5};
  assert my_constrained_integer == 3;

  return 0;
end;
