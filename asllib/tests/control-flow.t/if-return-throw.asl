type E of exception {-};

func foo (x: integer) => integer
begin
  if x >= 0 then return x;
  else throw E {-};
  end;
end;

func main () => integer
begin
  let res2 = foo (2);
  let res3 = foo (3);
  assert res2 == 2;
  assert res3 == 3;

  return 0;
end;
