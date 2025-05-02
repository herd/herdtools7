type E of exception {-};

func test0 () => integer
begin
  try throw E {-};
  catch when E => return 1; end;
end;

func main () => integer
begin
  let res = test0();
  assert res == 1;

  return 0;
end;
