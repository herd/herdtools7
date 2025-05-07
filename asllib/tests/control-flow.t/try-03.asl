type E of exception {-};

func test0 () => integer
begin
  try return 0;
  catch
    when E => return 1;
    otherwise => throw E {-};
  end;
end;

func main () => integer
begin
  let res = test0();
  assert res == 0;

  return 0;
end;
