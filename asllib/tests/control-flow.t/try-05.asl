type E of exception {-};
type F of exception {-};

func test0 () => integer
begin
  try throw E {-};
  catch
    when E => return 1;
    when F => println("Caught F");
    otherwise => throw E {-};
  end;
end;

func main () => integer
begin
  let res = test0();
  assert res == 0;

  return 0;
end;
