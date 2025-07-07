pure func foo (x: integer {1..10}) => integer {0..10_000}
begin
  var result: integer {0..10_000} = 79;
  result = result DIVRM x;
  return result;
end;

type T of bits (foo(4));

func main () => integer
begin
  var x: T = Zeros{foo(4)};
  assert Len(x) == 19;

  return 0;
end;


