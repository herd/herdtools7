constant MYCONST = 1;

func Parameterised{N}() => bits(N)
begin
  var a : bits(N);
  assert a == 0[:N];

  var b: bits(N+MYCONST);
  assert b == 0[:N+MYCONST];

  let c = LowestSetBit(a);
  var d: bits(c);
  assert d == 0[:c];

  return a;
end;

func main() => integer
begin
  - = Parameterised{8};
  return 0;
end;
