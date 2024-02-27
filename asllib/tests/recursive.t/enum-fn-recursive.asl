type MyEnum of enumeration { A, B, C };

func foo (x: integer) => integer
begin
  var e = B;
  var f = D;

  return x;
end

type MyEnum2 of enumeration { D, E, F };

func main () => integer
begin
  return foo(0);
end
