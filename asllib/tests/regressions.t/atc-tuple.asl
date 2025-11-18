type Named of enumeration { Named_X, Named_Y };

func Foo() => (Named, integer)
begin
  return (ARBITRARY : Named, 0);
end;

func main () => integer
begin
  var x : integer{0..31};
  (-, x) = Foo() as (Named, integer{0..31});
  return 0;
end;
