func Foo{N}(x: integer) => bits(N)
begin
  return Zeros{N};
end;

func bad_elide_parameter()
begin
  let - : bits(4) = Foo{,3}(0);
end;

func main() => integer
begin
  return 0;
end;
