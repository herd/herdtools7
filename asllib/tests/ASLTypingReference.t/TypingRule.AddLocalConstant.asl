func foo(x: integer {0..100}) => integer {0..100}
begin
  return x;
end;

func main() => integer
begin
  constant x = 32;
  constant z: integer {0..100} = foo(x);
  // The static environment binds z to 32.

  let bv: bits(32) = Zeros{z};
  return 0;
end;
