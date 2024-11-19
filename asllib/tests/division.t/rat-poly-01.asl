func foo {N} (x: bits(N)) => bits(N)
begin
  let y = Zeros{2 * ((N DIV 2) - 5)};
  let z = Zeros{10};
  return [y, z];
end;

func main () => integer
begin
  let a = foo{20}(Zeros{20});
  let b = foo{10}(Zeros{10});
  let c = foo{8}(Zeros{8});
  let d = foo{9}(Zeros{9});

  return 0;
end;
