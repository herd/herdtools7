func foo {N} (x: bits(N)) => bits(N)
begin
  let y = Zeros(2 * ((N DIV 2) - 5));
  let z = Zeros(10);
  return y :: z;
end

func main () => integer
begin
  let a = foo(Zeros(20));
  let b = foo(Zeros(10));
  let c = foo(Zeros(8));
  let d = foo(Zeros(9));

  return 0;
end
