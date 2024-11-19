func foo {M}() => (bits (M), boolean)
begin
  return (Zeros{M}, TRUE);
end;

func bar {B,A} (bv: bits(A)) => bits (B)
begin
  let (result, b) = foo {B};
  return result;
end;

func main () => integer
begin
  let x = bar{5,4}('1010');
  let y = bar{4,4}('0101');

  return 0;
end;
