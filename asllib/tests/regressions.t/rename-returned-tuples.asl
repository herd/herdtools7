func foo (M: integer) => (bits (M), boolean)
begin
  return (Zeros(M), TRUE);
end

func bar {A} (bv: bits(A), B: integer) => bits (B)
begin
  let (result, b) = foo (B);
  return result;
end

func main () => integer
begin
  let x = bar ('1010', 5);
  let y = bar ('0101', 4);

  return 0;
end
