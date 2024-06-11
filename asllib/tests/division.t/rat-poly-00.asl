func FirstHalf {N} (bv: bits(N)) => bits (N DIV 2)
begin
  return bv[0+:N DIV 2];
end

func main () => integer
begin
  let a = FirstHalf(Ones(4));
  assert a == '11';

  let b = FirstHalf(Zeros(0));
  assert b == '';

  let c = FirstHalf(Zeros(7));
  assert c == '000';
  
  return 0;
end
