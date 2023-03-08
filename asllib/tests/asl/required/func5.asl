
func f(i :: integer) => integer
begin
  return i + 2;
end

func f(b :: boolean) => boolean
begin
  return if b then FALSE else TRUE;
end

func f(x :: bits(3)) => boolean
begin
  return x[0] == '0';
end

func main()
begin
  assert f(0) == 2;
  assert f(1) == 3;
  assert f(FALSE);
  assert f('110');
end

