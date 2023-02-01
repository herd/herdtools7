getter f1[] => integer
begin
  return 3;
end

setter f1[] = v :: integer
begin
  assert v == 3;
end

getter f2[x::integer] => integer
begin
  return f1 + x;
end

setter f2[x::integer] = v :: integer
begin
  f1 = 3 * (v - x);
end

setter f3[x::integer] = v :: integer
begin
  assert x == 12;
  assert v == 13;
end

func main()
begin
  f1[] = f1[];
  f1 = f1;
  a = f1;
  assert a == 3;
  assert f1 == 3;
  b = f1[];
  assert b == 3;
  assert 3 == f1[];
  c = f2[4];
  assert c == 7;

  f2[5] = 6;
  f3[12] = 13;
end
