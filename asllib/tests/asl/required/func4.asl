func f() => integer
begin
  return 0;
end

func f(x::integer) => integer
begin
  return x;
end

func f(x::integer, y::integer) => integer
begin
  return x + y;
end

func main()
begin
  assert 0 == f();
  assert 1 == f(1);
  assert 5 == f(2, 3);
end
