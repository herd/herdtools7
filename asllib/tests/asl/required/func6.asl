func Len{N::integer}(x :: bits(N)) => integer
begin
  return N;
end

func main()
begin
  assert Len('11') == 2;
  assert Len('110') == 3;
  assert Len('101') == 3;
  assert Len('') == 0;
end

