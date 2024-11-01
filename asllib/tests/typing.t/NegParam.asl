func test(N: integer, bv: bits(N+1))
begin
  let x: integer {0..N} = 0;
end;

func main () => integer
begin
  test(2, '000');
  test(-1, '');

  return 0;
end;
