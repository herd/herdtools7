func main()
begin
  var a = '101';
  let b = a[1, 0];
  a[0] = '0';

  assert a == '100';
  assert b == '01';
end

