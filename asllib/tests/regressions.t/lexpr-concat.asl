
func main () => integer
begin
  var a, b, c, d : bits(2);
  for i = 0 to 0xff do
    let to_test = i[7:0];
    [a, b, c, d] = to_test;
    assert [a, b, c, d] == to_test;
    [a, [c, b], d] = to_test;
    assert [a, c, b, d] == to_test;
  end

  return 0;
end

