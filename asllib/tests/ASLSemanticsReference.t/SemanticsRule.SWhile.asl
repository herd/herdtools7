pure func limit_loop() => integer{4}
begin
  println("evaluated limit = ", 4);
  return 4;
end;

func test_condition(i: integer) => boolean
begin
  let limit = 3;
  println("testing ", i, " <= ", limit);
  return i <= limit;
end;

func main () => integer
begin
  var i: integer = 0;
  while test_condition(i) looplimit limit_loop() do
    println("i = ", i);
    assert i <= 3;
    i = i + 1;
   end;
  return 0;
end;
