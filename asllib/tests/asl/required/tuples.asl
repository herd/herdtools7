

func f() => (integer, integer, integer)
begin
  return (3, 4, 5);
end

func multiple_return_values ()
begin
  let (a, b, c) = f();
  assert a == 3;
  assert b == 4;
  assert c == 5;
end

func other_tuple_usages ()
begin
  let t = f();
  let (a, b, c) = t;
  assert a == 3;
  assert b == 4;
  assert c == 5;
end

func main()
begin
  multiple_return_values();
  // other_tuple_usages();
end
