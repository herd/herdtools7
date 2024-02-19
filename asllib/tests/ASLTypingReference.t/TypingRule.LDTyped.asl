type MyT of integer;

func foo (t: MyT) => integer
begin
  return t as integer;
end

func main () => integer
begin
  let x: MyT = 42;
  var z: MyT;

  assert foo (x) == 42;
  assert foo (z) == 0;

  return 0;
end
