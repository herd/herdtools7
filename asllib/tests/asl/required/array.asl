type a of array [4] of integer;

var global_a: a;

func get_2(local_a: a) => integer
begin
  return local_a[2];
end

func main () => integer
begin
  global_a [1] = 3;
  assert global_a[1] == 3;

  var local_a: a;
  local_a[2] = 5;
  assert get_2(local_a) == 5;

  return 0;
end

