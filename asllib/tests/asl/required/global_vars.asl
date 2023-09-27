
var global_x : integer = 0;

func incr ()
begin
  global_x = global_x + 1;
end

func incr2 () => integer
begin
  let y = global_x;
  global_x = y + 1;
  return y;
end

func add (x : integer, y: integer) => integer
begin
  return x + y;
end

func main () => integer
begin
  incr ();
  incr ();
  incr ();
  assert global_x == 3;

  assert incr2 () == 3;
  assert 2 + incr2 () * 4 == 18;
  // assert add (incr2 (), 3) == 8;
  assert global_x == 5;

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

