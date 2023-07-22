
var x :: integer = 0;

func incr ()
begin
  x = x + 1;
end

func incr2 () => integer
begin
  let y = x;
  x = y + 1;
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
  assert x == 3;

  assert incr2 () == 3;
  assert 2 + incr2 () * 4 == 18;
  // assert add (incr2 (), 3) == 8;
  assert x == 5;

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

