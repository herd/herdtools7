

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

func with_var ()
begin
  var a, b, c: integer;
  (a, b, c) = f();
  assert a == 3;
  assert b == 4;
  assert c == 5;
end

func main() => integer
begin
  multiple_return_values();
  other_tuple_usages();
  with_var ();

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

