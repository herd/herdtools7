func f() => integer
begin
  return 0;
end

func f(x:integer) => integer
begin
  return x;
end

func f(x:integer, y:integer) => integer
begin
  return x + y;
end

func main() => integer
begin
  assert 0 == f();
  assert 1 == f(1);
  assert 5 == f(2, 3);

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

