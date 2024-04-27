func main() => integer
begin
  var a = '101';
  let b = a[1, 0];
  a[0] = '0';

  assert a == '100';
  assert b == '01';

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

