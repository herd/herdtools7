func main() => integer
begin
  assert Len('11') == 2;
  assert Len('110') == 3;
  assert Len('101') == 3;
  assert Len('') == 0;

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

