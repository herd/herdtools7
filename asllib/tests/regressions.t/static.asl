
constant C1 : integer = 3;
constant C2 : integer = C1 + 2;
constant C3 : integer = C4 * C2;
constant C4 : integer = C1;
constant C5 : integer = - C2;

constant C6 : bits(4) = 15[3:0];
constant C7 : bits(4) = 0xC[3:0];

constant C8 : integer = if C2 == C3 then 42 else 57;
constant C9 : boolean = C7 + 3 == C6;
constant C10 : integer = if !C9 then C1 else C5;

func main() => integer
begin
  assert C1 == 3;
  assert C2 == 5;
  assert C3 == 15;
  assert C4 == 3;
  assert C5 == -5;

  assert C6 == '1111';
  assert C7 == '1100';

  assert C8 == 57;
  assert C9;
  assert C10 == -5;

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

