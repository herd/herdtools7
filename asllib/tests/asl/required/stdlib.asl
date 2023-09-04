// Extra main
func main() => integer
begin
  assert Abs (-1) == 1;
  assert Abs (2) == 2;

  assert Min (2, 3) == 2;
  assert Max (2, 3) == 3;

  assert Replicate('01',3) == '010101';
  assert Zeros(3) == '000';
  assert Zeros(8) == '00000000';

  assert Ones(3) == '111';
  assert IsZero(Zeros(2));
  assert IsOnes(Ones(3));
  assert ! IsZero(Ones(3));
  assert ! IsOnes(Zeros(3));
  assert ! IsZero ('101');

  assert SignExtend('100', 5) == '11100';
  assert ZeroExtend('100', 5) == '00100';

  assert Len('1010') == 4;

  assert UInt('110') == 6;
  assert UInt('') == 0;
  assert UInt('100000000') == 0x100;

  assert SInt('110') == -2;
  assert SInt('010') == 2;
  assert SInt('111') == -1;
  assert SInt('000') == 0;
  assert SInt('0') == 0;
  assert SInt('1') == -1;

  for n = 0 to 25 do
    assert Log2(2 ^ n) == n;
  end

  assert ((1 << 1)  == 2);
  // assert ((2 << -1) == 1);

  for m = -100 to 100 do
    let q = Real (m);
    assert RoundUp (q) == m;
    assert RoundDown (q) == m;
    assert RoundTowardsZero (q) == m;
  end

  for m = -100 to 100 do
    let q = Real (m) / 3.0;
    assert RoundDown (q) == m DIVRM 3;
  end

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

