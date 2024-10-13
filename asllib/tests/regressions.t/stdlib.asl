func test_uint {N} (bv: bits(N))
begin
  for i = 0 to 1 << N do
    assert UInt (i[N:0]) == i;
  end
end

func test_sint {N} (bv: bits(N))
begin
  for i = 0 to 1 << N  - 1 do
    assert SInt (i[N:0]) == i;
    assert SInt ('1' :: i[N-1:0]) == i - 1 << N;
  end
end

// Extra main
func main() => integer
begin
  assert Abs (-1) == 1;
  assert Abs (2) == 2;

  assert Min (2, 3) == 2;
  assert Max (2, 3) == 3;

  assert Min (2.3, 3.3) == 2.3;
  assert Max (2.3, 3.3) == 3.3;

  assert IsEven (2);
  assert IsEven (0);
  assert IsEven (-2);
  assert !(IsEven (1));
  assert !(IsEven (-1));
  assert IsOdd (1);
  assert IsOdd (-1);

  assert Replicate('01',3) == '010101';
  assert Replicate('',3) == '';

  assert Zeros(0) == '';
  assert Zeros(3) == '000';
  assert Zeros(8) == '00000000';

  assert Ones(0) == '';
  assert Ones(3) == '111';
  assert Ones(8) == '11111111';

  assert IsZero(Zeros(2));
  assert IsOnes(Ones(3));
  assert ! IsZero(Ones(3));
  assert ! IsOnes(Zeros(3));
  assert ! IsZero ('101');

  assert SignExtend('100', 5) == '11100';
  assert ZeroExtend('100', 5) == '00100';
  assert Extend('100', 5, TRUE) == '00100';
  assert Extend('100', 5, FALSE) == '11100';

  assert Len('') == 0;
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

  test_uint (Zeros(0));
  test_uint (Zeros(1));
  test_uint (Zeros(2));
  test_uint (Zeros(3));

  test_sint (Zeros(0));
  test_sint (Zeros(1));
  test_sint (Zeros(2));
  test_sint (Zeros(3));

  for n = 0 to 25 do
    assert Log2(2 ^ n) == n;
  end

  assert ((1 << 1)  == 2);
  assert ((1 << 0)  == 1);
  assert (((-1) << 0) == -1);
  assert (((-1) << 1) == -2);

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

  for a = -100 to 100 do
    for b = 1 to 5 do
      assert a MOD b + (a DIVRM b) * b == a;
      assert (b * a) DIV b == a;
      if a MOD b == 0 then assert b * (a DIV b) == a; end
    end
  end

  for i = 1 to 10 do
    for p = 1 to 10 do
      let x = Real(i);
      let (res, inexact) = SqrtRoundDown(x, p);
      assert Abs(res * res - x) <= 1.0 / 2.0 ^ p;
      assert inexact || res * res == x;
    end
  end

  assert BitCount ('000') == 0;
  assert BitCount ('101') == 2;
  assert BitCount ('010') == 1;
  assert BitCount ('') == 0;

  assert LowestSetBit ('000') == 3;
  assert LowestSetBit ('101') == 0;
  assert LowestSetBit ('010') == 1;
  assert LowestSetBit ('') == 0;

  assert HighestSetBit ('000') == -1;
  assert HighestSetBit ('101') == 2;
  assert HighestSetBit ('010') == 1;
  assert HighestSetBit ('') == -1;

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

