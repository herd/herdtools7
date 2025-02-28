func ModReal (x: real, y: real) => real
begin
  assert y > 0.0;
  return x - Real(RoundDown(x / y)) * y;
end;

func SqrtRoundedCorrect(value : real, fracbits : integer, sqrt : real) => boolean
begin
  if value == 0.0 then return  sqrt == 0.0; end;
  if  sqrt == 0.0 then return value == 0.0; end;

  var exp   : integer = ILog2(sqrt);
  var mant  : real = sqrt / (2.0 ^ exp);
  var ulp   : real = 2.0 ^ (- fracbits);
  var ulpx2 : real = 2.0 ^ (1-fracbits);
  var decr  : real = (2.0 ^ exp) * (mant - ulp);
  var incr  : real = (2.0 ^ exp) * (mant + ulp);

  return ( 1.0 <= mant ) &&
         ( mant < 2.0 ) &&
         ( sqrt == (2.0 ^ exp) * mant ) &&
         ( ModReal(mant, ulp) == 0.0 ) &&
         ( ( sqrt * sqrt == value ) || ( ModReal(mant, ulpx2) == ulp ) ) &&
         ( (decr * decr) < value ) &&
         ( (incr * incr) > value );
end;

func main () => integer
begin
  // Zero
  assert SqrtRounded(0.0, 1  ) == 0.0;
  assert SqrtRounded(0.0, 10 ) == 0.0;
  assert SqrtRounded(0.0, 100) == 0.0;

  // Exact values
  // Needs fracbits >= nb bits in the integer, here we take fracbits == n
  let n = 10;
  let m = 5;
  for i = 0 to 2^n do
    let p = Real(i) * (2.0 ^ (-m));
    assert SqrtRounded(p * p, n) == p;
  end;

  // Approximation checks on integers
  for i = 1 to 100 do
    let x = Real(i);
    let expected_res = SqrtRounded(x, 1000);
    for p = 1 to 10 do
      let res = SqrtRounded(x, p);
      assert Abs(res - expected_res) / expected_res < 2.0 ^ (-p);
    end;
  end;

  // Use exact checking with SqrtRoundedCorrect
  for l = 0 to 4 do
    for i = 2^l to 2^(l + 1) - 1 do
      let p = Real(i);
      assert SqrtRoundedCorrect(p, 1, SqrtRounded(p, 1));
      assert SqrtRoundedCorrect(p, 2, SqrtRounded(p, 2));
      assert SqrtRoundedCorrect(p, 3, SqrtRounded(p, 3));
      assert SqrtRoundedCorrect(p, 4, SqrtRounded(p, 4));
      assert SqrtRoundedCorrect(p, 100, SqrtRounded(p, 100));

      let q = p * (2.0 ^ (-400));
      assert SqrtRoundedCorrect(q, 1, SqrtRounded(q, 1));
      assert SqrtRoundedCorrect(q, 2, SqrtRounded(q, 2));
      assert SqrtRoundedCorrect(q, 3, SqrtRounded(q, 3));
      assert SqrtRoundedCorrect(q, 4, SqrtRounded(q, 4));
      assert SqrtRoundedCorrect(q, 100, SqrtRounded(q, 100));
    end;
  end;

  for l = 4 to 10 do
    for i = 2^l to 2^(l + 1) - 1 do
      let l2 = l DIVRM 2;

      let p = Real(i);
      assert SqrtRoundedCorrect(p, 1, SqrtRounded(p, 1));
      assert SqrtRoundedCorrect(p, 2, SqrtRounded(p, 2));
      assert SqrtRoundedCorrect(p, l2 - 1, SqrtRounded(p, l2 - 1));
      assert SqrtRoundedCorrect(p, l2, SqrtRounded(p, l2));
      assert SqrtRoundedCorrect(p, l2 + 1, SqrtRounded(p, l2 + 1));
      assert SqrtRoundedCorrect(p, l - 1, SqrtRounded(p, l - 1));
      assert SqrtRoundedCorrect(p, l, SqrtRounded(p, l));
      assert SqrtRoundedCorrect(p, l + 1, SqrtRounded(p, l + 1));
      assert SqrtRoundedCorrect(p, 100, SqrtRounded(p, 100));

      let q = p * (2.0 ^ (-400));
      assert SqrtRoundedCorrect(q, 1, SqrtRounded(q, 1));
      assert SqrtRoundedCorrect(q, 2, SqrtRounded(q, 2));
      assert SqrtRoundedCorrect(q, l2 - 1, SqrtRounded(q, l2 - 1));
      assert SqrtRoundedCorrect(q, l2, SqrtRounded(q, l2));
      assert SqrtRoundedCorrect(q, l2 + 1, SqrtRounded(q, l2 + 1));
      assert SqrtRoundedCorrect(q, l - 1, SqrtRounded(q, l - 1));
      assert SqrtRoundedCorrect(q, l, SqrtRounded(q, l));
      assert SqrtRoundedCorrect(q, l + 1, SqrtRounded(q, l + 1));
      assert SqrtRoundedCorrect(q, 100, SqrtRounded(q, 100));
    end;
  end;

  return 0;
end;

