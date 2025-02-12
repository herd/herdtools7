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

  assert Replicate{6}('01') == '010101';

  assert Zeros{0} == '';
  assert Zeros{3} == '000';
  assert Zeros{8} == '00000000';

  assert Ones{0} == '';
  assert Ones{3} == '111';
  assert Ones{8} == '11111111';

  assert IsZero(Zeros{2});
  assert IsOnes(Ones{3});
  assert ! IsZero(Ones{3});
  assert ! IsOnes(Zeros{3});
  assert ! IsZero ('101');

  assert SignExtend{5}('100') == '11100';
  assert ZeroExtend{5}('100') == '00100';
  assert Extend{5}('100', TRUE) == '00100';
  assert Extend{5}('100', FALSE) == '11100';

  assert Len('') == 0;
  assert Len('1010') == 4;

  assert ((1 << 1)  == 2);
  assert ((1 << 0)  == 1);
  assert (((-1) << 0) == -1);
  assert (((-1) << 1) == -2);

  for a = -100 to 100 do
    for b = 1 to 5 do
      assert a MOD b + (a DIVRM b) * b == a;
      assert (b * a) DIV b == a;
      if a MOD b == 0 then assert b * (a DIV b) == a; end;
    end;
  end;

  return 0;
end;


