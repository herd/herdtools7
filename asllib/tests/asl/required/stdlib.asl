// Extra main
func main()
begin
  assert Abs (-1) == 1;
  assert Abs (2) == 2;

  assert Min (2, 3) == 2;
  assert Max (2, 3) == 3;

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
end

