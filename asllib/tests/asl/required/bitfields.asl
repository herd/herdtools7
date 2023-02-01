
type MyBitVector of bits (5) {
  [2:0] first_three,
  [0  ] first,
  [4:0] everything,
  [0, 1, 2, 3, 4] reversed,
  [1:0, 2, 4:3] swapped,
};

func build_one() => MyBitVector
begin
  return '10111';
end

func set_first(bv::MyBitVector, b::bits(1)) => MyBitVector
begin
  bv.first = b;
  return bv;
end

func get_first_three(bv::MyBitVector) => bits(3)
begin
  return bv.first_three;
end

func as_MyBitVector(x :: bits(5)) => MyBitVector
begin
  return x;
end

func main ()
begin
  let bv = build_one ();

  assert set_first(bv, '0') == '10110';
  assert set_first(set_first(bv, '1'), '0') == '10110';
  assert get_first_three('11100') == '100';
  assert get_first_three('11101') == '101';

  assert as_MyBitVector('10010').everything == '10010';
  assert as_MyBitVector('10010').reversed == '01001';
  assert as_MyBitVector('10010').swapped == '10010';
  assert as_MyBitVector('11010').swapped == '10011';
end
