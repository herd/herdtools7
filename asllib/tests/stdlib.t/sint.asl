func test_sint {N} (bv: bits(N))
begin
  for i = 0 to 1 << N - 1 do
    assert SInt ('0' :: i[N-1:0]) == i;
    assert SInt ('1' :: i[N-1:0]) == i - (1 << N);
  end;
end;

// Same as test_sint, but only test 2^m numbers on each side of the interval
func test_big_sint {N} (bv: bits(N), m: integer {0..N})
begin
  for i = 0 to 1 << m do
    assert SInt ('0' :: i[N-1:0]) == i;
    assert SInt ('1' :: i[N-1:0]) == i - (1 << N);
  end;
  for i = 1 << ( N DIVRM 2 ) - 1 << m
       to 1 << ( N DIVRM 2 ) + 1 << m do
    assert SInt ('0' :: i[N-1:0]) == i;
    assert SInt ('1' :: i[N-1:0]) == i - (1 << N);
  end;
  for i = 1 << N - 1 << m to 1 << N - 1 do
    assert SInt ('0' :: i[N-1:0]) == i;
    assert SInt ('1' :: i[N-1:0]) == i - (1 << N);
  end;
end;

func main () => integer
begin
  assert SInt('110') == -2;
  assert SInt('010') == 2;
  assert SInt('111') == -1;
  assert SInt('000') == 0;
  assert SInt('0') == 0;
  assert SInt('1') == -1;
  assert SInt('') == 0;

  for N = 0 to 10 do
    test_sint{N}(Zeros{N});
  end;

  test_big_sint{15}(Zeros{15}, 5);
  test_big_sint{16}(Zeros{16}, 5);
  test_big_sint{17}(Zeros{17}, 5);
  test_big_sint{31}(Zeros{31}, 5);
  test_big_sint{32}(Zeros{32}, 5);
  test_big_sint{33}(Zeros{33}, 5);
  test_big_sint{63}(Zeros{63}, 5);
  test_big_sint{64}(Zeros{64}, 5);
  test_big_sint{65}(Zeros{65}, 5);
  test_big_sint{127}(Zeros{127}, 5);
  test_big_sint{128}(Zeros{128}, 5);
  test_big_sint{129}(Zeros{129}, 5);
  test_big_sint{255}(Zeros{255}, 5);
  test_big_sint{256}(Zeros{256}, 5);
  test_big_sint{257}(Zeros{257}, 5);

  return 0;
end;

