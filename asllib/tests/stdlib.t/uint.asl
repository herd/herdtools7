// Test UInt on all bitvectors of length N;
// bv is a phantom argument, only there to enable the parameter N
func test_uint {N} (bv: bits(N))
begin
  for i = 0 to 1 << N do
    assert UInt (i[N:0]) == i;
  end;
end;

// Same as test_uint, but only test 2^m numbers on each side of the interval
func test_big_uint {N} (bv: bits(N), m: integer {0..N})
begin
  for i = 0 to 1 << m do
    assert UInt (i[N:0]) == i;
  end;
  for i = 1 << N - 1 << m to 1 << N do
    assert UInt (i[N:0]) == i;
  end;
end;

func main () => integer
begin
  assert UInt('110') == 6;
  assert UInt('') == 0;
  assert UInt('100000000') == 0x100;

  for N = 0 to 10 do
    test_uint{N}(Zeros{N});
  end;

  test_big_uint{15}(Zeros{15}, 5);
  test_big_uint{16}(Zeros{16}, 5);
  test_big_uint{17}(Zeros{17}, 5);
  test_big_uint{31}(Zeros{31}, 5);
  test_big_uint{32}(Zeros{32}, 5);
  test_big_uint{33}(Zeros{33}, 5);
  test_big_uint{63}(Zeros{63}, 5);
  test_big_uint{64}(Zeros{64}, 5);
  test_big_uint{65}(Zeros{65}, 5);
  test_big_uint{127}(Zeros{127}, 5);
  test_big_uint{128}(Zeros{128}, 5);
  test_big_uint{129}(Zeros{129}, 5);
  test_big_uint{255}(Zeros{255}, 5);
  test_big_uint{256}(Zeros{256}, 5);
  test_big_uint{257}(Zeros{257}, 5);

  return 0;
end;
