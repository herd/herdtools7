func main () => integer
begin
  assert HighestSetBit{0}('') == -1;
  assert LowestSetBit{0}('') == 0;
  assert HighestSetBit{1}('0') == -1;
  assert LowestSetBit{1}('0') == 1;
  assert HighestSetBit{1}('1') == 0;
  assert LowestSetBit{1}('1') == 0;

  for n = 2 to 10 do
    assert HighestSetBit{n}(Zeros{n}) == -1;
    assert LowestSetBit{n}(Zeros{n}) == n;

    // This enumerates all non-zeros bitvectors of length n
    var count: integer = 1; // 0 already done
    for p = 1 to n do
      let k0 = 2^(p-1);
      let k1 = 2 * k0 - 1;
      for k = k0 to k1 do
        let bv = k[0+:n];

        let q = HighestSetBit{n}(bv);
        assert q == p-1;
        assert bv[n-1:q] == (Zeros{n-q-1} :: '1');
        assert HighestSetBitNZ{n}(bv) == q;

        let m = LowestSetBit{n}(bv);
        assert m >= 0; // bv is not zero
        assert k MOD 2^(m+1) == 2^m;
        assert bv[m:0] == ('1' :: Zeros{m});
        assert LowestSetBitNZ{n}(bv) == m;

        count = count + 1;
      end;
    end;

    // check that we've enumerated all possible bitvectors
    assert count == 2^n;
  end;

  return 0;
end;
