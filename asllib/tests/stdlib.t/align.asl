func main () => integer
begin
  assert AlignDownP2('110111', 1) == '110110';
  assert AlignDownP2('110111', 2) == '110100';
  assert AlignDownP2('110111', 3) == '110000';
  assert AlignDownP2('110111', 4) == '110000';
  assert AlignDownP2('110111', 5) == '100000';
  assert AlignDownP2('110111', 6) == '000000';
  assert AlignDownP2('001000', 1) == '001000';
  assert AlignDownP2('001000', 2) == '001000';
  assert AlignDownP2('001000', 3) == '001000';
  assert AlignDownP2('001000', 4) == '000000';
  assert AlignDownP2('001000', 5) == '000000';
  assert AlignDownP2('001000', 6) == '000000';
  assert AlignUpP2('110111', 1) == '111000';
  assert AlignUpP2('110111', 2) == '111000';
  assert AlignUpP2('110111', 3) == '111000';
  assert AlignUpP2('110111', 4) == '000000';
  assert AlignUpP2('110111', 5) == '000000';
  assert AlignUpP2('110111', 6) == '000000';
  assert AlignUpP2('001000', 1) == '001000';
  assert AlignUpP2('001000', 2) == '001000';
  assert AlignUpP2('001000', 3) == '001000';
  assert AlignUpP2('001000', 4) == '010000';
  assert AlignUpP2('001000', 5) == '100000';
  assert AlignUpP2('001000', 6) == '000000';

  for N = 0 to 5 do
    let pN = 2 ^ N;
    for x = -pN to pN do
      for y = 0 to N do
        let bv = x[0+:N];
        let p = 2^y as integer {1..2^N};

        assert AlignUpP2(bv, y) == AlignUpP2(x, y)[0+:N];
        assert AlignDownP2(bv, y) == AlignDownP2(x, y)[0+:N];

        assert AlignUpP2(x, y) IN {x..x+p};
        assert AlignDownP2(x, y) IN {(x - p)..x};

        assert AlignUpSize(x, p) == AlignUpP2(x, y);
        assert AlignDownSize(x, p) == AlignDownP2(x, y);

        assert AlignUpSize(bv, p) == AlignUpP2(bv, y);
        assert AlignDownSize(bv, p) == AlignDownP2(bv, y);
      end;
    end;
  end;

  return 0;
end;
