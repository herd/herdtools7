func main () => integer
begin
  for m = -100 to 100 do
    let q = Real (m);
    assert RoundUp (q) == m;
    assert RoundDown (q) == m;
    assert RoundTowardsZero (q) == m;
  end;

  for m = -100 to 100 do
    for n = 1 to 10 do
      let q = Real(m) / Real(n);
      let d = m DIVRM n;
      let round_up = RoundUp(q);
      let round_down = RoundDown(q);

      assert round_down == d;

      assert d <= round_up;
      assert round_up <= d + 1;

      if m >= 0 then
        assert RoundTowardsZero(q) == round_down;
      else
        assert RoundTowardsZero(q) == round_up;
      end;
    end;
  end;

  return 0;
end;

