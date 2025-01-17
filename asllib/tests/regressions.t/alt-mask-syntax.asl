func assert_same(b1: boolean, b2: boolean)
begin
  assert b1 == b2;
end;

func main() => integer
begin
  // Loop over all bits(4) bit vectors
  for i = 0 to 2^4 - 1 do
    let bv = i[:4];

    assert_same (bv == '1(0)(0)1', bv IN {'1xx1'});
    assert_same (bv == '1(0)(1)1', bv IN {'1xx1'});
    assert_same (bv == '1(1)(0)1', bv IN {'1xx1'});
    assert_same (bv == '1(1)(1)1', bv IN {'1xx1'});

    assert_same (bv == '1(00)1', bv IN {'1xx1'});
    assert_same (bv == '1(01)1', bv IN {'1xx1'});
    assert_same (bv == '1(10)1', bv IN {'1xx1'});
    assert_same (bv == '1(11)1', bv IN {'1xx1'});

    assert_same (bv != '0(0)1(0)', !(bv IN {'0x1x'}));
    assert_same (bv != '0(0)1(1)', !(bv IN {'0x1x'}));
    assert_same (bv != '0(1)1(0)', !(bv IN {'0x1x'}));
    assert_same (bv != '0(1)1(1)', !(bv IN {'0x1x'}));

    assert_same (bv IN {'1(0)(0)1', '0(1)1(1)'}, bv IN {'1xx1', '0x1x'});
    assert_same (bv IN {'1(0)(1)1', '0(1)1(0)'}, bv IN {'1xx1', '0x1x'});
  end;

  return 0;
end;
