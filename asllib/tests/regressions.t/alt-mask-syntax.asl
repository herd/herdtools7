func main() => integer
begin
  // Loop over all bits(4) bit vectors
  for i = 0 to 2^4 - 1 do
    let bv = i[:4];

    assert bv == '1(0)(0)1' <-> bv IN {'1xx1'};
    assert bv == '1(0)(1)1' <-> bv IN {'1xx1'};
    assert bv == '1(1)(0)1' <-> bv IN {'1xx1'};
    assert bv == '1(1)(1)1' <-> bv IN {'1xx1'};

    assert bv == '1(00)1' <-> bv IN {'1xx1'};
    assert bv == '1(01)1' <-> bv IN {'1xx1'};
    assert bv == '1(10)1' <-> bv IN {'1xx1'};
    assert bv == '1(11)1' <-> bv IN {'1xx1'};

    assert bv != '0(0)1(0)' <-> !(bv IN {'0x1x'});
    assert bv != '0(0)1(1)' <-> !(bv IN {'0x1x'});
    assert bv != '0(1)1(0)' <-> !(bv IN {'0x1x'});
    assert bv != '0(1)1(1)' <-> !(bv IN {'0x1x'});

    assert bv IN {'1(0)(0)1', '0(1)1(1)'} <-> bv IN {'1xx1', '0x1x'};
    assert bv IN {'1(0)(1)1', '0(1)1(0)'} <-> bv IN {'1xx1', '0x1x'};

    assert bv == '0(1   0)1' <-> bv IN {'0xx1'};

    assert bv == '1 xx (0)' <-> bv IN {'1xxx'};
  end;

  return 0;
end;
