// Underlying storage element, R
var R : array [[32]] of bits(64);

// Accessor, X
accessor X(regno: integer{0..31}) <=> value: bits(64)
begin
  getter
    if regno == 31 then
      return Zeros{64};
    else
      return R[[regno]];
    end;
  end;

  setter
    if regno != 31 then
      R[[regno]] = value;
    end;
  end;
end;

