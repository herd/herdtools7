type Rnum_X of integer{0..31};
type ElementSize of integer{8, 16, 32, 64, 128};

type ExtendType of enumeration
  {ExtendType_SXTB, ExtendType_SXTH, ExtendType_SXTW, ExtendType_SXTX,
  ExtendType_UXTB, ExtendType_UXTH, ExtendType_UXTW, ExtendType_UXTX};

accessor X{width : ElementSize}(n : Rnum_X) <=> value_in: bits(width)
begin
  getter
      assert width IN {8,16,32,64};
      return n[width-1:0];
  end;

  setter
    Unreachable();
  end;
end;

func ExtendReg{N : ElementSize}(reg : Rnum_X, exttype : ExtendType, shift : integer{0..4}) => bits(N)
begin
    assert shift >= 0 && shift <= 4;
    let val : bits(N) = X{}(reg);
    var unsigned : boolean;
    var len : ElementSize;

    case exttype of
        when ExtendType_SXTB => unsigned = FALSE; len = 8;
        when ExtendType_SXTH => unsigned = FALSE; len = 16;
        when ExtendType_SXTW => unsigned = FALSE; len = 32;
        when ExtendType_SXTX => unsigned = FALSE; len = 64;
        when ExtendType_UXTB => unsigned = TRUE;  len = 8;
        when ExtendType_UXTH => unsigned = TRUE;  len = 16;
        when ExtendType_UXTW => unsigned = TRUE;  len = 32;
        when ExtendType_UXTX => unsigned = TRUE;  len = 64;
    end;

    let nbits = Min(len, N - shift) as integer{0..N};
    return Extend{N}(val[0+:nbits] :: Zeros{shift}, unsigned);
end;

func CPositive12() => bits(8)
begin
    return ExtendReg{8}(0, ExtendType_SXTH, 2);
end;
