type Rnum_X of integer{0..31};
type ElementSize of integer{8, 16, 32, 64, 128};

type ExtendType of enumeration
  {ExtendType_SXTB, ExtendType_SXTH, ExtendType_SXTW, ExtendType_SXTX,
  ExtendType_UXTB, ExtendType_UXTH, ExtendType_UXTW, ExtendType_UXTX};

getter X[n : Rnum_X, width : ElementSize] => bits(width)
begin
    assert width IN {8,16,32,64};
    return n[width-1:0];
end

func ExtendReg(reg : Rnum_X, exttype : ExtendType, shift : integer{0..4}, N : ElementSize) => bits(N)
begin
    assert shift >= 0 && shift <= 4;
    let val : bits(N) = X[reg, N];
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
    end

    let nbits = Min(len, N - shift) as integer{0..N};
    return Extend(val[0+:nbits] :: Zeros(shift), N, unsigned);
end

func CPositive12() => bits(8)
begin
    return ExtendReg(0, ExtendType_SXTH, 2, 8);
end
