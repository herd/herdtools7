var X : integer;

func f() => integer{0..1000}
begin
    X = X + 1;
    return ARBITRARY: integer{0..1000};
end;

func g() => integer
begin
    X = X + 1;
    return ARBITRARY: integer;
end;

func getWid() => integer{3, 7}
begin
    return ARBITRARY: integer{3, 7};
end;

func Bitslices()
begin
    let offset = f();
    let k: integer{3, 7} = getWid();
    var src: bits(k);
    var dst: bits(k - 1);
    dst = src[(offset + k) -2:offset]; // Legal
    // but requires an execution-time check that
    // offset+k-2 < k
    // offset >= 0
    dst = src[offset +: k -1]; // Legal
    // but requires an execution-time bounds check that
    // offset+k-1 <= k
    // offset >= 0

    let w = offset;
    dst[0+:w] = src[0 +: w];
    // Legal but requires an execution-time bounds check that:
    // max index of LHS bitslice <= max index of dst
    // and max index of RHS bitslice <= max index of src.

    let zw: integer{0, 1, 2} = ARBITRARY: integer{0, 1, 2};
    var zb = f()[0 +: zw];
    let d = f()[1+:zw] :: g()[1 +: 8];
end;
