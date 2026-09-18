pure func static_func{N}(x: integer{N}) => integer{N}
begin
    return x;
end;

type Data of bits(128) {
    // Expressions in bitfield slices should be statically evaluable.
    // `symbolic{4}(4)` is statically evaluated to 4.
    [static_func{4}(4):0] data
};

func foo{N}(bv: bits(N)) => bits(N)
begin
    var res = bv;
    // Expressions in assignable slices need not be statically evaluable.
    // `N DIV 2` is normalized into itself.
    res[N DIV 2:0] = Zeros{N DIV 2 + 1};
    return res;
end;

func main() => integer
begin
    var bv : bits(128);
    - = foo{128}(Ones{128});
    return 0;
end;
