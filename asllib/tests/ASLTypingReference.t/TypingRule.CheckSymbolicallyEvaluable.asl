func plus_mul{N, M}(x: integer{N}, y: integer{M}) => integer{N + N * M}
begin
    return x + x * y;
end;

constant SEVEN = 7;

type Data of bits(128) {
    [plus_mul{SEVEN, 4}(SEVEN, 4):0] data
};

func foo{N: integer{0..plus_mul{SEVEN, 4}(SEVEN, 4)}}(p: bits(N))
begin
    pass;
end;

func main() => integer
begin
    var d : Data;
    d.data[34:0] = Zeros{35};
    assert 35 IN { plus_mul{SEVEN, 4}(SEVEN, 4) };
    var x : bits(plus_mul{SEVEN, 4}(SEVEN, 4)) = Zeros{plus_mul{SEVEN, 4}(SEVEN, 4)};
    var arr: array[[plus_mul{SEVEN, 4}(SEVEN, 4)]] of integer;
    return 0;
end;
