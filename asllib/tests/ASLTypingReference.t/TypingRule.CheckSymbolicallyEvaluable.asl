func plus_mul{N, M}(x: integer{N}, y: integer{M}) => integer{N + N * M}
begin
    return x + x * y;
end;

constant SEVEN = 7;

type Data of bits(128) {
    [plus_mul{SEVEN, 4}(SEVEN, 4):0] data
};
