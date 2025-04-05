func symbolic{N}(x: integer{N}) => integer{N}
begin
    return x;
end;

type Data of bits(128) {
    [symbolic{4}(4)] data
};
