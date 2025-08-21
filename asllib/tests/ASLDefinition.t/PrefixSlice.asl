func main() => integer
begin
    var x: bits(8) = ARBITRARY: bits(8);
    assert x[:5] == x[4:0];
    return 0;
end;
