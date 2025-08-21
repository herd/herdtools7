func main() => integer
begin
    var x: bit;
    x[0] = NOT x[0];
    assert x[0] == '1';
    return 0;
end;
