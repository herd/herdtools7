func main() => integer
begin
    var bv = '1010 0011';
    assert bv[1+:3, 7:5] == '001 101';
    return 0;
end;
