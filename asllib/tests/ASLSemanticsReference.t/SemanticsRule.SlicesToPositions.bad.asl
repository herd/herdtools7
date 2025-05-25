func main() => integer
begin
    var bv = '1010 0011';
    var from = -1;
    assert bv[from+:6] == '001 101';
    return 0;
end;
