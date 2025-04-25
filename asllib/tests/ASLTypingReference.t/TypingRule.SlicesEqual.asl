func main() => integer
begin
    var bv: bits(64);
    bv[5, 4, 3, 2, 1, 0, 3 *: 4] = bv[:6, 15:12];
    return 0;
end;
