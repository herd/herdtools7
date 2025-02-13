func checked_8bit_add(a: integer, b: integer) => integer
begin
    assert a >= 0;
    assert b >= 0;
    assert a + b < 256;
    return a + b;
end;

func main() => integer
begin
    var x = checked_8bit_add(0, 255);
    x = checked_8bit_add(1, 255);
    return 0;
end;
