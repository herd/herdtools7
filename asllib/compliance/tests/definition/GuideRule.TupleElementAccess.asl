func main() => integer
begin
    var a = (1, 2).item0; // a = 1;
    var b = (1, 2).item1; // b = 2;
    // the above 2 statements are equivalent to the following multi-assign statement.
    (a, b) = (1, 2);

    var x : (integer, integer) = (5, 6);
    assert x.item0 == 5 && x.item1 == 6;
    x = (x.item1, x.item0);
    assert x.item0 == 6 && x.item1 == 5;
    x = (x.item1, x.item1);
    assert x.item0 == 5 && x.item1 == 5;
    return 0;
end;
