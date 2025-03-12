func main() => integer
begin
    var x : (integer, integer) = (5, 6);
    assert x.item0 == 5 && x.item1 == 6;
    x = (x.item1, x.item0);
    assert x.item0 == 6 && x.item1 == 5;
    x = (x.item1, x.item1);
    assert x.item0 == 5 && x.item1 == 5;
    // The following statement in comment is illegal
    // as item2 is not an element of the tuple stored in 'x'.
    // x = (x.item1, x.item2);
    return 0;
end;
