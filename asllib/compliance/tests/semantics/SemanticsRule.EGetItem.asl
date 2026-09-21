func main() => integer
begin
    var t = (1, 2);
    assert t.item0 + t.item1 == 3;

    // The following statement in comment is illegal: item01 is treated
    // by the type system as a field.
    // let - = t.item01;
    return 0;
end;
