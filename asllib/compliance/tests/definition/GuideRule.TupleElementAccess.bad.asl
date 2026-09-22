func main() => integer
begin
    var x : (integer, integer) = (5, 6);
    // Illegal: item2 is not an element of the tuple stored in 'x'.
    x = (x.item1, x.item2);
    return 0;
end;
