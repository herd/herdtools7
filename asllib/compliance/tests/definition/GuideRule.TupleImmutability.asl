func main() => integer
begin
    var x : (integer, boolean) = (5, TRUE);
    // We can change the value of 'x'.
    x = (6, TRUE);
    // But we cannot change the tuple value stored in 'x':
    x.item1 = '1'; // Illegal: tuples are immutable.
    return 0;
end;
