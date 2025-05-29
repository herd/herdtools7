func main() => integer
begin
    var x: integer{4, 5, 6..10, 6..11, 5..10};
    var y = 2;
    var z: integer{-} = x DIV y;
    z = 2;
    return 0;
end;
