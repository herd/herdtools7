func increment(x: integer) => integer
begin
    return x + 1;
end;

var g : integer;

func main() => integer
begin
    var x : integer;
    var y : integer;
    x = 42;
    y = increment(42);
    g = increment(42);
    return 0;
end;
