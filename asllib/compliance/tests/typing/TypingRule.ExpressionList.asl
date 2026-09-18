func main() => integer
begin
    update(10, 20.0);
    return 0;
end;

var X: integer;
var Y: real;

func update(x: integer, y: real)
begin
    X = X + x;
    Y = Y + y;
end;
