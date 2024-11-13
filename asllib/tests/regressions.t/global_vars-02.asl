var x : integer = 5;
 
var y = x as integer{1..7};
 
func main() => integer
begin
    assert y == 5;

    return 0;
end;
