var g : integer = 0;

func main() => integer
begin
    var x = 15;
    var y = x + 9;

    for i = x to y do
        g = g + 1;
    end;
    return 0;
end;
