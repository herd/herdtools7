pure func factorial(x : integer) => integer
begin
    assert x >= 0;
    var res : integer = 1;
    for i = 1 to x do
        res = res * i;
    end;
    return res;
end;

constant y = factorial(7);

func main() => integer
begin
    var x = y;
    println(y);
    return 0;
end;
