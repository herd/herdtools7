type Color of enumeration {RED, GREEN, BLUE};

func main() => integer
begin
    var x : array[[Color]] of integer;
    x.RED = 42;
    return 0;
end;
