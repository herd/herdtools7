type Color of enumeration {RED, GREEN, BLUE};

func main() => integer
begin
    var - : boolean = ARBITRARY : boolean;
    var - : real = ARBITRARY : real;
    var - : string = ARBITRARY : string;
    var - : integer = ARBITRARY : integer;
    var i : integer{-1000..1000} = ARBITRARY : integer{-1000..1000};
    assert -1000 <= i && i <= 1000;
    var e : Color = ARBITRARY : Color;
    assert e == RED || e == GREEN || e == BLUE;
    return 0;
end;
