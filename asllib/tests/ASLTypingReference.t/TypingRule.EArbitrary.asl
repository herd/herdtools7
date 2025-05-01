type Color of enumeration {RED, GREEN, BLUE};

func main() => integer
begin
    var a : boolean = ARBITRARY : boolean;
    var b : real = ARBITRARY : real;
    var c : string = ARBITRARY : string;
    var d : integer = ARBITRARY : integer;
    var i : integer{-1000..1000} = ARBITRARY : integer{-1000..1000};
    assert -1000 <= i && i <= 1000;
    var e : Color = ARBITRARY : Color;
    assert e == RED || e == GREEN || e == BLUE;
    return 0;
end;
