pure func f{x}() => integer{x*8}
begin
    return x * 8;
end;

type Color of enumeration { RED, GREEN, BLUE };

func main() => integer
begin
    let z = ARBITRARY: integer{0..1000};
    var x1 : array[[z+z]] of integer;
    var y1 : array[[2*z]] of integer;
    // Legal as `z+z` is equivalent to `2*z`.
    x1 = y1;

    var x2 : array[[f{z+z}]] of integer;
    var y2 : array[[f{2*z}]] of integer;
    // Legal as `f(z+z)` is equivalent to `f(2*z)`.
    x2 = y2;

    var x3 : array[[Color]] of integer;
    var y3 : array[[Color]] of integer;
    // Legal as the same enumeration is used.
    x3 = y3;

    var x4 = 4 IN {1, 2, 3, 4, 5};
    var y4 = 4 IN {5, 2, 3, 4, 1};
    x4 = y4;

    return 0;
end;
