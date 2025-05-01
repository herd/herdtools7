type Color of enumeration { RED, GREEN, BLUE };

func main() => integer
begin
    var x : array[[3]] of integer;
    var y : array[[Color]] of integer;
    // Illegal as integer and enumeration are different
    // kinds of indices.
    x = y;

    return 0;
end;
