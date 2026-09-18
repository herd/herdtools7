constant WORD_SIZE = 64;

type MyRecord of record {
    data: bits(WORD_SIZE),
    c: Color
};

pragma pragma1;

type Color of enumeration { RED, GREEN, BLUE };

func main() => integer
begin
    var x = g;
    println rotate0(x.c, 10);
    return 0;
end;

func rotate_color(c: Color) => Color
begin
    case c of
        when RED => return GREEN;
        when GREEN => return BLUE;
        when BLUE => return RED;
    end;
end;

func rotate0(c: Color, rotate: integer) => Color recurselimit 100
begin
    if (rotate > 2) then
        return rotate1(rotate_color(c), rotate - 1);
    else
        return c;
    end;
end;

func rotate1(c: Color, rotate: integer) => Color recurselimit 100
begin
    if (rotate > 2) then
        return rotate0(rotate_color(c), rotate - 1);
    else
        return c;
    end;
end;

var g : MyRecord;
