type Color of enumeration { GREEN, ORANGE, RED };

func rotate_color(c : Color) => Color
begin
    if c == GREEN then
        return ORANGE;
    end;
    if c == ORANGE then
        return RED;
    else // c == RED
        return GREEN;
    end;
end;
