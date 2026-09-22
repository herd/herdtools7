var COUNT: integer;

func ColdReset()
begin
    COUNT = 0;
end;

func Step()
begin
    assert COUNT >= 0;
    COUNT = COUNT + 1;
    assert COUNT > 0;
end;
