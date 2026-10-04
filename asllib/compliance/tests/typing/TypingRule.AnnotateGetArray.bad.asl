var calls : integer = 0;

func next_index() => integer
begin
    calls = calls + 1;
    return 0;
end;

func main() => integer
begin
    var values : array[[1]] of integer;
    return values[[next_index()]];
end;
