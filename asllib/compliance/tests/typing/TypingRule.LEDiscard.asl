func increment(x: integer) => integer
begin
    return x + 1;
end;

func main() => integer
begin
    - = 42;
    - = increment(42);
    var (-, x) = (42, 43);
    return 0;
end;
