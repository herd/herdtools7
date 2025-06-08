func main() => integer
begin
    for i = 12 to 0 do
        assert FALSE; // this line is never executed
        // the initial value (12) is greater than the end expression (0)
    end;
    return 0;
end;
