func main() => integer
begin
    // Illegal: only global constant storage elements are allowed.
    constant c3 = 5;
    return 0;
end;
