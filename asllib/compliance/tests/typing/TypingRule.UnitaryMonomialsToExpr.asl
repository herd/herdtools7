func main() => integer
begin
                        // the expressions corresponding
                        // to the rhs monomials
    var x : integer;
    var y : integer;
    - = x ^ 0 * y;      // y
    - = x ^ 1 * y;      // x * y
    - = x ^ 2 * y;      // x * x * y
    - = x ^ 3 * y;      // x^3 * y
    return 0;
end;
