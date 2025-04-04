func factorial(n: integer) => integer recurselimit 11
begin
    return if n == 0 then 1 else n * factorial(n - 1);
end;

func main() => integer
begin
    assert factorial(10) == 3628800;
    return 0;
end;
