func main() => integer
begin
    var arr: array[[3]] of integer;
    arr[[1]] = 3;
    assert arr[[1]] == 3;

    assert (5, 7).item0 == 5;

    - = (5, 7) as (integer, integer);
    return 0;
end;
