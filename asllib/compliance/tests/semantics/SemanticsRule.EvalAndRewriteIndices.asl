var values : array[[2]] of integer;

readonly func selected_index() => integer
begin
    return values[[0]] MOD 2;
end;

func main() => integer
begin
    values[[0]] = 0;
    values[[1]] = 0;

    // Both indices are evaluated before either array element is updated.
    (values[[0]], values[[selected_index()]]) = (1, 2);

    // selected_index() returned 0. If it had instead been evaluated after the
    // first update, it would have returned 1 and the array would contain [1, 2].
    assert values[[0]] == 2;
    assert values[[1]] == 0;
    return 0;
end;
