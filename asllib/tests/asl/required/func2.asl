getter X_get[i::integer] => integer
begin
    return i;
end

setter X_set[i::integer] = v::integer
begin
    internal_i = i;
    internal_v = v;
end

func main()
begin
    X_set[2] = 3;
    x = X_get[4];

    assert x == 4;
end
