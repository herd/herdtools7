func f(i::integer) => integer
begin
    return i;
end

func main()
begin
    x = 3;
    y = f(x);
    assert x == 3;
    assert y == 3;
end
