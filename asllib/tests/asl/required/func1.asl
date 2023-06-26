func f(i::integer) => integer
begin
    return i;
end

func main()
begin
    let x = 3;
    let y = f(x);
    assert x == 3;
    assert y == 3;
end
