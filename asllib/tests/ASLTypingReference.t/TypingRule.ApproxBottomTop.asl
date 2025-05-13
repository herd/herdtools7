func main() => integer
begin
    let a: integer{1..10} = ARBITRARY: integer{1..10};
    let b: integer{5..20} = ARBITRARY: integer{5..20};
    var x : integer{a..b} = a;
    return 0;
end;
