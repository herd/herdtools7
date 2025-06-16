func foo(a: integer) => integer recurselimit 4
begin
    return bar(a);
end;

func bar(b: integer) => integer recurselimit 3
begin
    return foo(b);
end;

func main() => integer
begin
    - = foo(1);
    return 0;
end;
