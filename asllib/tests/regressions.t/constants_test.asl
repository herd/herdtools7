func foo(x: integer) => integer
begin
    return x;
end;

func main() => integer
begin
    constant x = 32;
    constant z = foo(x);
    println("z=", z);
    return 0;
end;
