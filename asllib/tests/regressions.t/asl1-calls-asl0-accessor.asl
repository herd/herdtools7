func main() => integer
begin
    - = ZeroExtend{64}(Foo());
    Foo() = Zeros{32};
    Foo()[0] = '1';
    return 0;
end;
