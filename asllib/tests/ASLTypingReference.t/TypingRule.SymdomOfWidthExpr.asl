func main() => integer
begin
    let x = ARBITRARY: integer{0..1000};
    var bv : bits(2 * x) = Zeros{x} :: Zeros{x};
    return 0;
end;
