func main() => integer
begin
    // Illegal: bitfields of the same name must have the same slices.
    var x : bits(64) { [1] data } = Zeros{64} as bits(64) { [2] data };
    return 0;
end;
