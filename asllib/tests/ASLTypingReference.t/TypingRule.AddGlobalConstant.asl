constant FOUR = 4;
// The static environment binds FOUR to 4.

func main() => integer
begin
    var bv: bits(2^FOUR) = Zeros{FOUR*FOUR};
    return 0;
end;
