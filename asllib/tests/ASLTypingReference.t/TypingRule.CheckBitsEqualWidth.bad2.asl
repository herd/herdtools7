func compare{
    int1: integer {1,2},
    int2: integer {1,2}}
    (
    bit1: bits(int1),
    bit2: bits(int2)
)
begin
    var cond: boolean;
    cond = bit1 == bit2; // Illegal
end;
