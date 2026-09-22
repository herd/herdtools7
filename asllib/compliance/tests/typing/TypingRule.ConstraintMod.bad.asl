func main() => integer
begin
    var x : integer{0..10};
    var y = 3;
    var z = x MOD y;
    z = 0;
    z = 1;
    z = 2;
    z = 3; // Illegal: the type inferred for z is integer{0..2}
    return 0;
end;