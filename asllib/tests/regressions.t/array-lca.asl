type A of integer;
func main() => integer
begin
    var b = UNKNOWN: boolean;
    var a : array[5] of integer;
    var c : array[5] of A;
    var x = if (b) then a else c;
    return 0;
end;

