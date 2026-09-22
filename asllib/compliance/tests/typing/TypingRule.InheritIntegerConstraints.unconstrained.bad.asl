func main() => integer
begin
    var a : integer;
    // The following is illegal as 'a' is not a constrained integer.
    var g : integer{} = a;
    return 0;
end;
