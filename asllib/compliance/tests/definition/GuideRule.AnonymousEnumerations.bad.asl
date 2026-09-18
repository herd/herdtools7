func main() => integer
begin
    // Illegal (doesn't parse): enumeration can only be declared in type definitions.
    var x : enumeration {RED, GREEN, BLUE};
    return 0;
end;
