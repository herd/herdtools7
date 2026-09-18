func TypeAssertions()
begin
    var A: bit;
    let B: integer = A as integer; // Illegal: bit cannot be an integer.
end;
