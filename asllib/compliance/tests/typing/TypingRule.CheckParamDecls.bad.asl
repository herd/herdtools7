// Illegal: a parameter list should start with the parameter
// of the return value: {D, A, B, C}
func parameter_lists{A, B, C, D}(
    x: integer{A..B},
    y: bits(C)) =>
    bits(D)
begin
    return Ones{D};
end;
