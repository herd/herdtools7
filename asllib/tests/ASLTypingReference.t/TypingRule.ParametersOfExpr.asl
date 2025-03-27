constant A = 15;

func parameters_of_expressions{B, C, D, E, F, G}(
    x: integer{A..B},
    y: integer{C .. (- D)},
    z: integer{E+F .. (G)}) =>
    bits(A)
begin
    return Ones{A};
end;
