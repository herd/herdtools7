constant A = 15;

func parameters_of_expressions{B, C, D, E, F, G, H, I, J}(
    w: integer{A..B},
    x: integer{C .. (- D)},
    y: integer{E+F .. (G)},
    z: integer{if H == 0 then I else J}) =>
    bits(A)
begin
    return Ones{A};
end;
