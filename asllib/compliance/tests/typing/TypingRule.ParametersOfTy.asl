func parameters_of_types{A, B, C, D, E}(
    x: (bits(B), bits(C)),
    y: integer{D..E},
    z: real,
    w: integer) =>
    bits(A)
begin
    return Ones{A};
end;
