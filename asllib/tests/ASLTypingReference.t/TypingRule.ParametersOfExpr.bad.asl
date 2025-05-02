constant A = 15;

func parameters_of_expressions{B, C, D, E}(
    z: integer{(D, E).item0}) => // Illegal expression in argument type
    bits(A)
begin
    return Ones{A};
end;
