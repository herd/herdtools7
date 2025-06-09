func foo()
begin
    // Illegal as non_constrained is not a constrained integer.
    let non_constrained: integer = 5;
    var illegal_array2: array [[non_constrained]] of integer;
end;
