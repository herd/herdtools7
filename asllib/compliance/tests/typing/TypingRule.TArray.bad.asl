func foo(symbolically_evaluable_constrained_var: integer{2,4,8})
begin
    var legal_array: array [[symbolically_evaluable_constrained_var]] of integer;
    let symbolically_evaluable_constrained_var2 = symbolically_evaluable_constrained_var * 2;
    var legal_array2: array [[symbolically_evaluable_constrained_var2]] of integer;

    // Illegal as non_symbolically_evaluable is mutable.
    var non_symbolically_evaluable = 5;
    var illegal_array1: array [[non_symbolically_evaluable]] of integer;
end;
