func foo(symbolically_evaluable_var: integer)
begin
    var legal_array: array [[symbolically_evaluable_var]] of integer;
    let symbolically_evaluable_var2 = symbolically_evaluable_var * 2;
    var legal_array2: array [[symbolically_evaluable_var2]] of integer;

    // Illegal as non_symbolically_evaluable is mutable.
    var non_symbolically_evaluable = 5;
    var illegal_array: array [[non_symbolically_evaluable]] of integer;
end;
