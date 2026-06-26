var g : integer = 0;
func upper_bound() => integer
begin
    g = 1; // writing to a global variable implies impurity.
    return 5;
end;

func ill_typed_for_loop_4()
begin
    // Illegal: 'upper_bound()' is not a readonly expression.
    for j = 0 to upper_bound() do
        pass;
    end;
end;
