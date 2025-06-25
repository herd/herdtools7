var g : integer = 0;
func upper_bound() => integer
begin
    g = 1; // writing to a global variables implies impurity.
    return 5;
end;

func ill_typed_for_loop_3()
begin
    // Illegal: 'upper_bound()' is not a readonly expression.
    for j = 0 to upper_bound() do
        pass;
    end;
end;
