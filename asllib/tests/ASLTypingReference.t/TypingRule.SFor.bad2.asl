func ill_typed_for_loop_2()
begin
    for i = 0 to 4 do
        // Illegal: 'i' is immutable.
        i = i + 1;
    end;
end;
