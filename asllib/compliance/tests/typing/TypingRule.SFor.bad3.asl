func ill_typed_for_loop_3()
begin
    for j = 0 to 4 do
        pass;
    end;

    j = 0; // Illegal: 'j' is in scope only in the loop body.
end;
