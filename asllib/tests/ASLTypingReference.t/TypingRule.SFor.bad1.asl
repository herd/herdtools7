func ill_typed_for_loop_1()
begin
    var i : integer = 0;
    // Illegal: 'i' is already declared.
    for i = 0 to 4 do
        pass;
    end;
end;
