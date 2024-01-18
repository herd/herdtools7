func invoke_me(N: integer { 8, 16})
begin
    return;
end

func illegal_f4()
begin
    var x: integer { 8 .. 64 };
    invoke_me(x);
end
