func invoke_me(N: integer { 8, 16})
begin
    return;
end

func illegal_f3()
begin
    var x: integer;
    invoke_me(x); // illegal as domains doesn't match
end
