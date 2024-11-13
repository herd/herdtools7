var      VAR_ALLOWED_NUMS    : integer {8, 16} = 8;

func negative4()
begin
    let testA : integer {VAR_ALLOWED_NUMS} = 8; // illegal var's aren't allowed in constraints
end;
