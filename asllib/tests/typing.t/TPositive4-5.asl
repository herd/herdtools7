func foo () => integer {8, 16}
begin return 8; end

config   CONFIG_ALLOWED_NUMS : integer {8,16} = foo();

func positive4()
begin
    let testG : integer {0..CONFIG_ALLOWED_NUMS} = 3;
end

