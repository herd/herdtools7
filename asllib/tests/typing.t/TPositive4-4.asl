pure func foo () => integer {8, 16}
begin return 8; end;

config   CONFIG_ALLOWED_NUMS : integer {8,16} = foo();

func positive4()
begin
    // configs can also be used and follow the same rules as lets
    let testF : integer {CONFIG_ALLOWED_NUMS}    = 16;
end;

