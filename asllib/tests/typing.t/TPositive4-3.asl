func foo () => integer {8, 16}
begin return 8; end

let      LET_ALLOWED_NUMS_C  : integer {8,16} = foo();

func positive4()
begin
    let testD : integer {0..LET_ALLOWED_NUMS_C}  = 3;
    let testE : integer {0..16}                  = testD;
end

