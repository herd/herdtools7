func foo () => integer {8, 16}
begin return 8; end

let      LET_ALLOWED_NUMS_C  : integer {8,16} = foo();

func positive4()
begin
    let testC : integer {LET_ALLOWED_NUMS_C}     = 16; 
end

