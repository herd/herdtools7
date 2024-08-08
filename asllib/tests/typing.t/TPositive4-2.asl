func foo () => integer {8, 16}
begin return 8; end

let      LET_ALLOWED_NUMS_A                   = 8;
let      LET_ALLOWED_NUMS_B  : integer {8,16} = 8;
let      LET_ALLOWED_NUMS_C  : integer {8,16} = foo();
constant CONST_ALLOWED_NUMS  : integer {8,16} = 8;
config   CONFIG_ALLOWED_NUMS : integer {8,16} = foo();
var      VAR_ALLOWED_NUMS    : integer {8}    = 8;

func positive4()
begin
    let testA : integer {LET_ALLOWED_NUMS_A}     = 8;  // NOTE its the type of LET_ALLOWED_NUMS_A (ie integer {8}) NOT its value that's
                                                       // used to determine the demain of testA
    // let testB : integer {LET_ALLOWED_NUMS_B}     = 16; // This is valid as testB is of type integer {8,16}, NOT integer {8}
    // let testC : integer {LET_ALLOWED_NUMS_C}     = 16; // This is valid as testC is of type integer {8,16}, regardless of what foo()
                                                       // returns

    // testD is of type integer {0..16} because this is the union of integer {0..8} and integer {0..16}
    let testD : integer {0..LET_ALLOWED_NUMS_C}  = 3;
    let testE : integer {0..16}                  = testD;

    // configs can also be used and follow the same rules as lets
    let testF : integer {CONFIG_ALLOWED_NUMS}    = 16;
    let testG : integer {0..CONFIG_ALLOWED_NUMS} = 3;
end

