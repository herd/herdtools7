let      LET_ALLOWED_NUMS_A                   = 8;
let      LET_ALLOWED_NUMS_B  : integer {8,16} = 8;
let      LET_ALLOWED_NUMS_C  : integer {8,16} = foo();
constant CONST_ALLOWED_NUMS  : integer {8,16} = 8;
config   CONFIG_ALLOWED_NUMS : integer {8,16} = foo();
var      VAR_ALLOWED_NUMS    : integer {8}    = 8;

func reconsider4()
begin
    // R_LYDS currently has a special case for constants which makes the following illegal. Given we've already changed constants
    // to make them behave more consistently with config/lets in other respects (see ASL-503) we should probably also change R_LYDS
    // to make the following legal.
    // let testA : integer {CONST_ALLOWED_NUMS}     = 16;
    let testB : integer {0..CONST_ALLOWED_NUMS}  = 10;
end
