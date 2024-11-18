type Nested_Type of bits(32) {
    [31:16] fmt0 {
        [15] common,
        [14] moving
    },
    [31:16] fmt1 {
        [15] common,
        [0]  moving
    },
    [31] common,
    [0]  fmt            // format choice
};

var nested : Nested_Type = '10101010101010101010101010101010';

// select the correct view of moving
// nested.fmt is '0'
//    nested.fmt0.moving is nested[30]
// nested.fmt is '1'
//    nested.fmt1.moving is nested[16]
let moving = if nested.fmt == '0' then nested.fmt0.moving
    else nested.fmt1.moving;

func main() => integer
begin
// below are all equivalent to nested[31]
    let common = nested.common;
    let common_fmt0 = nested.fmt0.common;
    let common_fmt1 = nested.fmt1.common;
    assert common == common_fmt0;
    assert common == common_fmt1;
    return 0;
end;
