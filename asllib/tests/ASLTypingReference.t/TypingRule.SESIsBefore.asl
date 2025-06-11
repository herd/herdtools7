constant WORD_SIZE = 32;

// The time frame of a bitvector width for a bitvector
// type with bitfields must be Constant.
type Data of bits(WORD_SIZE * 2) {
    [0] LSB
};

var g_execution_time : integer{0..10} = 5;

config c : integer{0..WORD_SIZE * 8} = WORD_SIZE * 4;

let gl : integer{0..10} = g_execution_time;

// The time frame of an expression initializing
// a local storage constant must be Constant.
constant ADDRESS_SPACE_SIZE = 2 ^ WORD_SIZE;

func main() => integer
begin
    return 0;
end;
