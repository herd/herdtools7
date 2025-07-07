constant WORD_SIZE = 32;

// A bitvector width for a bitvector type with bitfields must be pure.
type Data of bits(WORD_SIZE * 2) {
    [0] LSB
};

var g_impure : integer{0..10} = 5;

// An expression initializing a constant storage element must be pure.
config c : integer{0..WORD_SIZE * 8} = WORD_SIZE * 4;

let gl : integer{0..10} = g_impure;

// An expression initializing a config storage element must be pure.
constant ADDRESS_SPACE_SIZE = 2 ^ WORD_SIZE;

func main() => integer
begin
    return 0;
end;
