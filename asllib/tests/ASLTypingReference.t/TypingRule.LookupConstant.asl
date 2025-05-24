type Color of enumeration {RED, GREEN, BLUE};

constant WORD_SIZE = 64;

func main() => integer
begin
    var c : Color; // Initialization requires looking up the constant RED.
    var bv1: bits(WORD_SIZE); // Requires looking up WORD_SIZE.

    constant HALF_WORD_SIZE = 32;
    var bv2: bits(HALF_WORD_SIZE); // Requires looking up HALF_WORD_SIZE.

    return 0;
end;
