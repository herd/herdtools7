constant HALF_WORD_SIZE = 16 * 2;
type Word of bits(HALF_WORD_SIZE * 2) {
    [HALF_WORD_SIZE * 2 - 1:HALF_WORD_SIZE] upper,
    [HALF_WORD_SIZE - 1:0] lower
};

func main() => integer
begin
    var x: Word;
    return 0;
end;
