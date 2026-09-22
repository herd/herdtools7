constant WORD_SIZE = 64;
type Word of bits(WORD_SIZE) {
    [WORD_SIZE DIV 3 - 1:WORD_SIZE DIV 2] upper,
    [WORD_SIZE DIV 2 - 1:0] lower
};
