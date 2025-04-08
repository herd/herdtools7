let g : integer{0..10} = 1;

// Illegal as `g` is in the Execution time frame.
type Data of bits(g * 2) {
    [0] LSB
};
