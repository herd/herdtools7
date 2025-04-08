constant g = 5;
let gl : integer{0..1000} = 500;

type Data of bits(g * 2) {
    [0] LSB
};

// Legal as there are no bitfields.
type Data2 of bits(g + gl) {
};

// The next declaration in comment is illegal, since the maximal time frame
// for `g + gl` is Execution time.
// type Data3 of bits(g + gl) {
//     [0] LSB
// };
