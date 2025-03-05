var myData: bits(16) {
    [4] flag,
    [3:0, 5+:3] data,
    [3*:5] value // Illegal: position 19 exceeds 16
};
