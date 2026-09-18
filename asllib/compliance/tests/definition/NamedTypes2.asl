type qualifiedData of bits(16) {
    [4] flag,
    [3: 0, 8:5] data,
    [9:0] value
};

type DatawithFlag of qualifiedData;
