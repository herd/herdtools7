var myData: bits(16) {
    [4] flag,
    // Illegal: slices declared for the same bitfield must not overlap
    [3:0, 5:3] data,
    [3*:4] value
};
