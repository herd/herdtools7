type Nested_Type of bits(16) {
    [12:9, 7:2] slices {    // [12:9, 7:2] slices
        [5:2, 7:6, 9:8] sub // [12:11, 7:2] slices.sub
    },

    [9+:4, 2+:6] slices1 {  // [12:9, 7:2] slices1
        [5:2, 7:6, 9:8] sub // [12:11, 7:2] slices1.sub
    },

    [12:11, 7:2] sub        // [12:11, 7:2] sub
};
