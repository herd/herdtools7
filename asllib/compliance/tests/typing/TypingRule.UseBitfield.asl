constant FOUR = 4;
constant FIVE = 4;

var myData: bits(16) {
    [FOUR] flag,            // { Other(FOUR) }
    [3:0, 8:FIVE] data {    // { Other(FIVE), Other(FOUR) }
        [FOUR] data_5       // { Other(FOUR) }
    },
    [9:0] value             // { }
};
