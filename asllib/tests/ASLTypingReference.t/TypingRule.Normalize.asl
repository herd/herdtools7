                                // normalized rhs expression
constant TWO = 1 + 1;           // 2

func main() => integer
begin
                                // normalized rhs expression
    let FOUR = TWO * 2;         // 4
    var o = FOUR + FOUR;        // 2 * FOUR
    - = 5.0;                    // 5.0

    let x = 3;                  // 3
    - = 3 * x;                  // 9

    var y : integer{1, 2, 3};
    var z : integer{4, 5, 6};
    var p1 = (y + 5) - z;        // (y + 5) - z
    var p2 = (z DIV 2) * y;      // (z DIV 2) * y
    var p3 = z * (y DIV 2);      // z * (y DIV 2)
    var p4 = z * (z * y);        // z * (z * y)
    var p5 = z as integer{4, 5}; // z
    var p6 = z ^ y;              // z ^ y

    return 0;
end;
