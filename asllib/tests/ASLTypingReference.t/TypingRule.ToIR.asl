func main() => integer
begin
                                // to_ir of rhs expression
    constant ONE = 1;           // 1
    var o = ONE;                // 1
    - = 5.0;                    // Top

    let x = 3;                  // 3
    - = 3 * x;                  // 9

    var y : integer{1, 2, 3};
    var z : integer{4, 5, 6};
    var p1 = (y + 5) - z;        // 5 + y - x
    var p2 = (z DIV 2) * y;      // (z * y) DIV 2
    var p3 = z * (y DIV 2);      // (z * y) DIV 2
    var p4 = z * (z * y);        // z^2 * y
    var p5 = z as integer{4, 5}; // z
    var p6 = z ^ y;              // Top

    return 0;
end;
