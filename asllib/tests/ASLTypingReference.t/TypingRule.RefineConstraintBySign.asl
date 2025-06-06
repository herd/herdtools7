func foo{A, B}(a: integer{A}, b: integer{B})
begin
    var y: integer{
        0,
        1,
        B,
        3..4,
        -1..2,
        -4.. -3,
        -1..B,
        B.. -1,
        A..B
    } = 0;
    var z: integer{} = A DIV y;
    // The following type represents the type constraints generated for `A DIV y`
    // with each constraints followed by a comment denoting the corresponding
    // constraint of `y`.
    var z_typed: integer{
        // 0 (None)
        A, // 1
        (A DIV B), // B
        (A DIV 3), (A DIV 4), // 3..4
        A, (A DIV 2), // -1..2
        // -4..-3 (None)
        A..(A DIV B), (A DIV B)..A, // -1..B
        A..( -1 * A), ( -1 * A)..A, // B.. -1
        A..(A DIV B), (A DIV B)..A // A..B
    } = A DIV y;
end;
