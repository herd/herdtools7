func pow_func{A, B, C, D}(
    a: integer{A},
    b: integer{B},
    c: integer{C},
    d: integer{D})
begin
    var x : integer{A..B} = ARBITRARY : integer{A..B};
    var y : integer{C..D} = ARBITRARY : integer{C..D};
    var z : integer{0..(B ^ D), 1, (- ((- A) ^ D))..((- A) ^ D)} = x ^ y;
end;
