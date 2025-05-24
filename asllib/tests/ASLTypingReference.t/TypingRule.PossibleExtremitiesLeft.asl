func pow_func{A, B, C}(
    a: integer{A},
    b: integer{B},
    c: integer{C})
begin
    var x : integer{A..B} = ARBITRARY : integer{A..B};
    var e_mul : integer{
        (A * C)..(A * C),
        (A * C)..(B * C),
        (B * C)..(A * C),
        (B * C)..(B * C)} = x * c;
    var e_plus: integer {(C + A)..(C + B)} = x + c;
end;
