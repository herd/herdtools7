func pow_func{A, B, C}(
    a: integer{A},
    b: integer{B},
    c: integer{C})
begin
    var y: integer{B..C} = ARBITRARY : integer{B..C};
    var e_plus: integer {(B + A)..(C + A)} = a + y;
    var e_minus: integer {((-1 * C) + A)..((-1 * B) + A)} = a - y;
    var e_mul: integer{
        (A * B)..(A * B),
        (A * B)..(A * C),
        (A * C)..(A * B),
        (A * C)..(A * C)} = a * y;
    var e_shl: integer {A..(A << C), (A << C)..A} = a << y;
    var e_shr: integer {(A >> 0)..(A >> C), (A >> C)..(A >> 0)} = a >> y;
    var e_div: integer {A..(A DIV C), (A DIV C)..A} = a DIV y;
    var e_divrm: integer {
        (A DIVRM 1)..(A DIVRM C),
        (A DIVRM C)..(A DIVRM 1)} = a DIVRM y;
end;
