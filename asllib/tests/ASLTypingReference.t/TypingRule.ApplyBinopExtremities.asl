func pow_func{A, B, C, D}(
    a: integer{A},
    b: integer{B},
    c: integer{C},
    d: integer{D})
begin
    var x : integer{A..B} = ARBITRARY : integer{A..B};
    var y : integer{C..D} = ARBITRARY : integer{C..D};

    var e_plus: integer {(C + A)..(D + B)} = x + y;
    var e_minus: integer {((-1 * D) + A)..((-1 * C) + B)} = x - y;
    var e_mul:
      integer {(A * C)..(A * C), (A * C)..(A * D), (A * C)..(B * C),
               (A * C)..(B * D), (A * D)..(A * C), (A * D)..(A * D),
               (A * D)..(B * C), (A * D)..(B * D), (B * C)..(A * C),
               (B * C)..(A * D), (B * C)..(B * C), (B * C)..(B * D),
               (B * D)..(A * C), (B * D)..(A * D), (B * D)..(B * C),
               (B * D)..(B * D)} = x * y;
    var e_pow: integer {0..(B ^ D), 1, (- ((- A) ^ D))..((- A) ^ D)} = x ^ y;
    var e_div: integer {A..(B DIV D), (A DIV D)..B} = x DIV y;
    var e_divrm:
      integer {(A DIVRM 1)..(B DIVRM D), (A DIVRM D)..(B DIVRM 1)} = x DIVRM y;
    var e_shl: integer {A..(B << D), (A << D)..B} = x << y;
    var e_shr: integer {(A >> 0)..(B >> D), (A >> D)..(B >> 0)} = x >> y;
end;
