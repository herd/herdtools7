func constraint_ops{A,B,C,D}(
    bv_a: bits(A),
    bv_b: bits(B),
    bv_c: bits(C),
    bv_d: bits(D))
begin
    var ab : integer{A..B} = A as integer{A..B};
    var a : integer{A} = A as integer{A};
    var cd : integer{C..D} = C as integer{C..D};

    var ab_plus_cd  : integer{(C + A)..(D + B)} = ab + cd;
    var ab_minus_cd : integer{(- D + A)..(- C + B)} = ab - cd;

    // Notice how the set of integers A*2, (A+1)*2, ..., B*2 is approximated
    // by the following range constraints:
    var ab_times_2  : integer{(2 * A)..(2 * A), (2 * A)..(2 * B), (2 * B)..(2 * A),
                              (2 * B)..(2 * B)} = ab * 2;
    var a_times_cd  : integer{(A * C)..(A * C), (A * C)..(A * D), (A * D)..(A * C),
                              (A * D)..(A * D)} = a * cd;
    // Notice how in the next statement, 0 has been filtered out
    // from the left-hand-side constraints.
    var a_div : integer{A, (A DIV 2), (A DIV 3)} = a DIV (1 as integer{-5..3});

    // Notice how in the next statement, the left-hand-side constraints
    // only depend on the right operand constraint 0..3.
    var a_mod_0_to_3 : integer{0..2} = a MOD (1 as integer{0..3});
    var mod_0_to_3_a : integer{0..(A - 1)} = (1 as integer{0..3}) MOD a;
    var mod_0_to_3_ab : integer{0..(B - 1)} = (1 as integer{0..3}) MOD ab;

    var a_div_cd : integer{A..(A DIV D), (A DIV D)..A} = a DIV cd;
    var cd_div_a : integer{(C DIV A)..(D DIV A)} = cd DIV a;
    var a_mod_cd : integer{0..(D - 1)} = a MOD cd;
    var cd_mod_a : integer{0..(A - 1)} = cd MOD a;

    var a_pow_cd : integer{0..(A ^ D), 1, (- ((- A) ^ D))..((- A) ^ D)} = (a ^ cd) as
                   integer{0..(A ^ D), 1, (- ((- A) ^ D))..((- A) ^ D)};
    var cd_pow_a : integer{0..(D ^ A), (- ((- C) ^ A))..((- C) ^ A)} = cd ^ a;

    // Notice how large sets of constraints (more than 2^17)
    // are approximated via ranges.
    var - : integer{0..2^28} = (1 as integer{0..2^14}) * (2 as integer{0..2^14});
    var - : integer{0..2^14} = (1 as integer{0..2^14}) DIV (2 as integer{0..2^14});
end;
