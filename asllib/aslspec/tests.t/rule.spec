typedef Num;

typing relation r(a: Num, b: Num) -> (c: Num) {} =
    res := cond(
        a = b : a,
        a > b : a + b,
        a < b : b
    );
    --
    res;
;
