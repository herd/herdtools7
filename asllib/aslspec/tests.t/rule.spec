typedef Num;

typedef rec = [f: Num, g: Num];

typing relation r(a: Num, b: Num, r: rec) -> (c: Num, r': rec) {} =
    res := cond(
        a = b : a,
        a > b : a + b,
        a < b : b
    );
    r' := r(f : res); // This is 'r' with its 'f' field updated to 'res'.
    --
    (res, r');
;
