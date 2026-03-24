typedef Num;

typedef rec = [f: Num, g: Num] { "the rec record with $\Fieldf$ value {f} and $\Fieldg$ value {g}" };

typing relation r(a: Num, b: Num, r: rec) -> (c: Num, r': rec) {} =
    res := cond(
        a = b : a,
        a > b : a + b,
        a < b : b
    );
    y := [f: a, g: b];
    r_f := r.f + r.g; // equivalent to r_f := (r.f + r.g);
    r' := r(f : res); // This is 'r' with its 'f' field updated to 'res'.
    --
    (res, r');
;
