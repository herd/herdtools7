typedef t << prose_description = "t", >>;

typedef s << prose_description = "s", >> = powerset(t)
    | A
    | B(s, s)
;
