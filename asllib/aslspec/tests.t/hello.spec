typedef t { prose_description = "t", };

typedef s { prose_description = "s", } = powerset(t)
    | A
    { prose_description = "A", }
    | B(s, s)
    { prose_description = "B", }
;
