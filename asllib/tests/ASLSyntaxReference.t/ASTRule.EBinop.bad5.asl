func operator_precedence(
    a: integer,
    b: integer,
    c: integer)
    begin
        let p_sub_sub = a - b - c;
        // '-' is not associative, so this is a static error.
        // Must be written as:
        let p_sub_sub_1 = (a - b) - c;
    end;
