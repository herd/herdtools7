func operator_priority(
    a: integer,
    b: integer,
    c: integer)
    begin
        let p_sub_sub = a - b - c;
        // '-' is not mathematically associative, so this is a static error.
        // Must be written as:
        let p_sub_sub_1 = (a - b) - c;
    end;
