func operator_priority(
    a: integer,
    b: integer,
    c: integer)
    begin
        let p_s_a = a - b + c;
        // '-' has equal priority to '+' so is a static error.
        // Must be written as either:
        let p_s_a_A1 = (a - b) + c;
        let p_s_a_A2 = a - (b + c);
    end;
