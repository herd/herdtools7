func operator_priority(
    a: integer,
    b: integer,
    c: integer)
    begin
        let p_a_s = a + b - c;
        // '+' has equal priority to '-' so is a static error.
        // Must be written as either:
        let p_a_s_A1 = (a + b) - c;
        let p_a_s_A2 = a + (b - c);
    end;
