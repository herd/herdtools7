func operator_precedence(
    a: integer,
    b: integer,
    c: integer)
    begin
        let p_s_a = a - b + c;
        // '-' has equal precedence to '+' so is a static error.
        // Must be written as either:
        let p_s_a_A1 = (a - b) + c;
        let p_s_a_A2 = a - (b + c);
    end;
