func operator_precedence(
    a: integer,
    b: integer,
    g: boolean)
    begin
        let p_eq_eq = a == b != g;
        // '==' and '!=' and equal precedence so is a static error.
        // Must be written as:
        let p_eq_eq_A1 = (a == b) != g;
        // Note: 'a == (b != g)' is not valid as it does not type satisfy.
    end;ma
