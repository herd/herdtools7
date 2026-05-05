func operator_precedence(
    a: integer,
    b: integer,
    c: integer,
    d: bits(8),
    e: bits(8),
    f: bits(8),
    g: boolean)
    begin
        let p_m_s = a * b - c;
        // '*' has higher precedence than '-' so interpreted as:
        let p_m_s_I = (a * b) - c;
        assert(p_m_s == p_m_s_I);
        let p_s_m = a - b * c;
        // '*' has higher precedence than '-' so interpreted as:
        let p_s_m_I = a - (b * c);
        assert(p_s_m == p_s_m_I);
        let p_a_e = a + b ^ c;
        // '^' has higher precedence than '+' so interpreted as:
        let p_a_e_I = a + (b ^ c);
        assert(p_a_e == p_a_e_I);
        let p_and_and = d AND e AND f;
        // 'AND' is associative so can be interpreted as either:
        let p_and_and_i1 = (d AND e) AND f;
        let p_and_and_i2 = d AND (e AND f);
        assert(p_and_and == p_and_and_i1);
        assert(p_and_and == p_and_and_i2);
        let p_band_eq = g && a == b;
        // '&&' is of precedence class 'Boolean'.
        // '==' is of precedence class 'Comparison'.
        // 'Comparison' has higher precedence than 'Boolean' so interpreted as:
        let p_band_eq_I = g && (a == b);
        assert(p_band_eq == p_band_eq_I);
    end;
