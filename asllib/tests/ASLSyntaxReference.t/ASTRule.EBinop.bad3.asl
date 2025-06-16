func operator_precedence(
    d: bits(8),
    e: bits(8),
    f: bits(8))
    begin
        let p_and_or = d AND e OR f;
        // 'AND' and 'OR' have equal precedence so is a static error.
        // Must be written as either:
        let p_and_or_A1 = (d AND e) OR f;
        let p_and_or_A2 = d AND (e OR f);
    end;
