func ConditionHolds(cond)

    // Current code for ConditionHolds in Armv8a

    // boolean result;
    // case cond<3:1> of
    //     when '000' result = (PSTATE.Z == '1');                          // EQ or NE
    //     when '001' result = (PSTATE.C == '1');                          // CS or CC
    //     when '010' result = (PSTATE.N == '1');                          // MI or PL
    //     when '011' result = (PSTATE.V == '1');                          // VS or VC
    //     when '100' result = (PSTATE.C == '1' && PSTATE.Z == '0');       // HI or LS
    //     when '101' result = (PSTATE.N == PSTATE.V);                     // GE or LT
    //     when '110' result = (PSTATE.N == PSTATE.V && PSTATE.Z == '0');  // GT or LE
    //     when '111' result = TRUE;                                       // AL

    // // Condition flag values in the set '111x' indicate always true
    // // Otherwise, invert condition if necessary.
    // if cond<0> == '1' && cond != '1111' then
    // result = !result;

    // However, herd does not support all this, so we can only use this

    pstate_nzcv = read_pstate_nzcv();
    // Condition supported now :  NE | EQ | GE | GT | LE | LT
    if cond == 0 then // NE
        result = pstate_nzcv AND 2 == 0
    else if cond == 1 then // EQ
        result = pstate_nzcv AND 2 == 2
    else if cond == 2 then // GE
        result = pstate_nzcv AND 1 == 0
    else if cond == 3 then // GT
        result = pstate_nzcv AND 3 == 0
    else if cond == 4 then // LE
        result = pstate_nzcv AND 1 == 1
    else if cond == 5 then // LT
        result = pstate_nzcv AND 3 == 1
    else pass end end end end end end;

    return result
endfunc

func main(d, n, m, cond)
	if ConditionHolds(cond) then
		result = read_register(n)
	else
		result = read_register(m)
	end;
    write_register(d, result)
endfunc
