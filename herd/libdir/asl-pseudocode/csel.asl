// We do not yet support bitfield reading, so we have to do this instead
getter PSTATE_V => bits(1)
begin
  return read_pstate_nzcv() AND 1;
end

getter PSTATE_C => bits(1)
begin
  return (read_pstate_nzcv() AND 2) >> 1;
end

getter PSTATE_Z => bits(1)
begin
  return (read_pstate_nzcv() AND 4) >> 2;
end

getter PSTATE_N => bits(1)
begin
  return (read_pstate_nzcv() AND 8) >> 3;
end

func ConditionHolds(cond::bits(4)) => boolean
begin
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

  // However, we do not yet support bitfields reading

  case cond AND 14 of      // ie cond<3:1>
    when 0 : result = PSTATE_Z == 1;                                  // EQ or NE
    when 2 : result = PSTATE_C == 1;                                  // MI or PL
    when 4 : result = PSTATE_N == 1;                                  // MI or PL
    when 6 : result = PSTATE_V == 1;                                  // VS or VC
    when 8 : result = ( PSTATE_C == 1 ) && ( PSTATE_Z == 0 );         // HI or LS
    when 10: result = ( PSTATE_N == PSTATE_V );                       // GE or LT
    when 12: result = ( PSTATE_N == PSTATE_V ) && ( PSTATE_Z == 0 );  // GT or LE
    when 14: result = 1;                                              // AL
  end

  if ((cond AND 1) == 1) && (cond != 15) then
    result = !result;
  end

  return result;
end

func main(d::integer, n::integer, m::integer, cond::integer, datasize::integer)
begin
	if ConditionHolds(cond) then
		result = read_register(n, datasize);
	else
		result = read_register(m, datasize);
	end

  write_register(d, datasize, result);
end
