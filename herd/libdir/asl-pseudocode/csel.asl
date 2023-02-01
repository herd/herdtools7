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

  mcond = cond AND 14 ; // ie cond<3:1>
  if mcond == 0 then                       // EQ or NE
    result = PSTATE_Z == 1;
  else if mcond == 2 then                  // CS or CC
    result = PSTATE_C == 1;
  else if mcond == 4 then                  // MI or PL
    result = PSTATE_N == 1;
  else if mcond == 6 then                  // VS or VC
    result = PSTATE_V == 1;
  else if mcond == 8 then                  // HI or LS
    result = ( PSTATE_C == 1 ) && ( PSTATE_Z == 0 );
  else if mcond == 10 then                  // GE or LT
    result = ( PSTATE_N == PSTATE_V );
  else if mcond == 12 then                  // GT or LE
    result = ( PSTATE_N == PSTATE_V ) && ( PSTATE_Z == 0 );
  else                                     // AL
    result = 1;
  end end end end end end end

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
