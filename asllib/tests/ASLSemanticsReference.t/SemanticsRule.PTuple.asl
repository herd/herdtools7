func main () => integer
begin

  let match_true = (3, '101010') IN {( <= 42, 'xx1010')};
  assert match_true == TRUE;

  let match_false = (3, '101010') IN {( >= 42, 'xx1010')};
  assert match_false == FALSE;

  return 0;
end;
