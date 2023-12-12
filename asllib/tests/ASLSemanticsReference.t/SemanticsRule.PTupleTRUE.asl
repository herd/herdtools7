func main () => integer
begin

  let match_me = (3, '101010') IN {( <= 42, 'xx1010')};
  assert match_me == TRUE;

  return 0;
end
