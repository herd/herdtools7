func main () => integer
begin

  let match_true = 42 IN { 3, 42 };
  assert match_true == TRUE;

  let match_false = 42 IN { 3, 4 };
  assert match_false == FALSE;

  return 0;
end;
