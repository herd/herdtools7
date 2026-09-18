func main () => integer
begin

  let match_true = '101010' IN {'xx1010'};
  assert match_true == TRUE;

  let match_false = '101010' IN {'0x1010'};
  assert match_false == FALSE;

  let match_empty_mask = '' IN {''};
  assert match_empty_mask == TRUE;

  return 0;
end;
