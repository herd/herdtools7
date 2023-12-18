func main () => integer
begin

  let match_me = 3 IN { >= 42 };
  assert match_me == FALSE;

  return 0;
end
