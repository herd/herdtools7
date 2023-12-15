func main () => integer
begin

  let match_me = 42 IN { 3, 4 };
  assert match_me == FALSE;

  return 0;
end
