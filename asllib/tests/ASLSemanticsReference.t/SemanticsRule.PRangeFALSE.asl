func main () => integer
begin

  let match_me = 1 IN {3..42};
  assert match_me == FALSE;

  return 0;
end
