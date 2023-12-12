func main () => integer
begin

  let match_me = 42 IN { >= 3 };
  assert match_me == TRUE;

  return 0;
end
