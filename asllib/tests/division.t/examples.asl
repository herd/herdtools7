func main () => integer
begin
  assert 6 DIV 3 == 2;
  assert 6 DIVRM 3 == 2;
  assert 6 MOD 3 == 0;

  assert -6 DIV 3 == -2;
  assert -6 DIVRM 3 == -2;
  assert -6 MOD 3 == 0;

  assert 5 DIVRM 3 == 1;
  assert 5 MOD 3 == 2;
  
  assert -5 DIVRM 3 == -2;
  assert -5 MOD 3 == 1;

  return 0;
end
