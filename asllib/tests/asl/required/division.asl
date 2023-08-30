func divrm()
begin
  // Identities
  assert(1 DIVRM 1 == 1);
  assert(5 DIVRM 1 == 5);
  assert(5 DIVRM 5 == 1);
  assert(0 DIVRM 4 == 0);

  // Exact
  assert(4 DIVRM 2 == 2);

  // Rounding
  assert(5 DIVRM 2 == 2);
  assert(11 DIVRM 3 == 3);
end

func check_mod()
begin
  assert(5 MOD 1 == 0);
  assert(4 MOD 2 == 0);
  assert(3 MOD 2 == 1);
  assert(3 MOD 3 == 0);
end

func main () => integer
begin
  divrm();
  check_mod();

  assert 6 DIV 3 == 2;
  assert 6 DIVRM 3 == 2;
  assert 6 MOD 3 == 0;

  assert (-6) DIV 3 == -2;
  assert (-6) DIVRM 3 == -2;
  assert (-6) MOD 3 == 0;

  assert 5 DIVRM 3 == 1;
  assert 5 MOD 3 == 2;

  assert (-5) DIVRM 3 == -2;
  assert (-5) MOD 3 == 1;

  return 0;
end
