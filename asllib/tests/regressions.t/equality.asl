type myenum of enumeration {A, B, C};

type myty of integer;

func main () => integer
begin
  assert 1 == 1;
  assert 1 != 2;
  assert TRUE == TRUE;
  assert TRUE != FALSE;
  assert 3.13 == 3.13;
  assert 3.13 != 3.14;
  assert '010' == '010';
  assert '011' != '010';
  assert "blah" == "blah";
  assert "foo" != "blah";
  assert A == A;
  assert A != B;
  assert (1 as myty) == (1 as integer);
  assert (1 as myty) != (2 as integer);

  return 0;
end;

