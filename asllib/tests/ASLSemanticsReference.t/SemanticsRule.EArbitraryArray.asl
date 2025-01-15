type Enum of enumeration {A, B, C};
type Arr of array[[Enum]] of integer;

func main () => integer
begin
  var int_array = ARBITRARY : array[[3]] of integer;
  int_array[[2]] = 1;
  assert int_array[[2]] == 1;

  var enum_array = ARBITRARY : Arr;
  enum_array[[A]] = 7;
  assert enum_array[[A]] == 7;

  return 0;
end;
