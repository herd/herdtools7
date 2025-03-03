type Color of enumeration {RED, GREEN, BLUE};

func main () => integer
begin

  var my_array: array [[Color]] of integer;
  my_array[[RED]] = 53;
  assert my_array[[RED]] == 53;

  return 0;
end;
