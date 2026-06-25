func main () => integer
begin
  var int_array = ARBITRARY : array[[3]] of integer;
  int_array[[2]] = 1;
  assert int_array[[2]] == 1;

  return 0;
end;
