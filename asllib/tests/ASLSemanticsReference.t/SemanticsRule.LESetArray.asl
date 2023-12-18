func main () => integer
begin

  var my_array: array [42] of integer;
  my_array[3] = 53;
  assert my_array[3] == 53;

  return 0;
end
