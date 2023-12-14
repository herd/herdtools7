type MyArrayType of array [3] of integer;

var my_array : MyArrayType;  

func main () => integer
begin

  my_array[3]=42;
  assert my_array[3]==42;

  return 0;
end 
