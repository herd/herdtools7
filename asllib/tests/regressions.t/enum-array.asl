type MyEnum of enumeration { A, B, C };

type MyArray of array [ MyEnum ] of integer;

func main () => integer
begin
  var my_array: MyArray;
  print (my_array);

  my_array[A] = 1;
  my_array[B] = 2;
  my_array[C] = 3;

  assert (my_array[A] == 1);
  assert (my_array[B] == 2);
  assert (my_array[C] == 3);

  return 0;
end

