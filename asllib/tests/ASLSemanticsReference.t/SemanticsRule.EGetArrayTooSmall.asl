type MyArrayType of array [[3]] of integer;

var my_array : MyArrayType;

func main () => integer
begin
  println my_array[[3]];
  return 0;
end;
