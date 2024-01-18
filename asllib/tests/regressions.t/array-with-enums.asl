type MyEnum of enumeration {
  MyEnumA,
  MyEnumB,
  MyEnumC,
};

type MyArray of array [ MyEnum ] of integer;

var myArray : MyArray;

func main () => integer
begin
  myArray[MyEnumA] = 3;
  assert myArray[MyEnumA] == 3;


  myArray[MyEnumB] = 4;
  assert myArray[MyEnumA] == 3;
  assert myArray[MyEnumB] == 4;

  return 0;
end
