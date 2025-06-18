type MyCollection of collection {
  field1: bits(3),
  field2: bits(5),
};

var my_collection: MyCollection;

func main () => integer
begin
  print(my_collection.field2);

  return 0;
end;
