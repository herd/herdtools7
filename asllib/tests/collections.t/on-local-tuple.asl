type MyCollection of collection {
  field1: bits(1),
  field2: bits(2),
};

var my_collection: MyCollection;

func main () => integer
begin
  var col2 = (my_collection, 32);
  let bv = col2.item0.field1;

  return 0;
end;

