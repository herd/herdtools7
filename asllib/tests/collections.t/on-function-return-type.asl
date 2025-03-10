type MyCollection of collection {
  field1: bits(1),
  field2: bits(2),
};

var col: MyCollection;

func foo () => MyCollection
begin
  return col;
end;

