type MyCollection of collection {
  field1: bits(2),
  field2: bits(3),
};

func main () => integer
begin
  var test: MyCollection; // Illegal: local storage elements cannot have collection types.

  println(test);

  return 0;
end;
