var MyCollection : collection {
  field1: bits(1),
  field2: bits(2),
};

var MyCollection2 = MyCollection;

func main () => integer
begin
  print(MyCollection2.field1);

  return 0;
end;

