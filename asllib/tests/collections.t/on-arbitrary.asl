type MyCollection of collection {
  field1: bits(1),
  field2: bits(2),
};

func main () => integer
begin
  let col = ARBITRARY: MyCollection;
  let bv = col.field1;

  return 0;
end;
