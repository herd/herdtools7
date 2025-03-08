type MyCollection of collection {
  field1: bits(1),
  field2: bits(2),
};

func foo (col: MyCollection) => integer
begin
  let bv = col.field1;

  return 0;
end;

