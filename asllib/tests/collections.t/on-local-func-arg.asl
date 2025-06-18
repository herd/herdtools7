var MyCollection : collection {
  field1: bits(1),
  field2: bits(2),
};

func foo (col: collection {
    field1: bits(1),
    field2: bits(2),
  }) => integer
begin
  let bv = col.field1;

  return 0;
end;

