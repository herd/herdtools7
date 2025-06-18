var MyCollection : collection {
  field1: bits(1),
  field2: bits(2),
};

func foo () => collection {
    field1: bits(1),
    field2: bits(2),
  }
begin
  return MyCollection;
end;

