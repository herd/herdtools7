func main () => integer
begin
  var col = ARBITRARY: collection {
    field1: bits(1),
    field2: bits(2),
  };

  let bv = col.field1;

  return 0;
end;
