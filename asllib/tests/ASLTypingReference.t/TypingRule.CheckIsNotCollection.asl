func main () => integer
begin
  var test: collection {
    field1: bits(2),
    field2: bits(3),
  };; // Illegal: local storage elements cannot have collection types.

  println(test);

  return 0;
end;
