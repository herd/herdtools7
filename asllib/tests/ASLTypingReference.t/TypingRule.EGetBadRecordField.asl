type MyRecordType of record {i: integer, b: boolean};

func main () => integer
begin
  let my_record = MyRecordType{i=3, b=TRUE};
  // Illegal as field 'undeclared_field' does not exist in MyRecordType.
  var x = my_record.undeclared_field;
  return 0;
end;
