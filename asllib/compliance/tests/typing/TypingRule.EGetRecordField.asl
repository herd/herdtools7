type MyRecordType of record {i: integer, b: boolean};

func main () => integer
begin
  let my_record = MyRecordType{i=3, b=TRUE};
  //      array access expression   inferred type
  var x = my_record.i               as integer;
  var y = my_record.b               as boolean;
  return 0;
end;
