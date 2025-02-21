type MyRecordType of record {a: integer, b: integer};

func main () => integer
begin

  let my_record = MyRecordType{a=3, b=42};
  assert my_record.a == 3;

  // The following statement is illegal as every record field
  // needs to be initialized exactly once.
  // let - = MyRecordType{a=3, a=4, b=42};
  return 0;
end;
