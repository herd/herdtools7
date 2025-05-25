type MYCOLLECTION of collection {
  field1: bits(3),
  field2: bits(4),
};

var MyCollection: MYCOLLECTION;

type MYRECORD of record {
  field1: bits(3),
  field2: bits(4),
};

var MyRecord: MYRECORD;

func main () => integer
begin
  MyCollection.[field2, field1] = '1010010';
  assert MyCollection.[field2, field1] == '1010010';
  assert MyCollection.field2 == '1010';
  assert MyCollection.field1 == '010';

  MyRecord.[field2, field1] = '1010010';
  assert MyRecord.[field2, field1] == '1010010';
  assert MyRecord.field2 == '1010';
  assert MyRecord.field1 == '010';
  return 0;
end;
