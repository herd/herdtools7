type MyRecord of record { a: integer, b: boolean };
type RecordWithEmptyFieldList of record {};
type RecordWithoutFields of record;

func main() => integer
begin
    var - = MyRecord {a = 3, b = TRUE};
    var - = RecordWithEmptyFieldList {};
    var - = RecordWithoutFields {};
    return 0;
end;
