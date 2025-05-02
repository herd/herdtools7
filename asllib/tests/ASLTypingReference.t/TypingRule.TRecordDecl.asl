type MyRecord of record { a: integer, b: boolean };
type RecordWithEmptyFieldList of record {-};
type RecordWithoutFields of record;

func main() => integer
begin
    - = MyRecord {a = 3, b = TRUE};
    - = RecordWithEmptyFieldList {-};
    - = RecordWithoutFields {-};
    return 0;
end;
