type MyRecord of record { a: integer, b: boolean };
type RecordWithoutFields of record{-};

func main() => integer
begin
    - = MyRecord {a = 3, b = TRUE};
    - = RecordWithoutFields {-};
    return 0;
end;
