type MyRecord of record {status: boolean, time: integer, data: bits(8)};

func main() => integer
begin
    var x : MyRecord;
    x.status = TRUE;
    x.time = 0;
    x.data = Ones{8};
    return 0;
end;
