type MyRecord of record {
    RED: integer,
    GREEN: integer,
    BLUE: integer
};

func main() => integer
begin
    var x: integer;
    x.RED = 42;
    return 0;
end;
