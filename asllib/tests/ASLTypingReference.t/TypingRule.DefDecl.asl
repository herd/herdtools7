type MyRecord of record{-}; // { Other(MyRecord) }

var g : MyRecord; // { Other(g) }

func main() => integer // { Subprogram(main) }
begin
    return 0;
end;
