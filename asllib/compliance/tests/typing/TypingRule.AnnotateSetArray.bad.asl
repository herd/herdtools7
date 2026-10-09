type Row of record {
    columns : array[[1]] of integer
};

var rows : array[[2]] of Row;
var current_row : integer = 0;

func select_column() => integer
begin
    current_row = 1;
    return 0;
end;

func main() => integer
begin
    // Does this write to rows[[0]].columns[[0]]
    // or to rows[[1]].columns[[0]]?
    rows[[current_row]].columns[[select_column()]] = 42;
    return 0;
end;
