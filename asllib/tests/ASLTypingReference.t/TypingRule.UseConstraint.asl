constant FIVE = 5;
constant SEVEN = 7;
let g = 3;

func main() => integer
begin
    var - : integer{FIVE.. SEVEN}; // { Other(FIVE), Other(SEVEN) }
    var - : integer{g}; // { Other(g) }
    return 0;
end;
