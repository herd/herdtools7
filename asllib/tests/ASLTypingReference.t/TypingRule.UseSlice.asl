constant FIVE = 5;
constant SEVEN = 7;

func main() => integer
begin
    var bv = Ones{64};
    var - = bv[FIVE]; // { Other(FIVE) }
    var - = bv[SEVEN  : FIVE]; // { Other(FIVE), Other(SEVEN) }
    var - = bv[SEVEN +: FIVE]; // { Other(FIVE), Other(SEVEN) }
    var - = bv[SEVEN *: FIVE]; // { Other(FIVE), Other(SEVEN) }
    return 0;
end;
