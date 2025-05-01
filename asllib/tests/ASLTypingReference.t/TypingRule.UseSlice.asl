constant FIVE = 5;
constant SEVEN = 7;

func main() => integer
begin
    var bv = Ones{64};
    - = bv[FIVE]; // { Other(FIVE) }
    - = bv[SEVEN  : FIVE]; // { Other(FIVE), Other(SEVEN) }
    - = bv[SEVEN +: FIVE]; // { Other(FIVE), Other(SEVEN) }
    - = bv[SEVEN *: FIVE]; // { Other(FIVE), Other(SEVEN) }
    return 0;
end;
