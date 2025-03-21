constant FIVE = 5;
constant SEVEN = 7;

func main() => integer
begin
    var - = 5 IN { - }; // { }
    var - = 5 IN { FIVE }; // { Other(FIVE) }
    var - = 5 IN { FIVE..SEVEN }; // { Other(FIVE), Other(SEVEN) }
    var - = 5 IN { <= SEVEN }; // { Other(SEVEN) }
    var - = 5 IN { >= SEVEN }; // { Other(SEVEN) }
    var - = 5 IN { <= FIVE, >= SEVEN }; // { Other(FIVE), Other(SEVEN) }
    var - = 5 IN !{ FIVE, SEVEN }; // { Other(SEVEN), Other(FIVE) }
    var - = (1, 2) IN { (-, <= FIVE) }; // { Other(FIVE) }
    var - = '101' IN { 'x0x' }; // { }
    return 0;
end;
