constant FIVE = 5;
constant SEVEN = 7;

func main() => integer
begin
    - = 5 IN { - }; // { }
    - = 5 IN { FIVE }; // { Other(FIVE) }
    - = 5 IN { FIVE..SEVEN }; // { Other(FIVE), Other(SEVEN) }
    - = 5 IN { <= SEVEN }; // { Other(SEVEN) }
    - = 5 IN { >= SEVEN }; // { Other(SEVEN) }
    - = 5 IN { <= FIVE, >= SEVEN }; // { Other(FIVE), Other(SEVEN) }
    - = 5 IN !{ FIVE, SEVEN }; // { Other(SEVEN), Other(FIVE) }
    - = (1, 2) IN { (-, <= FIVE) }; // { Other(FIVE) }
    - = '101' IN { 'x0x' }; // { }
    return 0;
end;
