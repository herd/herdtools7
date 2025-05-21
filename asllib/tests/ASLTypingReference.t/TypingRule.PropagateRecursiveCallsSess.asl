var g1 : integer;
var g2 : integer;
var g3 : integer;

func main() => integer
begin
    - = count_g1(10);
    return 0;
end;

func increment_g3()
begin
    g3 = g3 + 1;
end;

func count_g1(counter: integer) => integer recurselimit 100
begin
    g1 = g1 + 1;
    if counter > 0 then
        - = count_g2(counter - 1);
    end;
    return g1;
end;

func count_g2(counter: integer) => integer recurselimit 100
begin
    g2 = g2 + 1;
    increment_g3();
    - = count_g1(counter - 1);
    return g2;
end;
