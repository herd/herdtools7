func f(d: bits(2), a: integer) => integer
begin
    // in the following 'd' is evaluated once,
    // if d == '01' and a == 5 then the sub-expression 'a' will be evaluated twice
    case d of
        when '00' => return 1;
        // if d matches '01' then evaluate (a>8), if true then return 9
        when '01' where a > 8 => return 9;
        // if d matches '01' then evaluate (a<3), if true then return 2
        when '01' where a < 3 => return 2;
        when '01' => return 3;
        when '10' => return 4;
        when '11' => return 5;
    end;
end;

func main() => integer
begin
    assert f('01', 2) == 2;
    return 0;
end;
