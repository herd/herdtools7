func scan{N}(x: bits(N)) => integer{0..N}
begin
    var res : integer = 0;
    // N is a constrained integer, since N is a parameter,
    // and thus can be used as a limit expression.
    for j = 0 to N - 1 looplimit N + 1 do
        if x[j] == '1' then
            res = res + 1;
        end;
        println("j = ", j);
    end;
    return res as integer{0..N};
end;

func main () => integer
begin
    var x = Ones{5};
    println("#ones in x = ", scan{5}(x));

    var ones: integer = 0;
    for i = 4 downto 0 do
        println("i = ", i);
        assert i < 5;
        if x[i] == '1' then
            ones = ones + 1;
        end;
    end;
    println("#ones in x = ", ones);
    return 0;
end;
