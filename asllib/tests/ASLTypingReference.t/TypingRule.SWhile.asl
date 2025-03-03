func scan{N}(x: bits(N)) => integer{0..N}
begin
    var res : integer = 0;
    var i: integer = 0;
    // N is a constrained integer, since N is a parameter,
    // and thus can be used as a limit expression.
    while i < N looplimit N do
        if x[i] == '1' then
            res = res + 1;
        end;
        i = i + 1;
    end;
    return res as integer{0..N};
end;

func main () => integer
begin
    var x = Ones{20};
    println(scan{20}(x));

    var i: integer = 0;
    var ones: integer = 0;
    while i < 20 do
        assert i < 20;
        if x[i] == '1' then
            ones = ones + 1;
        end;
        i = i + 1;
    end;
    println(ones);
    return 0;
end;
