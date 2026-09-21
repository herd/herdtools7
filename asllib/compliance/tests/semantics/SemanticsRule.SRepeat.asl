func scan{N}(x: bits(N)) => integer{0..N}
begin
    var res : integer = 0;
    var j: integer = 0;
    repeat
        if x[j] == '1' then
            res = res + 1;
        end;
        println "j = ", j;
        j = j + 1;
    // N is a constrained integer, since N is a parameter,
    // and thus can be used as a limit expression.
    until j == N looplimit N;
    return res as integer{0..N};
end;

func main () => integer
begin
    var x = Ones{5};
    println "#ones in x = ", scan{5}(x);

    var i: integer = 0;
    var ones: integer = 0;
    repeat
        println "i = ", i;
        assert i < 5;
        if x[i] == '1' then
            ones = ones + 1;
        end;
        i = i + 1;
    until i == 5;
    println "#ones in x = ", ones;
    return 0;
end;
