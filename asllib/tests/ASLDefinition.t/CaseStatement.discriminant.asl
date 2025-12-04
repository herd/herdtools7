var num_tests : integer = 0;

func test_and_increment(x: integer) => integer
begin
    println "num_tests: ", num_tests;
    num_tests = num_tests + 1;
    if x > 100 then
        return x;
    else
        return x + 1;
    end;
end;

func main() => integer
begin
    var x = 50;
    case test_and_increment(x) of
        when 50 => println "selected case x=50";
        when 51 => println "selected case x=51";
        when 52 => println "selected case x=52";
    end;
    return 0;
end;
