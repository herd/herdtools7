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
    var x = 52;
    case test_and_increment(x) of
        when 50 => println "selected case 1";
        when 51 => println "selected case 2";
        when 52 => println "selected case 2";
        otherwise => println "selected otherwise";
    end;
    return 0;
end;
