//I_FLKF: When a parameter takes its value from one of the actual argument’s
//bitwidths, type satisfaction of all other formals by the actuals will
//ensure that occurrences of the parameter in all other formal bitwidth
//expressions comply with that value.

// RUN: not interp %s | FileCheck %s

func test{N:integer}(a: bits(N), b: bits(N))
begin
    pass;
end

func main() => integer
begin
    test('1', '11');
    return 0;
end
