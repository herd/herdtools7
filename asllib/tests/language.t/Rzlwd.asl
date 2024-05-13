//R_ZLWD: Where a parameter is not a formal argument, the declared type of
//the parameter must be type-satisfied by an integer type with the
//constraints taken from the invocation’s actuals.

// RUN: not interp %s | FileCheck %s

func test{N: integer{5..10}}(a: bits(N))
begin
    pass;
end

func main() => integer
begin
    test('11');
    return 0;
end
