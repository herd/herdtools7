//R_PFWQ: An invocation is illegal if it provides different values or
//constraints for a particular parameter’s parameter-defining formals.

// RUN: not interp %s | FileCheck %s

func test(N: integer, a: bits(N))
begin
    pass;
end

func main() => integer
begin
    test(10, '1111');
    return 0;
end
