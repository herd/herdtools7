//R_TCDL: Where a parameter is also a formal argument, the parameter’s value
//and constraints for an invocation of the subprogram are the same as the
//corresponding actual in the invocation.

// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => integer{N}
begin
    return N;
end

func main() => integer
begin
    return 0;
end
