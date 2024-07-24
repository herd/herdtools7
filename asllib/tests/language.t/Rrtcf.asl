//R_RTCF: It is a type-checking error if a subprogram invocation does not
//match exactly one subprogram declaration.

// RUN: not interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test(10);
    return 0;
end
