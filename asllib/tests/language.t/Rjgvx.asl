//R_JGVX: Functions must only be used in function invocation expressions. 

// RUN: interp %s | FileCheck %s

func test(a: integer) => integer
begin
    return a + 1;
end

func main() => integer
begin
    var a = test(10);

    return 0;
end