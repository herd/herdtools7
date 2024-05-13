//R_MWBN: The invocation type of a formal argument or return type is its
//declared type after the values and constraints of parameters have been
//applied.

// RUN: interp %s | FileCheck %s

config c: integer{} = 10;

func test(N: integer) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    var a: bits(10) = test(10);
    var b: bits(5) = test(5);

    var cc: bits(c) = test(c);
    return 0;
end
