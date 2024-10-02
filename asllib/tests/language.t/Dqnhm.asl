//D_QNHM: A subprogram invocation is a compile-time-constant invocation
//if all the following hold:
//- the invoked subprogram is a compile-time-constant subprogram
//- all of the actual arguments are compile-time-constant expressions
//- all actual arguments which are bitvectors were declared with a
//constant expression width

// RUN: interp %s | FileCheck %s

func compiletime(a: integer, b: integer)
begin
    var c = 10;
end

func test{N:integer}(a: bits(N))
begin
    pass;
end

func main() => integer
begin
    compiletime(10, 10);
    test(Zeros(10));
    return 0;
end
