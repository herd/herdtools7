//I_CMLP: Note that integer formal arguments which are also parameters of
//the subprogram are treated as the under-constrained integer and therefore
//cannot be type-satisfied by an unconstrained integer actual.

// RUN: not interp %s | FileCheck %s

func test(N: integer, a: bits(N))
begin
    pass;
end

func main() => integer
begin
    var a: integer = 4;
    test(a, '1111');
    return 0;
end
