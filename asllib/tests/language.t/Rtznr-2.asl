//R_TZNR: The domain of a constrained integer type with a constraint that 
//contains at least one constraint range that contains an under-constrained
//integer expression is the domain of the under-constrained integer.

// RUN: interp %s | FileCheck %s

func test{N}(a: bits(N))
begin
    var b: integer{0..N};
    pass;
end

func main() => integer
begin
    return 0;
end
