//R_TJKQ: Any parameter of a subprogram which is declared as the
//unconstrained integer type shall be treated as though it was declared as
//the under-constrained integer.

// RUN: not interp %s | FileCheck %s

func test(N: integer)
begin
    let illegal : integer {32, 64} = N;
end

func main() => integer
begin
    return 0;
end
