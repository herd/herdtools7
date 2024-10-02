//R_QYBH: Where a parameter is not a formal argument, its value and
//constraints are taken from the actuals which correspond to the parameter’s
//parameter-defining formals.

// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => integer{N}
begin
    return N;
end

func main() => integer
begin
    return 0;
end
