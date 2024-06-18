//R_WZVX: An execution-time check is a condition that is evaluated during
//the evaluation of an execution-time initializer expression or subprogram.
//If the condition evaluates to FALSE a dynamic error is generated.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a : integer{0..4} = 4;
    var b = a as integer{0..3};
    return 0;
end

// XFAIL: *
