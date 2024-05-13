//R_VBLL: Wherever an execution-time check is required, a tool may elide the
//check if it can be proven at compile time to always be true.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a : integer{2} = 2;
    var b = a as integer{0..3};
    return 0;
end
