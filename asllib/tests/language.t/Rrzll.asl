//R_RZLL: A global config identifier is initialized after any
//compile-time-constant initializer expressions are evaluated and
//before any execution-time initializer expressions or subprograms are
//evaluated.

// RUN: interp %s | FileCheck %s

constant a = 10 + 10;
config b = 10 + 10;
var c = a + b;

func main() => integer
begin
    return 0;
end
