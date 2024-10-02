//R_ZJKY: The type of an integer compile-time-constant expression is the
//constrained integer type whose constraint holds only the value of the
//expression.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{10} = 10;

    return 0;
end
