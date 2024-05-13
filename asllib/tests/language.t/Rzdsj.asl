//R_ZDSJ: Integer literals (both decimal and hexadecimal) have constrained
//integer type. The type of an integer literal is the constrained integer
//type whose constraint holds only the value of the literal.

// RUN: interp %s | FileCheck %s

var a : integer{10} = 10;
var b : integer{9} = 0x9;

func main() => integer
begin
    return 0;
end
