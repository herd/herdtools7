//R_SPPT: The type of a bit-vector literal is bits(N) where N is the number
//of ‘0’ and ‘1’ characters in the literal.

// RUN: interp %s | FileCheck %s

var a: bits(5) = '11111';

func main() => integer
begin
    return 0;
end
