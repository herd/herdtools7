//I_EOAX: A checked type conversion on the width expression of a bitvector
//type is sufficient to ensure the width is a constrained integer.

// RUN: interp %s | FileCheck %s

config a: integer = 10;

func main() => integer
begin
    var b: bits(a as integer{10});
    return 0;
end
