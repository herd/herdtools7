//R_HJYJ: The type of an enumeration literal is the anonymous enumeration
//type which defined the literal.

// RUN: interp %s | FileCheck %s

type enum of enumeration{A, B};

func main() => integer
begin
    var a: enum = A;

    return 0;
end
