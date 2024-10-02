//R_LCCN: The base value of an enumeration type is the first (i.e. leftmost)
//enumeration literal in the declaration of that enumeration type.
//  type SigEnum of enumeration {LOW, HIGH};  // SigEnum has base value LOW


// RUN: interp %s | FileCheck %s
// CHECK: TRUE
type enum of enumeration{A, B};

func main() => integer
begin
    var a: enum;
    print((a == A));
    return 0;
end
