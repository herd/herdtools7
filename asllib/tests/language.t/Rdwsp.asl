//R_DWSP: No enumeration literal may explicitly appear in multiple
//enumeration type declarations.

// RUN: not interp %s | FileCheck %s

type enum of enumeration{A, B};
type enum2 of enumeration{A, B};

func main() => integer
begin
    return 0;
end