//D_YZBQ: The enumeration type declaration defines a list of enumeration
//literals which act as global constants that can be compared for
//equality and inequality. The type’s domain is the set of enumeration
//literals.

// RUN: interp %s | FileCheck %s

type enum of enumeration{A, B};

func main() => integer
begin
    return 0;
end
