//R_VNKT: A statically evaluable expression that must be checked for
//equivalence may contain an integer divide operation (DIV, but not DIVRM)
//only if the divisor is a product of non-zero compile-time-constant integer
//expressions and immutable integer variables.
//Informally, reduction to canonical form involves:
//- Recursively substituting immutable variable identifiers with their
//initialization expression, provided the initialization expression is a
//statically evaluable expression.
//- Applying addition, subtraction, unary negation and exact division (DIV).
//- Distributing addends across multiplication.
//- Removing terms with a zero factor.
//- Cancelling common factors.
//- Sorting addends and factors by the immutable variable identifiers they
//  contain, according to some common total order.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{1..10} = 2;
    var b = (10 DIV a) == 5;
    return 0;
end
