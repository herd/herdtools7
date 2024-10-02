//R_LSNP: A constraint is specified as a list of constraint ranges, where
//each range consists of a single statically evaluable, constrained integer
//expression or a lower and upper bound range (inclusive) using the syntax
//expr .. expr where the bounds are statically evaluable, constrained integer
//expressions.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a: integer{0..10};
    return 0;
end
