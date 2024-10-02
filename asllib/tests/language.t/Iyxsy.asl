// RUN: interp %s | FileCheck %s

func primitiveConstraint(value: integer {8,16})
begin
    let j = value*(1+1);
    // The expression (1+1) is a compile-time constant
    // hence the type of (1+1) is integer {2}
    // and j is of type integer {16,32}
    var factor: integer = 2; // factor is of type integer
    // Note that without the explicit type, factor would be integer {2}
    let k = value * factor; // k is of type integer
    // since factor is unconstrained
    let c = if (factor==2) then value * 2 else value;
    // The 'then' subexpression is integer {16,32}
    // The 'else' subexpression is integer {8,16}
    // The conditional expression (and hence c)
    // is of type integer {8,16,32}
end

func main() => integer
begin
    return 0;
end
