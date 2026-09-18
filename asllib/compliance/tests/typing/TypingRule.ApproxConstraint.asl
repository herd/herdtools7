func main() => integer
begin
    let x: integer{0..10} = ARBITRARY: integer{0..10};
    let y: integer{20..30} = ARBITRARY: integer{20..30};
    let a: integer{12..15} = ARBITRARY: integer{12..15};
    let b: integer{13..20} = ARBITRARY: integer{13..20};
    let rhs_over_approx: integer{a..b} = ARBITRARY: integer{a..b};
    var lhs_under_approx: integer{x..y} = rhs_over_approx;
    // The constraint `x..y` is underapproximated as `10..20` whereas
    // the constraint `a..b` is overapproximated  as `12..20`.
    var lhs_explicit : integer{10..20} = rhs_over_approx as integer {12..20};
    return 0;
end;
