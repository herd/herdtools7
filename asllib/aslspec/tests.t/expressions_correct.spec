typedef N;

typedef Rec =
    | R[rf: N, rg: N]
;

typedef Ctx =
    | [pred: fun N -> Bool]
;

operator assign[T](lhs: T, rhs: T) -> Bool {};
operator reverse_assign[T](lhs: T, rhs: T) -> Bool {};
operator equal[T](lhs: T, rhs: T) -> Bool {};
operator num_minus(lhs: N, rhs: N) -> N {};
operator num_plus(lhs: N, rhs: N) -> N {};
operator num_times(lhs: N, rhs: N) -> N {};
operator num_divide(lhs: N, rhs: N) -> N {};
operator num_exponent(lhs: N, rhs: N) -> N {};
operator and(lhs: Bool, rhs: Bool) -> Bool {};
operator or(lhs: Bool, rhs: Bool) -> Bool {};
operator iff(lhs: Bool, rhs: Bool) -> Bool {};
operator member[T](x: T, s: powerset(T)) -> Bool {};
operator not_member[T](x: T, s: powerset(T)) -> Bool {};
operator less_or_equal(lhs: N, rhs: N) -> Bool {};
operator less_than(lhs: N, rhs: N) -> Bool {};
operator greater_or_equal(lhs: N, rhs: N) -> Bool {};
operator greater_than(lhs: N, rhs: N) -> Bool {};
operator not_equal[T](lhs: T, rhs: T) -> Bool {};
operator if_then_else[T](c: Bool, then_branch: T, else_branch: T) -> T {};
operator cond_case[T](c: Bool, result: T) -> T {};
variadic operator cond_op[T](cases: list1(T)) -> T {};
variadic operator make_set[T](members: list1(T)) -> powerset(T) {};

relation step(x: N) -> N {};

relation expression_forms(
    a: N,
    b: N,
    flag: Bool,
    xs: list0(N),
    r: Rec,
    ctx: Ctx
) -> (out: N, ok: Bool)
{} =
    sum := a + b;
    diff := a - b;
    product := a * b;
    quotient := a / b;
    power := a ^ b;
    chosen := if flag then sum else diff;
    cond_chosen := cond(flag: chosen, True: product);
    tuple_value := (chosen, cond_chosen);
    rec_value := R[rf: chosen, rg: cond_chosen];
    updated := r(rf: rec_value.rg);
    indexed := xs[a + b];
    updated.rf =: reverse_target;
    ctx.pred(indexed);
    flag && True;
    flag || False;
    flag <=> True;
    a in make_set(a, b);
    a not_in make_set(b);
    a <= b;
    a < b;
    a >= b;
    a > b;
    a != b;
    step(a) -> stepped | ;
    INDEX(i, xs: step(xs[i]) -> ys[i]);
    --
    (stepped, flag);
;
