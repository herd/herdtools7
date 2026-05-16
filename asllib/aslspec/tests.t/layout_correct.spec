typedef LayoutTerm
{ math_layout = _ } =
    | (N)
    { math_layout = _ }
    | (N, (Bool, N))
    { math_layout = (_, (_, _)) }
    | Pair(N, (Bool, N))
    { math_layout = (_, (_, _)) }
    | [lt_f: N, lt_g: (Bool, N)]
    { math_layout = (_, (_, _)) }
    | fun (N, Bool) -> (N, N)
    { math_layout = ((_, _), (_, _)) }
    | powerset((N, Bool))
    { math_layout = (_, _) }
    | constants_set(True, False)
    { math_layout = (_, _) }
;

typedef Ctx
{ math_layout = _ } =
    | [pred: fun N -> Bool]
    { math_layout = _ }
;

typedef RecN
{ math_layout = _ } =
    | [rf: N, rg: N]
    { math_layout = (_, _) }
;

typedef RecPair
{ math_layout = _ } =
    | [pf: N, pg: (N, N)]
    { math_layout = (_, (_, _)) }
;

constant layout_constant_value = (True, False)
{ math_layout = (_, _) };

relation bool_rel(x: N) -> Bool
{};

relation step(x: N) -> N
{};

operator always_true() -> Bool
{};

relation layout_exprs(
    a: N,
    b: N,
    c: N,
    flag: Bool,
    r: RecN,
    xs: list0(N),
    ctx: Ctx
) -> (out1: N, out2: N)
{} =
    flag { _ };
    always_true() { _ };
    not(flag) { _ };
    bool_rel(a + b) { ((_, _)) };
    ctx.pred(a) { (_) };
    [pf: a, pg: (b, c)] = [pf: a, pg: (b, c)]
    { ((_, (_, _)), (_, (_, _))) };
    r(rf: a, rg: b) = r { ((_, (_, _)), _) };
    xs[a + b] = c { ((_, _), _) };
    r.rf = a { (_, _) };
    INDEX(i, xs: step(xs[i]) -> _) { (_, (_, _)) };
    --
    (a, b) { (_, (_, _)) };
;
