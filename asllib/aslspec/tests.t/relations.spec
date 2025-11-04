typedef Num {""};

ast type { prose_description = "type"} =
    | Int
    { "integer type" }
    | String
    { "string type" }
;

ast expr { "expression" } =
    | Number(v: Num)
    { "a number expression for {v}" }
    | Plus(lhs: expr, rhs: expr)
    { "a binary expression for {lhs} and {rhs}" }
;

typedef type_error
{
    short_circuit_macro = \TypeErrorConfig,
};

// A relation associates a tuple of types (the input) with an output type.
typing relation annotate_expr(input: expr) -> (inferred_type: type) | type_error
{
    "infers the type {inferred_type} for the expression {input}",
    prose_application = "annotating the expression {input} yields the type {inferred_type}",
};

typing function annotate_plus(input: Plus(lhs: expr, rhs: expr)) -> (inferred_type: type)
{
    "infers the type {inferred_type} for the plus expression {input}",
    prose_application = "annotating the plus expression {input} yields the type {inferred_type}",
};

semantics relation eval_plus(input: Plus(lhs: expr, rhs: expr)) -> (output_val: Num)
{
    prose_description = "evaluates the expression {input} and returns {output_val}",
    prose_application = "",
};
