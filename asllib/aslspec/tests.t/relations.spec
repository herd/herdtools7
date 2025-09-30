ast type { prose_description = "type"} =
    | Int
    { prose_description = "integer type" }
    | String
    { prose_description = "string type" }
;

ast expr { prose_description = "expression" } =
    | Number(v: Int)
    { prose_description = "a number expression for {v}" }
    | Plus(lhs: expr, rhs: expr)
    { prose_description = "a binary expression for {lhs} and {rhs}" }
;

// A relation associates a tuple of types (the input) with an output type.
relation annotate_expr'(input: expr) -> (inferred_type: type)
{
    prose_description = "infers the type {inferred_type} for the expression {input}",
    prose_application = "annotating the expression {input} yields the type {inferred_type}",
};

relation annotate_plus(input: Plus(lhs: expr, rhs: expr)) -> (inferred_type: type)
{
    prose_description = "infers the type {inferred_type} for the plus expression {input}",
    prose_application = "annotating the plus expression {input} yields the type {inferred_type}",
};

