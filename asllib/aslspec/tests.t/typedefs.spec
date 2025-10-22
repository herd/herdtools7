// A type with no specification of its domain.
typedef Int { prose_description = "integer" };

// Constants without a type.
constant One { prose_description = "1" };
constant Two { prose_description = "2" };

// A type can be defined as the union (denoted here as '|') of other types.
typedef B { prose_description = "B"} =
    | C
    { prose_description = "the label C" }
    | powerset(Int)
    { prose_description = "any subset of values from Int" }
    | option(Int)
    { prose_description = "either the empty set or a singleton set with an Int value" }
    | list0(Int)
    { prose_description = "a possibly-empty sequence of Int values" }
    | list1(Int)
    { prose_description = "a non-empty sequence of Int values" }
    | (Int)
    { prose_description = "a value from Int" }
    | fun Int -> Int
    { prose_description = "a total function from Int to Int" }
    | partial Int -> Int
    { prose_description = "a partial function from Int to Int" }
    | constants_set(One, Two)
    { prose_description = "the set {One, Two}" }
    | (Int, Int)
    { prose_description = "an ordered pair of Int values" }
    | Rec(Int, Int)
    { prose_description = "a labelled ordered pair of Int values" }
    | [f: Int, g: fun (Int, Int) -> powerset(Int)]
    { prose_description = "a record with two fields" }
    | LRec[f: Rec(Int, Int), g: fun (Int, Int) -> powerset(Int)]
    { prose_description = "a labelled record with two fields" }
;

// A type definition, identical to typedef, except that is is
// rendered in the style of context-free grammars.
ast expr { prose_description = "expression" } =
    | Number(v: Int)
    { prose_description = "a number expression for {v}" }
    | Plus(lhs: expr, rhs: expr)
    { prose_description = "a binary expression for {lhs} and {rhs}" }
;
