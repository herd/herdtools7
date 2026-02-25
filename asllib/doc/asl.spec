////////////////////////////////////////////////////////////////////////////////
// This file specifies the types, relations, and inference rules used to define
// the abstract syntax, type rules, and dynamic semantics rules for ASL.
// The definition is in terms of a custom language --- aslspec --- which allows to
// attach information needed to emit LaTeX mathematical descriptions and
// LaTeX prose for the types and rules.
// The document `../aslspec/aslspec.md` is a tutorial for aslspec.

////////////////////////////////////////////////////////////////////////////////
// Generic Types
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////
// Map-related definitions

constant empty_func
{
    "empty function",
    math_macro = \emptyfunc,
};

operator bindings[A,B](f: partial A -> B) -> list0((A, B))
{
  "input-output bindings of {f}",
  math_macro = \bindings,
};

operator bindings_to_map[A,B](bindings: list0((A,B))) -> (partial A -> B)
{
  "function consisting of a binding per pair of {bindings}",
  math_macro = \bindingstomap,
};

operator restrict_map[A,B](f: partial A -> B, dom: powerset(A)) -> (partial A -> B)
{
  "the restriction of {f} to {dom}",
  math_macro = \restrictmapop,
  custom = true,
};

constant None { "the empty \optionalterm{}" };

typedef Bool
{  "Boolean",
    math_macro = \Bool,
} =
  | True
  { "true", math_macro = \True }
  | False
  { "false", math_macro = \False }
;

typedef CheckResult
{
  "check result",
  math_macro = \CheckResult,
} = (True);

typedef Bit
{ "bit" };

typedef N
{  "natural number",
    math_macro = \N,
} = (N_pos);

constant zero : N { math_macro = \zero, };
constant one : N { math_macro = \one, };
constant two : N { math_macro = \two, };
constant rational_zero : Q { math_macro = \zero, };

typedef N_pos
{  "positive natural number",
    math_macro = \Npos,
};

typedef Z
{ "integer",
    math_macro = \Z,
} = (N);

typedef Q
{ "rational",
   math_macro = \Q,
} =
  | Q_nonzero { "non-zero rational", math_macro = \Qnonzero }
;

typedef ascii
{
  "ASCII character",
  math_macro = \REasciichar,
};

typedef Identifier
{
  "identifier",
   math_macro = \Identifier,
} = list0(ascii);

typedef Strings
{ "string",
   math_macro = \Strings,
} =
  // Identifiers are a subset of strings but distinguishing
  // between proper identifiers and non-identifier strings
  // is too complicated to achieve with a type system.
  (Identifier)
;

constant new_line : Strings
{
  math_macro = \vnewline,
};

constant main : Identifier
{
  math_macro = \vmain,
};

typedef ASTLabels
{ "AST label",
   math_macro = \ASTLabels,
};

constant bot { "bottom", math_macro = \bot };

// Note: This is a simplification of the data type used
// by the implementation. Specifically, the scope field
// in the implementation can be derived from the name,
// and is thus dropped.
// Further, we replace the implementation record with a pair.
typedef TAbsField { "\absolutebitfields", math_macro = \TAbsField } =
    (name: list0(Identifier), slice: list0(Z))
    { "absolute field named {name} with slice {slice}" }
;

typedef def_use_name { "subprogram identifier kind" } =
    | Subprogram(id: Identifier)
    { "subprogram identifier {id}" }
    | Other(id: Identifier)
    { "non-subprogram identifier {id}" }
;

////////////////////////////////////////////////////////////////////////////////
// Operator definitions
// Some of the operators below will be removed once type parameters
// are made available to ordinary relations.

operator fresh_identifier() -> Identifier
{
  math_macro = \freshidentifier,
};

// Converts a list to the sequence of its indices, starting from 0.
operator indices[T](l: list0(T)) -> (indices: list0(N))
{
  "the list of indices for {l}",
  math_macro = \indicesop,
};

operator ast_label[T](T) -> ASTLabels
{
  math_macro = \astlabelop,
};

typedef TStructured = (T_Record(list0(field))) | (T_Exception(list0(field))) | (T_Collection(list0(field)));
operator make_structured(l: ASTLabels, fields: list0(field)) -> TStructured
{
  math_macro = \makestructured,
  custom = true,
};

operator map_update[K,V](partial K -> V, K, V) -> (partial K -> V)
{
  math_macro = \mapupdate,
  custom = true,
};

operator dom[K,V](partial K -> V) -> powerset(K)
{
  math_macro = \domop,
};

operator map_apply_opt[K,V](partial K -> V, K) -> option(V)
{
  custom = true,
  math_macro = \mapapplyoptop,
};

operator map_apply[K,V](partial K -> V, K) -> V
{
  custom = true,
  math_macro = \mapapplyop,
};

operator sort[T](unsorted: list0(T), comparator: fun T -> Sign) -> list0(T)
{
  "the list {unsorted} sorted according to {comparator}",
  math_macro = \sortop,
  custom = true,
};

operator sign(q: Q) -> Sign
{
  custom = true,
  math_macro = \signop,
};

operator equal[T](a: T, b: T) -> (c: Bool)
{
  math_macro = \equal,
  prose_application = "equating {a} to {b} yields {c}",
};

operator not_equal[T](T, T) -> Bool
{
  math_macro = \notequal,
};

operator if_then_else[T](Bool, T, T) -> T
{
  math_macro = \ifthenelseop,
};

operator some[T](T) -> option(T)
{
  math_macro = \some,
};

////////////////////////////////////////
// Set-related definitions

constant empty_list
{
    "the empty list",
    math_macro = \emptylist,
};

operator list_map[A,B](bound_variable: A, elements: list0(A), mapped_elem: B) -> (new_elements: list0(B))
{
  "a list where for each binding of {bound_variable} to an element of {elements} in order of appearance,
   {new_elements} has the corresponding element {mapped_elem}",
  math_macro = \listmap,
};

operator list_filter[T](bound_variable: T, elements: list0(T), condition: Bool) -> (new_elements: list0(T))
{
  "the sublist of {elements} for which {condition} holds",
  math_macro = \listfilter,
};

operator filter_option_list[T](elements: list0(option(T))) -> (new_elements: list0(T))
{
  "the non-$\None$ elements of {elements} in the order of appearance",
  math_macro = \filteroptionlist,
};

// Constructs a list out of a finite number of arguments.
variadic operator make_list[T](list1(T)) -> list1(T)
{
  math_macro = \makelist,
};

operator make_singleton_list[T](T) -> list1(T)
{
  math_macro = \makelist,
};

operator match_singleton_list[T](T) -> list0(T)
{
  math_macro = \makelist,
};

operator range_list(from: Z, to: Z) -> list1(Z)
{
  "the list of values between {from} and {to}, inclusive, if {from} is less than {to},
   and the list of values between {to} and {from}, inclusive, otherwise.",
  math_macro = \rangelistop,
  custom = true,
};

operator list_len[T](list0(T)) -> N
{
  math_macro = \listlen,
};

operator same_length[A,B](lst_a: list0(A), lst_b: list0(B)) -> Bool
{
  math_macro = \samelengthop,
  custom = true,
};

// Concatenates a fixed number of lists into a single list.
variadic operator concat[T](lists: list0(list0(T))) -> list0(T)
{
  associative = true,
  math_macro = \concat,
};

// Flattens a list of lists into one list that concatenates
// all of the input lists.
operator list_flatten[T](ll: list0(list0(T))) -> list0(T)
{
  "flattening of the list of lists {ll}",
  math_macro = \concatlist,
};

operator list_set[T](s: powerset(T)) -> list0(T)
{
  "listing the elements of set {s}",
  math_macro = \listset,
};

operator cons[T](T, list0(T)) -> list1(T)
{
  math_macro = \cons,
};

// The output type has to be list0, not list1,
// since we are matching the output to deconstruct it.
operator match_cons[T](head: T, tail: list0(T)) -> list0(T)
{
  "a non-empty list with head {head} and tail {tail}",
  math_macro = \cons,
};

operator match_non_empty_cons[T](head: T, tail: list0(T)) -> list1(T)
{
  "a non-empty list with head {head} and tail {tail}",
  math_macro = \cons,
};

operator list_combine[A,B](list0(A), list0(B)) -> list0((A, B))
{
  math_macro = \listcombine,
  custom = true,
};

operator list_cross[A,B](list0(A), list0(B)) -> list0((A, B))
{
  math_macro = \listcrossop,
};

operator list_fst[A,B](list0((A, B))) -> list0(A)
{
  math_macro = \listfst,
};

operator list_combine_three[A,B,C](list0(A), list0(B), list0(C)) -> list0((A, B, C))
{
  math_macro = \listcombinethree,
  custom = true,
};

operator list_max[T](list0(T)) -> N
{
  math_macro = \listmax,
};

operator assoc_opt[T](list0((Identifier, T)), Identifier) -> option(T)
{
  math_macro = \assocopt,
  custom = true,
};

operator listprefix[T](l1: list0(T), l2: list0(T)) -> Bool
{
  "checks whether {l1} is a prefix of {l2}",
  math_macro = \listprefix,
  custom = true,
};

// A type conversion in case we know that the input
// list must be non-empty.
operator match_non_empty_list[T](list0(T)) -> list1(T)
{
  math_macro = \identityop,
  typecast = true,
};

// Constructs a set out of a fixed list of expressions.
variadic operator make_set[T](list1(T)) -> powerset_finite(T)
{
  math_macro = \makeset,
};

variadic operator match_set[T](list1(T)) -> powerset(T)
{
  math_macro = \makeset,
};

operator list_to_set[T](s: list0(T)) -> powerset_finite(T)
{
  "list {s} viewed as a set",
  math_macro = \listassetop,
};

operator range(from: Z, to: Z) -> list1(Z)
{
  "the set of values between {from} and {to}, inclusive, if {from} is less than {to},
   and the set of values between {to} and {from}, inclusive, otherwise.",
  math_macro = \rangeop,
  custom = true,
};

////////////////////////////////////////
// Set-related definitions

constant empty_set
{
    "empty set",
    math_macro = \emptyset,
};

operator set_as_finite[T](s: powerset(T)) -> powerset_finite(T)
{
  "{s}",
  math_macro = \identityop,
  typecast = true,
};

operator set_from_list[A,B](bound_variable: A, elements: list0(A), mapped_elem: B) -> (new_elements: powerset(B))
{
  "a set where for each binding of {bound_variable} to an element of {elements},
   {new_elements} has the corresponding element {mapped_elem}",
  math_macro = \setfromlist,
};

operator range_set(low: Z, high: Z) -> powerset_finite(Z)
{
  "the set of values between {low} and {high}, inclusive, assuming {low} is less than or equal to {high}",
  math_macro = \rangesetop,
  custom = true,
};

// The size of a finite set.
operator cardinality[T](powerset(T)) -> N
{
  math_macro = \cardinality,
};

operator member[T](x: T, s: powerset(T)) -> Bool
{
  math_macro = \member,
};

operator not_member[T](x: T, s: powerset(T)) -> Bool
{
  math_macro = \notmember,
};

operator subseteq[T](A: powerset(T), B: powerset(T)) -> Bool
{
  math_macro = \subseteq,
};

variadic operator union[T](sets: list1(powerset(T))) -> powerset(T)
{
  "the union of {sets}",
  math_macro = \cup,
  associative = true,
};

variadic operator disjoint_union[T](list1(powerset(T))) -> powerset(T)
{
  math_macro = \disjointunion,
  associative = true,
};

variadic operator union_finite[T](list1(powerset_finite(T))) -> powerset_finite(T)
{
  math_macro = \cup,
  associative = true,
};

operator union_list[T](list0(powerset(T))) -> powerset(T)
{
  math_macro = \unionlistop,
};

operator union_list_finite[T](list0(powerset(T))) -> powerset_finite(T)
{
  math_macro = \unionlistop,
};

variadic operator intersect[T](list0(powerset(T))) -> powerset(T)
{
  math_macro = \cap,
  associative = true,
};

variadic operator intersect_finite[T](list0(powerset_finite(T))) -> powerset_finite(T)
{
  math_macro = \cap,
  associative = true,
};

operator set_min[T](powerset(T)) -> N
{
  math_macro = \setmin,
};

operator set_max[T](powerset(T)) -> N
{
  math_macro = \setmax,
};

operator as_rational(Z) -> Q
{
  math_macro = \identityop,
  typecast = true,
};

operator round_up(Q) -> N
{
  math_macro = \roundup,
};

operator round_down(Q) -> N
{
  math_macro = \rounddown,
};

operator fraction(a: Z, b: Z) -> Q
{
  "the fraction for {a}/{b}",
  math_macro = \fractionop,
  custom = true,
};

operator n_to_n_pos(n: N) -> N_pos
{
  "casts {n} to a positive natural number",
  math_macro = \identityop,
  typecast = true,
};

operator z_to_n(z: Z) -> N
{
  "{z}",
  math_macro = \identityop,
  typecast = true,
};

operator abs_value(Z) -> N
{
  math_macro = \absvalueop,
};

operator numbered_identifier(prefix: Identifier, n: N) -> (result: Identifier)
{
  "concatenates {prefix} and the string for {n} to yield {result}",
  math_macro = \concatstrings,
  associative = true,
};

operator negate_bit(b: Bit) -> Bit
{
  "negation of {b}",
  math_macro = \negatebit,
  custom = true,
};

operator and_bit(a: Bit, b: Bit) -> Bit
{
  "bit-level conjunction of {a} and {b}",
  math_macro = \land,
};

operator or_bit(a: Bit, b: Bit) -> Bit
{
  "bit-level disjunction of {a} and {b}",
  math_macro = \lor,
};

////////////////////////////////////////
// Execution graph operators

operator ReadEffect(x: Identifier) -> (N, read: effect_type, Identifier)
{
  math_macro = \ReadEffectop,
};

operator WriteEffect(x: Identifier) -> (N, write: effect_type, Identifier)
{
  math_macro = \WriteEffectop,
};

variadic operator parallel(list1(XGraphs)) -> XGraphs
{
  math_macro = \parallelcomp,
  associative = true,
};

operator parallel_graphs(gs: list0(XGraphs)) -> (g: XGraphs)
{
  "the parallel composition of all graphs in the list {gs}",
  math_macro = \parallelgraphs,
};

variadic operator ordered_data(list1(XGraphs)) -> XGraphs
{
  associative = true,
  math_macro = \ordereddata,
};

variadic operator ordered_ctrl(list1(XGraphs)) -> XGraphs
{
  associative = true,
  math_macro = \orderedctrl,
};

variadic operator ordered_po(list1(XGraphs)) -> XGraphs
{
  associative = true,
  math_macro = \orderedpo,
};

operator graph_of[T](T) -> XGraphs
{
  math_macro = \graphof,
};

operator with_graph[T](T, XGraphs) -> T
{
  custom = true,
  math_macro = \withgraph,
};

operator environ_of[T](T) -> envs
{
  math_macro = \environof,
};

operator with_environ[T](T, envs) -> T
{
  custom = true,
  math_macro = \withenviron,
};

operator ReturningConfig() -> TReturning
{
  math_macro = \ReturningConfig,
};

operator ThrowingConfig() -> TThrowing
{
  math_macro = \ThrowingConfig,
};

operator DynErrorConfig() -> TDynError
{
  math_macro = \DynErrorConfig,
};

operator DivergingConfig() -> TDiverging
{
  math_macro = \DivergingConfig,
};

operator nvbool(b: Bool) -> NV_Literal(L_Bool(Bool))
{
  math_macro = \nvboolop,
};

operator nvint(z: Z) -> NV_Literal(L_Int(Z))
{
  math_macro = \nvintop,
};

operator nvbitvector(bits: list0(Bit)) -> NV_Literal(L_Bitvector(list0(Bit)))
{
  math_macro = \nvbitvectorop,
};

operator nvstring(s: Strings) -> NV_Literal(L_String(Strings))
{
  math_macro = \nvstringop,
};

////////////////////////////////////////
// AST abbreviations

constant unconstrained_integer : ty
{
  "unconstrained integer",
  math_macro = \unconstrainedinteger
};

operator ELint(Z) -> expr
{
  math_macro = \ELInt,
};

operator AbbrevEBinop(op: binop, lhs: expr, rhs: expr) -> expr
{
  math_macro = \AbbrevEBinop,
};

operator AbbrevECond(e_cond: expr, e_true: expr, e_false: expr) -> expr
{
  math_macro = \AbbrevECond,
};

operator AbbrevConstraintRange(e1: expr, e2: expr) -> int_constraint
{
  "range constraint for {e1} and {e2}",
  math_macro = \AbbrevConstraintRangeOp,
  custom = true,
};

operator AbbrevConstraintExact(e: expr) -> int_constraint
{
  "exact constraint for {e}",
  math_macro = \AbbrevConstraintExactOp,
};

////////////////////////////////////////////////////////////////////////////////
// Syntax-related definitions
typedef regex { "regular expression" };

constant int_lit_regex : regex
{
  "regular expression for integer literals",
  math_macro = \REintlit,
};

operator Lang(r : regex) -> (l: powerset(Strings))
{
  "{l} is the set of strings defined by {r}",
  math_macro = \Lang,
};

////////////////////////////////////////////////////////////////////////////////
// Types for Symbolic Equivalence Testing
constant negative_sign : Sign { "negative sign", math_macro = \negativesign };
constant positive_sign : Sign { "positive sign", math_macro = \positivesign };
constant equal_sign : Sign { "equal sign", math_macro = \equalsign };
typedef Sign { "sign" } =
    constants_set(negative_sign, positive_sign, equal_sign)
;

typedef unitary_monomial { "unitary monomial" } = partial Identifier -> N_pos;
typedef polynomial { "polynomial" } = partial unitary_monomial -> Q;
typedef monomial { "monomial" } = (exponents: unitary_monomial, factor:Q);
render symbolic_expressions = polynomial(-), unitary_monomial(-),  monomial(-);
constant CannotBeTransformed { "cannot be transformed", math_macro = \CannotBeTransformed };

////////////////////////////////////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////////////////////////////////////

ast literal { "literal" } =
    | L_Int(whole_number: Z)
    { "integer literal for {whole_number}" }
    | L_Bool(boolean_value: Bool)
    { "Boolean literal for {boolean_value}" }
    | L_Real(rational_number: Q)
    { "rational literal for {rational_number}" }
    | L_Bitvector(bits: list0(Bit))
    { "bitvector literal for {bits}" }
    | L_String(string: Strings)
    { "string literal for {string}" }
    | L_Label(enumeration_label: Identifier)
    { "enumeration label {enumeration_label}" }
;

constant label_L_Int : ASTLabels { math_macro = \LInt };
constant label_L_Bool : ASTLabels { math_macro = \LBool };
constant label_L_Real : ASTLabels { math_macro = \LReal };
constant label_L_Bitvector : ASTLabels { math_macro = \LBitvector };
constant label_L_String : ASTLabels { math_macro = \LString };
constant label_L_Label : ASTLabels { math_macro = \LLabel };

ast unop { "unary operator" } =
    | BNOT
    { "Boolean negation operator" }
    | NEG
    { "integer negation operator" }
    | NOT
    { "bitvector negation operator" }
;

ast binop { "binary operator" } =
    | BAND
    { "Boolean conjunction operator" }
    | BOR
    { "Boolean disjunction operator" }
    | IMPL
    { "Boolean implication operator" }
    | BEQ
    { "Boolean bi-implication operator" }
    | EQ
    { "equality operator" }
    | NE
    { "inequality operator" }
    | GT
    { "greater than operator" }
    | GE
    { "greater than or equal to operator" }
    | LT
    { "less than operator" }
    | LE
    { "less than or equal to operator" }
    | ADD
    { "addition operator" }
    | SUB
    { "subtraction operator" }
    | OR
    { "bitvector or operator" }
    | XOR
    { "bitvector xor operator" }
    | AND
    { "bitvector and operator" }
    | MUL
    { "multiplication operator" }
    | DIV
    { "exact division operator" }
    | DIVRM
    { "rounding division operator" }
    | MOD
    { "modulus operator" }
    | SHL
    { "shift left operator" }
    | SHR
    { "shift right operator" }
    | RDIV
    { "rational division operator" }
    | POW
    { "exponentiation operator" }
    | BV_CONCAT
    { "bitvector concatenation operator" }
    | STR_CONCAT
    { "string concatenation operator" }
;

render unop_and_binop = unop(-), binop(-);

ast expr { "expression" } =
////////////////////////////////////////////////
// Unyped AST
////////////////////////////////////////////////
    | E_Literal(value: literal)
    { "literal expression for {value}" }
    | E_Var(name: Identifier)
    { "variable expression for {name}" }
    | E_ATC(source: expr, type: ty)
    { "asserting type conversion for the source expression {source} and type {type}" }
    | E_Binop(binary_operator: binop, left: expr, right: expr)
    { "binary expression for the operator {binary_operator}, left expression {left} and right expression {right}" }
    | E_Unop(unary_operator: unop, subexpression: expr)
    { "unary expression for the unary operator {unary_operator} and subexpression {subexpression}" }
    | E_Call(call_descriptor: call)
    { "call expression for the call descriptor {call_descriptor}" }
    | E_Slice(base: expr, slices: list0(slice))
    { "slice expression for the base expression {base} and slices {slices}" }
    | E_Cond(test: expr, true_branch: expr, false_branch: expr)
    { "condition expression with test {test}, true branch {true_branch}, and false branch {false_branch}", }
    | E_GetArray(base: expr, index: expr)
    { "array read expression for the base expression {base} and index expression {index}" }
    | E_GetField(record: expr, field_name: Identifier)
    { "field read expression for the record expression {record} and field name {field_name}" }
    | E_GetFields(record: expr, field_names: list0(Identifier))
    { "multi-field read expression for the record expression {record} and field names {field_names}" }
    | E_Record(record_type: ty, field_initializers: list0( (field_name: Identifier, initializer: expr) ))
    { "a record construction expression for the record type {record_type} and field initializers {field_initializers}" }
    | E_Tuple(components: list1(expr))
    { "a tuple expression for the components {components}" }
    | E_Arbitrary(type: ty)
    { "an arbitrary value choice expression for {type}" }
    | E_Pattern(discriminant: expr, pattern: pattern)
    { "a pattern expression for {discriminant} and {pattern}" }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////
    | E_GetItem(base: expr, index: N)
    { "an access to tuple expression {base} of the component at index {index}" }
    | E_Array[
        length: expr,
        array_value: expr { math_macro = \arrayvalue }
      ]
    { "array construction for an array of length given by {length} with all cells initialized with {array_value}" }
    | E_EnumArray[
        enum: Identifier,
        labels: list1(Identifier),
        enum_array_value: expr { math_macro = \enumarrayvalue }
      ]
    { "array construction for an array associating each label in {labels} with the value given by {enum_array_value}" }
    | E_GetEnumArray(base: expr, key: expr)
    { "access to enumeration-indexed array {base} with key expression {key}" }
    | E_GetCollectionFields(collection_name: Identifier, field_names: list0(Identifier))
    { "access to the list of fields given by {field_names} of the collection variable named {collection_name}" }
;

render untyped_expr = expr(
    E_Literal,
    E_Var,
    E_ATC,
    E_Binop,
    E_Unop,
    E_Call,
    E_Slice,
    E_Cond,
    E_GetArray,
    E_GetField,
    E_GetFields,
    E_Record,
    E_Tuple,
    E_Arbitrary,
    E_Pattern,
);

render expr_literal = expr(E_Literal);
render expr_var = expr(E_Var);
render expr_atc = expr(E_ATC);
render expr_binop = expr(E_Binop);
render expr_unop = expr(E_Unop);
render expr_call = expr(E_Call), call(-);
render expr_slice = expr(E_Slice);
render expr_cond = expr(E_Cond);
render expr_getarray = expr(E_GetArray);
render expr_getfield = expr(E_GetField);
render expr_getfields = expr(E_GetFields);
render expr_record = expr(E_Record);
render expr_tuple = expr(E_Tuple);
render expr_arbitrary = expr(E_Arbitrary);
render expr_pattern = expr(E_Pattern);

render typed_expr { lhs_hypertargets = false } = expr(E_GetItem, E_Array, E_EnumArray, E_GetEnumArray, E_GetCollectionFields);
render expr_array = expr(E_Array, E_EnumArray);

constant zero_bit : Bit
{ "\texttt{0}", math_macro = \zerobit };

constant one_bit : Bit
{ "\texttt{1}", math_macro = \onebit };

constant x_bit
{ "\texttt{x}", math_macro = \xbit };

ast pattern { "pattern" } =
    | Pattern_All
    { "match-all pattern" }
    | Pattern_Any(patterns: list0(pattern))
    { "match-any pattern" }
    | Pattern_Geq(subexpression: expr)
    { "greater-or-equal pattern" }
    | Pattern_Leq(subexpression: expr)
    { "less-or-equal pattern" }
    | Pattern_Mask(mask_constant: list0(constants_set(zero_bit, one_bit, x_bit)))
    { "mask pattern" }
    | Pattern_Not(subpattern: pattern)
    { "negation pattern" }
    | Pattern_Range(lower: expr, upper: expr)
    { "range pattern" }
    | Pattern_Single(subexpression: expr)
    { "single-expression pattern" }
    | Pattern_Tuple(patterns:list0(pattern))
    { "tuple pattern" }
;

ast slice
{ "slice" } =
////////////////////////////////////////
// Untyped AST
////////////////////////////////////////
    | Slice_Single(index: expr)
    { "slice at position {index}" }
    | Slice_Range(upper_index: expr, lower_index: expr)
    { "slice from position {upper_index} down to position {lower_index}" }
    | Slice_Length(start_index: expr, length: expr)
    { "slice from position {start_index} of {length} elements" }
    | Slice_Star(factor: expr, scale: expr)
    { "slice from position {factor}*{scale} of {scale} elements" }

////////////////////////////////////////
// Typed AST
////////////////////////////////////////
    | typed_Slice_Length(start_index: expr, length: expr)
    {
        "slice from position {start_index} of {length} elements",
        math_macro = \typedSliceLength,
    }
;

render untyped_slice = slice(
    Slice_Single,
    Slice_Range,
    Slice_Length,
    Slice_Star,
);

render typed_slice { lhs_hypertargets = false } = slice(typed_Slice_Length);

ast call { "call descriptor" } =
    [   call_name: Strings { math_macro = \callname },
        params: list0(expr),
        call_args: list0(expr) { math_macro = \callargs },
        call_type: subprogram_type,
    ]
    { "call of {call_type} subprogram {call_name} with parameters {params}, arguments {call_args}" }
;

render calls = expr(E_Call), stmt(S_Call);

ast ty { "type" } =
    | T_Int(kind: constraint_kind)
    { "integer type" }
    | T_Real
    { "real type" }
    | T_String
    { "string type" }
    | T_Bool
    { "Boolean type" }
    | T_Bits(width: expr, bitfields: list0(bitfield))
    { "bitvector type of bitwidth {width} and bitfields {bitfields}" }
    | T_Tuple(component_types: list0(ty))
    { "tuple type with component types {component_types}" }
    | T_Array(index: array_index, element_type: ty)
    { "array type with {index} and element_type {element_type}" }
    | T_Named(type_name: Identifier)
    { "named type with name {type_name}" }
    | T_Enum(labels: list1(Identifier))
    { "enumeration type with labels {labels}" }
    | T_Record(fields: list0(field))
    { "record type with fields {fields}" }
    | T_Exception(fields: list0(field))
    { "exception type with fields {fields}" }
    | T_Collection(fields: list0(field))
    { "collection type with fields {fields}" }
;

constant label_T_Int : ASTLabels { math_macro = \TInt };
constant label_T_Real : ASTLabels { math_macro = \TReal };
constant label_T_String : ASTLabels { math_macro = \TString };
constant label_T_Bool : ASTLabels { math_macro = \TBool };
constant label_T_Bits : ASTLabels { math_macro = \TBits };
constant label_T_Tuple : ASTLabels { math_macro = \TTuple };
constant label_T_Array : ASTLabels { math_macro = \TArray };
constant label_T_Named : ASTLabels { math_macro = \TNamed };
constant label_T_Enum : ASTLabels { math_macro = \TEnum };
constant label_T_Record : ASTLabels { math_macro = \TRecord };
constant label_T_Exception : ASTLabels { math_macro = \TException };
constant label_T_Collection : ASTLabels { math_macro = \TCollection };

ast constraint_kind { "constraint kind" } =
//////////////////////////////////////////////////
// Untyped AST
//////////////////////////////////////////////////
    | Unconstrained
    { "no constraint" }
    | WellConstrained(constraints: list1(int_constraint))
    { "list of constraints {constraints}" }
    | Parameterized(parameter_name: Identifier)
    { "parameter constraint for {parameter_name}" }
    | PendingConstrained
    { "pending constraint" }

//////////////////////////////////////////////////
// Typed AST
//////////////////////////////////////////////////
    | typed_WellConstrained(constraints: list1(int_constraint), precision_loss: precision_loss_indicator)
    {
        "list of constraints {constraints} with a \Proseprecisionlossindicator{} {precision_loss}",
        math_macro = \typedWellConstrained,
        math_layout = [_,_],
    }
;

render untyped_constraint_kind = constraint_kind(
    Unconstrained,
    WellConstrained,
    Parameterized,
    PendingConstrained,
);
constant label_Unconstrained : ASTLabels { math_macro = \Unconstrained };
constant label_WellConstrained : ASTLabels { math_macro = \WellConstrained };
constant label_Parameterized : ASTLabels { math_macro = \Parameterized };
constant label_PendingConstrained : ASTLabels { math_macro = \PendingConstrained };

render typed_constraint_kind { lhs_hypertargets = false } = constraint_kind(typed_WellConstrained), precision_loss_indicator(-);

ast precision_loss_indicator { "\Proseprecisionlossindicator{}" } =
    | Precision_Full
    { "no precision loss" }
    | Precision_Lost
    { "some precision loss" }
;

render ty_int_constraint_and_kind = ty(T_Int), int_constraint(-), constraint_kind(-);

ast int_constraint { "integer constraint" } =
    | Constraint_Exact(subexpression: expr)
    { "exact constraint for the subexpression {subexpression}" }
    | Constraint_Range(start_expression: expr, end_expression: expr)
    { "range constraint from the start expression {start_expression} to the end expression {end_expression}" }
;

ast bitfield { "bitfield" } =
    | BitField_Simple(name: Identifier, slices: list0(slice))
    { "bitfield named {name} with slices {slices}" }
    | BitField_Nested(name: Identifier, slices: list0(slice), nested_bitfields: list0(bitfield))
    { "bitfield named {name} with slices {slices} and nested bitfields {nested_bitfields}" }
    | BitField_Type(name: Identifier, slices: list0(slice), type: ty)
    { "bitfield named {name} with slices {slices} and type {type}" }
;

constant label_BitField_Simple : ASTLabels { math_macro = \BitFieldSimple };
constant label_BitField_Nested : ASTLabels { math_macro = \BitFieldNested };
constant label_BitField_Type : ASTLabels { math_macro = \BitFieldType };

ast array_index { "array index" } =
//////////////////////////////////////////////////
// Untyped AST
//////////////////////////////////////////////////
    ArrayLength_Expr(length: expr)
    { "integer length expression {length}" }

//////////////////////////////////////////////////
// Typed AST
//////////////////////////////////////////////////
    | ArrayLength_Enum(enumeration_name: Identifier, enumeration_labels: list1(Identifier))
    { "index for the enumeration {enumeration_name} with labels {enumeration_labels}" }
;

constant label_E_Arbitrary : ASTLabels { math_macro = \ELiteral };
constant label_ArrayLength_Expr : ASTLabels { math_macro = \ArrayLengthExpr };
constant label_E_Call : ASTLabels { math_macro = \ECall };
constant label_E_Literal : ASTLabels { math_macro = \ELiteral };
constant label_E_Var : ASTLabels { math_macro = \EVar };
constant label_E_ATC : ASTLabels { math_macro = \EATC };
constant label_E_Binop : ASTLabels { math_macro = \EBinop };
constant label_E_Unop : ASTLabels { math_macro = \EUnop };
constant label_E_Cond : ASTLabels { math_macro = \ECond };
constant label_E_Tuple : ASTLabels { math_macro = \ETuple };

render untyped_array_index = array_index(ArrayLength_Expr);
render typed_array_index = array_index(ArrayLength_Enum);

ast field { "field" } =
    (name: Identifier, type: ty)
    { "field named {name} with type {type}" }
;

ast typed_identifier { "typed identifier" } =
    (name: Identifier, type: ty)
    { "identifier {name} with type {type}" }
;

ast lexpr { "\assignableexpression{}" } =
////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////
    | LE_Discard
    { "discarding \assignableexpression{}" }
    | LE_Var(var: Identifier)
    { "assignable variable expression for {var}" }
    | LE_Slice(base: lexpr, slices: list0(slice))
    { "assignable slice expression for {base} and {slices}" }
    | LE_SetArray(base: lexpr, index: expr)
    { "assignable array write expression for {base} at index {index}" }
    | LE_SetField(base: lexpr, field_name: Identifier)
    { "assignable field write expression for {base} and field name {field_name}" }
    | LE_SetFields(base: lexpr, field_names: list0(Identifier))
    { "assignable multi-field write expression for {base} and field names {field_names}" }
    | LE_Destructuring(subexpressions: list0(lexpr))
    { "multi-assignment for the list of \assignableexpressions{} {subexpressions}" }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////

    | LE_SetEnumArray(base: lexpr, index: expr)
    { "assignable expression for the enumeration-indexed array {base} at index {index}" }
    | LE_SetCollectionFields(collection_name: Identifier, field_names: list0(Identifier), slices: list0((Z, Z)))
    { "assignable expression for the collection named {collection_name}, field names {field_names}, and inferred slices {slices}", }
    | typed_LE_SetFields(base: lexpr, field_names: list0(Identifier), slices: list0((Z, Z)))
    {
      "assignable multi-field write expression for {base}, field names {field_names}, and inferred slices {slices}",
      math_macro = \typedLESetFields,
    }
;

constant label_LE_Destructuring : ASTLabels { math_macro = \LEDestructuring };
constant label_LE_Var : ASTLabels { math_macro = \LEVar };
constant label_LE_Discard : ASTLabels { math_macro = \LEDiscard };

render untyped_lexpr = lexpr(
    LE_Discard,
    LE_Var,
    LE_Slice,
    LE_SetArray,
    LE_SetField,
    LE_SetFields,
    LE_Destructuring,
);
render typed_lexpr { lhs_hypertargets = false } = lexpr(LE_SetEnumArray, LE_SetCollectionFields, typed_LE_SetFields);

render lexpr_discard = lexpr(LE_Discard);
render lexpr_var = lexpr(LE_Var);
render lexpr_slice = lexpr(LE_Slice);
render lexpr_setarray = lexpr(LE_SetArray);
render lexpr_setfield = lexpr(LE_SetField);
render lexpr_setfields = lexpr(LE_SetFields);
render lexpr_destructuring = lexpr(LE_Destructuring);
render lexpr_setarray_and_typed = lexpr(LE_SetArray, LE_SetEnumArray);

render pattern_all = pattern(Pattern_All);
render pattern_single = pattern(Pattern_Single);
render pattern_range = pattern(Pattern_Range);
render pattern_leq = pattern(Pattern_Leq);
render pattern_geq = pattern(Pattern_Geq);
render pattern_mask = pattern(Pattern_Mask);
render pattern_tuple = pattern(Pattern_Tuple);
render pattern_any = pattern(Pattern_Any);
render pattern_not = pattern(Pattern_Not);

render ty_real = ty(T_Real);
render ty_string = ty(T_String);
render ty_bool = ty(T_Bool);
render ty_bits = ty(T_Bits);
render ty_tuple = ty(T_Tuple);
render ty_enum = ty(T_Enum);
render ty_array = ty(T_Array);
render ty_record = ty(T_Record);
render ty_exception = ty(T_Exception);
render ty_collection = ty(T_Collection);
render ty_named = ty(T_Named);

ast local_decl_keyword { "local declaration keyword" } =
    | LDK_Var
    { "local variable" }
    | LDK_Let
    { "local immutable variable" }
;

ast local_decl_item { "local declaration item" } =
  | LDI_Var(variable_name: Identifier)
  { "local declaration item for the variable {variable_name}" }
  | LDI_Tuple(variable_names: list0(Identifier))
  { "local declaration item for the list of variables {variable_names}" }
;

render local_decl_keyword_and_item = local_decl_keyword(-), local_decl_item(-);

ast for_direction { "direction" } =
    | UP
    { "upward" }
    | DOWN
    { "downward" }
;

ast stmt { "statement" } =
////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////
  | S_Pass
  { "pass statement" }
  | S_Seq(first: stmt, second: stmt)
  { "sequence statement for {first} and {second}" }
  | S_Decl(keyword: local_decl_keyword, item: local_decl_item, type_annotation: option(ty), initializer: option(expr))
  { "declaration statement with keyword {keyword},
    item {item},
    optional type annotation {type_annotation}, and
    optional initializer {initializer}" }
  | S_Assign(left_hand_side: lexpr, right_hand_side: expr)
  { "assignment statement of \assignableexpression{} {left_hand_side} by {right_hand_side}" }
  | S_Call(call_descriptor: call)
  { "call statement with descriptor {call_descriptor}" }
  | S_Return(return_value: option(expr))
  { "return statement with optional return expression {return_value}" }
  | S_Cond(condition: expr, then_statement: stmt, else_statement: stmt)
  { "condition statement with condition expression {condition},
    true branch statement {then_statement}, and
    false branch statement {else_statement}" }
  | S_Assert(condition: expr)
  { "assertion statement with {condition}" }
  | S_For [
    index_name: Identifier,
    start_e   : expr,
    dir       : for_direction,
    end_e     : expr,
    body      : stmt,
    limit     : option(expr)
  ]
  { "for loop statement with
    index variable {index_name},
    start expression {start_e},
    direction {dir},
    end expression {end_e},
    body {body},
    and optional loop limit {limit}" }
  | S_While(condition: expr, loop_limit: option(expr), body: stmt)
  { "while statement with condition {condition},
    optional loop limit {loop_limit},
    and body {body}" }
  | S_Repeat(body: stmt, condition: expr, loop_limit: option(expr))
  { "repeat statement with body {body},
    condition {condition},
    and optional loop limit {loop_limit}" }
  | S_Throw(exception: expr)
  { "throw statement with exception expression {exception}" }
  | S_Try(statement: stmt, catchers: list0(catcher), otherwise: option(stmt))
  { "try statement with statement {statement},
    list of catchers {catchers},
    and otherwise optional statement {otherwise}" }
  | S_Print(arguments: list0(expr), newline: Bool)
  { "print statement with list of arguments {arguments} and newline choice {newline}" }
  | S_Pragma(pragma_name: Identifier, arguments: list0(expr))
  { "pragma statement for the pragma name {pragma_name} and list of arguments {arguments}" }
  | S_Unreachable
  { "unreachable statement" }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////
  | typed_S_Throw(exception: expr, exception_type: ty)
    {
        "throw statement with exception expression {exception} and inferred type {exception_type}",
        math_macro = \typedSThrow,
    }
;

render untyped_stmt = stmt(
    S_Pass,
    S_Seq,
    S_Decl,
    S_Assign,
    S_Call,
    S_Return,
    S_Cond,
    S_Assert,
    S_For,
    S_While,
    S_Repeat,
    S_Throw,
    S_Try,
    S_Print,
    S_Pragma,
    S_Unreachable,
);
render typed_stmt { lhs_hypertargets = false } = stmt(typed_S_Throw);

render stmt_pass = stmt(S_Pass);
render stmt_seq = stmt(S_Seq);
render stmt_decl = stmt(S_Decl);
render stmt_assign = stmt(S_Assign);
render stmt_call = stmt(S_Call);
render stmt_return = stmt(S_Return);
render stmt_cond = stmt(S_Cond);
render stmt_assert = stmt(S_Assert);
render stmt_for = stmt(S_For), for_direction(-);
render stmt_while = stmt(S_While);
render stmt_repeat = stmt(S_Repeat);
render stmt_throw = stmt(S_Throw);
render stmt_try = stmt(S_Try);
render stmt_print = stmt(S_Print);
render stmt_pragma = stmt(S_Pragma);
render stmt_unreachable = stmt(S_Unreachable);

constant label_S_Decl : ASTLabels { math_macro = \SDecl };
constant label_S_Assign : ASTLabels { math_macro = \SAssign };
constant laebl_S_Assert : ASTLabels { math_macro = \SAssert };
constant label_S_Print : ASTLabels { math_macro = \SPrint };

ast case_alt { "case alternative" } =
    [ case_alt_pattern: pattern { math_macro = \casealtpattern },
      where: option(expr),
      case_alt_stmt: stmt { math_macro = \casealtstmt }
    ]
    { "case alternative for the pattern {pattern},
        optional where expression {where},
        and statement {stmt}"
    }
;

ast catcher { "catcher" } =
 (variable: option(Identifier), guard_type: ty, execute: stmt)
 { "catcher for an exception of type {guard_type}
    with the optional variable name {variable}
    executing {execute}" }
;

ast subprogram_type { "subprogram type" } =
    | ST_Procedure
    { "procedure" }
    | ST_Function
    { "function" }
    | ST_Getter
    { "getter" }
    | ST_Setter
    { "setter" }
;

ast func_qualifier { "subprogram qualifier" } =
    | Pure
    { "pure" }
    | Readonly
    { "readonly" }
    | Noreturn
    { "noreturn" }
;

ast override_info { "override qualifier" } =
    | Impdef
    { "impdef qualifier" }
    | Implementation
    { "implementation qualifier" }
;

ast func { "subprogram descriptor" } =
    [
    name: Strings,
    parameters: list0((name: Identifier, type: option(ty))),
    args: list0(typed_identifier),
    func_body: stmt { math_macro = \funcbody },
    return_type: option(ty),
    func_subprogram_type: subprogram_type { math_macro = \funcsubprogramtype},
    recurse_limit: option(expr),
    builtin: Bool,
    qualifier: option(func_qualifier),
    override: option(override_info),
    ]
    { "a subprogram descriptor for the subprogram name {name},
        parameter list {parameters},
        arguments {args},
        body {body},
        optional return type {return_type},
        subprogram type {subprogram_type},
        optional recursion limit {recurse_limit},
        builtin flag {builtin},
        subprogram qualifier {qualifier},
        and override qualifier {override}"
    }
;

ast global_decl_keyword { "global declaration keyword" } =
 | GDK_Constant
 { "constant" }
 | GDK_Config
 { "configuration" }
 | GDK_Let
 { "immutable global storage" }
 | GDK_Var
 { "mutable global storage" }
;

ast global_decl { "global storage declaration" } =
    [
    keyword: global_decl_keyword,
    global_decl_name: Identifier { math_macro = \globaldeclname },
    global_decl_ty: option(ty) { math_macro = \globaldeclty },
    initial_value: option(expr)
    ]
    { "global storage declaration with the
        keyword {keyword},
        element name {name},
        optional type annotation {ty},
        and optional initializer {initial_value}" }
;

ast decl { "global declaration" } =
    | D_Func(descriptor: func)
    { "subprogram declaration with descriptor {descriptor}" }
    | D_GlobalStorage(storage_declaration: global_decl)
    { "global storage declaration with {storage_declaration}" }
    | D_TypeDecl(type_name: Identifier, annotation: ty, extra_fields: option((super_type: Identifier, with_fields: list0(field))))
    { "type declaration for the type name {type_name},
        type annotation {annotation},
        optional extra fields {extra_fields} in addition to those in {super_type}" }
    | D_Pragma(pragma_name: Identifier, arguments: list0(expr))
    { "pragma declaration for the pragma name {pragma_name} and
        arguments {arguments}" }
;

constant label_D_Func : ASTLabels { math_macro = \DFunc };

render decl_global_storage = decl(D_GlobalStorage), global_decl(-), global_decl_keyword(-);
render decl_func = decl(D_Func), func(-) ,subprogram_type(-), func_qualifier(-), override_info(-), typed_identifier(-);
render decl_type = decl(D_TypeDecl), field(-);
render decl_global_pragma = decl(D_Pragma);

ast spec { "specification" } =
 list0((declarations: decl))
  { "list of declarations {declarations}" }
;

////////////////////////////////////////////////////////////////////////////////
// Type System Types
////////////////////////////////////////////////////////////////////////////////

typedef static_envs
    {
        "static environment",
        math_macro = \staticenvs,
    } =
 [
  static_envs_G: global_static_envs
  { math_macro = \staticenvsG },
  static_envs_L: local_static_envs
  { math_macro = \staticenvsL },
 ]
  {
      "static environment with global static environment {G} and local static environment {L}",
  }
;

typedef global_static_envs
    {
        "global static environment",
        math_macro = \globalstaticenvs,
    } =
    [
        declared_types: partial Identifier -> (element_type: ty, element_purity: TPurity),
        constant_values: partial Identifier -> literal,
        global_storage_types: partial Identifier -> (element_type: ty, declared_keyword: global_decl_keyword),
        global_expr_equiv: partial Identifier -> (initializer: expr) { math_macro = \globalstaticenvsexprequiv },
        subtypes: partial (sub_type: Identifier) ->
         (super_type: Identifier),
        subprogram: partial Identifier -> (func, side_effects: powerset(TSideEffect)),
        overloaded_subprograms: partial Identifier -> powerset(Strings)
    ]
;

typedef local_static_envs
    {
        "local static environment",
        math_macro = \localstaticenvs,
    } =
    [
        local_storage_types: partial Identifier -> (element_type: ty, declared_keyword: local_decl_keyword),
        local_expr_equiv: partial Identifier -> expr { math_macro = \localstaticenvsexprequiv },
        local_return_type: option(ty) { math_macro = \localstaticenvsreturntype }
    ]
;

render static_envs_and_components = static_envs(-), global_static_envs(-), local_static_envs(-);

constant empty_tenv : static_envs {
    "empty static environment",
    math_macro = \emptytenv,
};

typedef type_error
    {
        "\typingerrorterm{}",
        short_circuit_macro = \TypeErrorConfig,
    } =
    TypeError(error_code: type_error_code)
    {
        "\typingerrorterm{} with error code {error_code}",
    }
;

operator TypeErrorConfig() -> TypeError(type_error_code)
{
  "a \typingerrorterm{} configuration",
  math_macro = \TypeErrorConfig,
};

typedef type_error_code { "type error code" } =
  | TE_UI   { "Undefined identifier Typing" }
  | TE_IAD  { "Identifier already declared" }
  | TE_AIM  { "Assign to immutable" }
  | TE_TSF  { "Type satisfaction failure" }
  | TE_LCA  { "Lowest common ancestor" }
  | TE_NBV  { "No base value" }
  | TE_TAF  { "Type assertion failure" }
  | TE_SEF  { "Static evaluation failure" }
  | TE_BO   { "Bad operands" }
  | TE_UT   { "Unexpected type" }
  | TE_BTI  { "Bad tuple index" }
  | TE_BS   { "Bad slices" }
  | TE_BF   { "Bad field" }
  | TE_BSPD { "Bad subprogram declaration" }
  | TE_BD   { "Bad declaration" }
  | TE_BC   { "Bad call" }
  | TE_SEV  { "Side effect violation" }
  | TE_OE   { "Overriding error" }
  | TE_PLD  { "Declaration with an imprecise type" }
;

render type_error_and_codes = type_error(-), type_error_code(-);

////////////////////////////////////////////////////////////////////////////////
// Side Effects Types

typedef TPurity { "\purity" } =
    | SE_Pure { "purity descriptor for the evaluation of a \pure{} construct" }
    | SE_Readonly { "purity descriptor for the evaluation of a \readonly{} construct" }
    | SE_Impure { "purity descriptor for the evaluation of a construct that is neither \pure{} nor \readonly{}" }
;

operator ge_pure(a: TPurity, b: TPurity) -> Bool
{
  "{a} is greater or equal to {b} in the purity order",
  math_macro = \puritygeq
};

typedef TSideEffect { "\sideeffectdescriptorterm{}" } =
    | LocalEffect(purity: TPurity)
    { "local \sideeffectdescriptorterm{} with \purity{} {purity}" }
    | GlobalEffect(purity: TPurity)
    { "global \sideeffectdescriptorterm{} with \purity{} {purity}" }
    | Immutability(immutable: Bool)
    { "\sideeffectdescriptorterm{} for a construct that accesses storage elements whose immutability is given by {immutable}" }
;

////////////////////////////////////////////////////////////////////////////////
// Dynamic Semantics Types
////////////////////////////////////////////////////////////////////////////////

function dynamic_domain(env: envs, t: ty) -> (d: powerset(native_value))
{
  "assigns a set of \nativevaluesterm{} {d} to the annotated type {t} in the environment {env}.",
  prose_application = "",
};

typedef native_value
    {
        "\nativevalue{}",
        math_macro = \nativevalue,
    } =
    | NV_Literal(l: literal)
    { "\nativevalue{} for the literal {l}" }
    | NV_Vector(values: list0(native_value))
    { "\nativevalue{} for the vector {values}" }
    | NV_Record(field_to_value: partial Identifier -> native_value)
    { "\nativevalue{} record {field_to_value}" }
;

typedef tint
   {
       "native integer type",
       math_macro = \tint,
   } =
   (NV_Literal(L_Int(v: Z)))
   {
       "native integer for {v}",
   }
;

typedef tbool
   {
       "native Boolean type",
       math_macro = \tbool,
   } =
   (NV_Literal(L_Bool(v: Bool)))
   {
       "native Boolean for {v}",
   }
;

typedef treal
   {
       "native real type",
       math_macro = \treal,
   } =
   (NV_Literal(L_Real(v: Q)))
   {
       "native real for {v}",
   }
;

typedef tlabel
   {
       "native label type",
       math_macro = \tlabel,
   } =
   (NV_Literal(L_Label(v: Identifier)))
   {
       "native label for {v}",
   }
;

typedef tstring
   {
       "native string type",
       math_macro = \tstring,
   } =
   (NV_Literal(L_String(v: Strings)))
   {
       "native string for {v}",
   }
;

typedef tbitvector
   {
       "native bitvector type",
       math_macro = \tbitvector,
   } =
   (NV_Literal(L_Bitvector(v: list0(Bit))))
   {
       "native bitvector for {v}",
   }
;

typedef tvector
   {
       "native vector type",
       math_macro = \tvector,
   } =
   (NV_Vector(values: list0(native_value)))
   {
       "native vector for {values}",
   }
;

typedef trecord
   {
       "native record type",
       math_macro = \trecord,
   } =
   (NV_Record(field_to_value: partial Identifier -> native_value))
   {
       "native record for {field_to_value}",
   }
;

render native_types = tint(-), tbool(-), treal(-), tlabel(-), tstring(-), tbitvector(-), tvector(-), trecord(-);

typedef dynamic_envs
    {
        "dynamic environment",
        math_macro = \dynamicenvs,
    } =
    [
      dynamic_envs_G: global_dynamic_envs { math_macro = \dynamicenvsG },
      dynamic_envs_L: local_dynamic_envs { math_macro = \dynamicenvsL },
    ]
    {
        "dynamic environment with global dynamic environment {dynamic_envs_G} and local dynamic environment {dynamic_envs_L}",
    }
;

typedef global_dynamic_envs
    {
        "global dynamic environment",
        math_macro = \globaldynamicenvs,
    } =
    [
        storage: partial Identifier -> native_value,
        pending_calls: partial Identifier -> N
    ]
    {
        "global dynamic environment with storage mapping {storage} and pending calls mapping {pending_calls}",
    }
;

typedef local_dynamic_envs
    {
        "local dynamic environment",
        math_macro = \localdynamicenvs,
    } =
    (partial Identifier -> native_value)
    {
        "local dynamic environment as a partial mapping from identifiers to native values",
    }
;

render dynamic_envs_and_components = dynamic_envs(-), global_dynamic_envs(-), local_dynamic_envs(-);

constant empty_denv : dynamic_envs
    {
        "empty dynamic environment",
        math_macro = \emptydenv,
    }
;

typedef envs
    {
        "environment",
        math_macro = \envs,
    } =
    (static: static_envs, dynamic: dynamic_envs)
    {
        "environment with static environment {static} and dynamic environment {dynamic}",
    }
;

////////////////////////////////////////////////////////////////////////////////
// Concurrent Execution Graphs

typedef effect_type =
  | Read
    { "read effect", math_macro = \Read }
  | Write
  { "write effect", math_macro = \Write }
;

typedef Labels
{
  "execution graph labels",
  math_macro = \Labels,
} =
  | asldata { "data dependency", math_macro = \asldata }
  | aslctrl { "control dependency", math_macro = \aslctrl }
  | aslpo   { "program order dependency", math_macro = \aslpo }
;

typedef Nodes
{
    "execution graph nodes",
    math_macro = \Nodes,
} =
    (node_id: N, effect_type, storage_element: Identifier)
    {
        "execution graph node with identifier {node_id}, operation type {operation_type}, and storage element {storage_element}",
    }
;

typedef XGraphs
{
    "\executiongraphterm{}",
    math_macro = \XGraphs,
} =
    (vertices: powerset(Nodes), edges: powerset((source: Nodes, label: Labels, target: Nodes)), output_nodes: powerset(Nodes))
    {
        "\executiongraphterm{} with vertices {vertices}, labeled edges {edges}, and output nodes {output_nodes}",
    }
;

render xgraphs_and_components = XGraphs(-), Nodes(-), Labels(-), effect_type(-);

constant empty_graph : XGraphs { "empty execution graph", math_macro = \emptygraph };

constant return_var_prefix : Strings
{
  math_macro = \returnvarprefix,
};

constant dot_str : Strings
{
  math_macro = \dotstr,
};

ast symdom { "\symbolicdomain{}" } =
    | Finite(powerset_finite(Z))
    { "symbolic finite set integer domain" }
    | ConstrainedDom(int_constraint)
    {
      "symbolic constrained integer domain",
      math_macro = \ConstrainedDom
    }
;

ast symdom_or_top { "symbolic integer set" } =
    | Top
    { "symbolic unconstrained integer domain" }
    | Subdomains(list1(symdom))
    { "symbolic subdomains" }
;

render symbolic_domains = symdom(-), symdom_or_top(-);

typedef approximation_direction { "approximation direction" } =
  | Over { "overapproximation" }
  | Under { "underapproximation" }
;

typedef ApproximationFailure
{
  "approximation failure indicator",
  short_circuit_macro = \TypeErrorConfig
} =
  | CannotOverapproximate {
      "cannot overapproximate",
      math_macro = \CannotOverapproximate
    }
  | CannotUnderapproximate {
      "cannot underapproximate",
      math_macro = \CannotUnderapproximate
    }
;

typedef abstract_configuration
{
    "\hyperlink{type-abstractconfiguration}{abstract configuration}",
    math_macro = \AbsConfig
} =
    | Abs_Continuing
    { "abstract continuing configuration" }
    | Abs_Returning
    { "abstract returning configuration" }
    | Abs_Abnormal
    { "abstract abnormal configuration" }
;

////////////////////////////////////////////////////////////////////////////////
// Dynamic Semantics Configurations

typedef TNormal
{
    "normal execution result",
} =
    | ResultExpr(value_and_graph: (native_value, XGraphs), environment: envs)
    { "expression result with value-graph pair {value_and_graph} and {environment}" }
    | ResultExprSEF(value: native_value, graph: XGraphs)
    { "side-effect-free expression result with {value} and {graph}" }
    | ResultLexpr(graph: XGraphs, environment: envs)
    { "assignable expression result with {graph} and {environment}" }
    | ResultLDI(graph: XGraphs, environment: envs)
    { "local declaration item result with {graph} and {environment}" }
    | ResultSlices(slices_and_graph: (list0((native_value, native_value)), XGraphs), environment: envs)
    { "slices result with slice list and graph {slices_and_graph} and {environment}" }
    | ResultExprList(values_and_graph: (list0(native_value), XGraphs), environment: envs)
    { "expression list result with values and graph {values_and_graph} and {environment}" }
    | ResultExprListM(value_graph_pairs: list0((native_value, XGraphs)), environment: envs)
    { "expression list result with value-graph pairs {value_graph_pairs} and {environment}" }
    | ResultPattern(boolean_value: tbool, graph: XGraphs)
    { "pattern result with {boolean_value} and {graph}" }
    | ResultCall((values: list0(value_read_from), gdenv: global_dynamic_envs), g: XGraphs)
    { "call result with value-read effects {values}, a global dynamic environment {gdenv}, and a an \executiongraphterm{} {g}" }
;

// Casts a native value into a Boolean native value.
operator native_value_as_tbool(v: native_value) -> tbool
{
  "{v}",
  math_macro = \identityop,
  typecast = true,
};

typedef TThrowing
{
    "throwing execution result",
    short_circuit_macro = \ThrowingConfig,
} =
    Throwing(exception_value: value_read_from, exception_type: ty, graph: XGraphs, environment: envs)
    { "throwing result with exception value {exception_value}, type {exception_type}, {graph}, and {environment}" }
;

typedef TContinuing
{
    "continuing execution result",
} =
    Continuing(graph: XGraphs, environment: envs)
    { "continuing result with {graph} and {environment}" }
;

typedef TReturning
{
    "returning execution result",
    short_circuit_macro = \ReturningConfig,
} =
    Returning(values_and_graph: (list0(native_value), XGraphs), environment: envs)
    { "returning result with values and graph {values_and_graph} and {environment}" }
;

typedef TDynError
{
    "dynamic error result",
    short_circuit_macro = \DynErrorConfig,
} =
    DynamicError(error_code: dynamic_error_code)
    { "dynamic error with error code {error_code}",
        math_macro = \DynamicError,
    }
;

typedef dynamic_error_code { "dynamic error code" } =
  | DE_UNR  { "Dynamic unreachable error" }
  | DE_DAF  { "Dynamic assertion failure" }
  | DE_TAF  { "Dynamic type assertion failure" }
  | DE_AET  { "ARBITRARY empty type" }
  | DE_BO   { "Bad operands" }
  | DE_LE   { "Limit exceeded" }
  | DE_UE   { "Uncaught exception" }
  | DE_BI   { "Bad index" }
  | DE_OSA  { "Overlapping slice assignment" }
  | DE_NAL  { "Negative array length" }
  | DE_NEP  { "No entry point" }
;

render dynamic_error_and_codes = TDynError(-), dynamic_error_code(-);

typedef TDiverging
{
    "diverging execution result",
    short_circuit_macro = \DivergingConfig,
} =
    Diverging
    { "diverging execution result" }
;

typedef TOutConfig { "catcher output configuration" } =
  | (TNormal)
  | (TThrowing)
  | (TContinuing)
  | (TReturning)
;

typedef TContinuingOrReturning { "continuing or returning configuration" } =
  | (TContinuing)
  | (TReturning)
;

typedef value_read_from { "value-reading effect" } =
    (v: native_value, id: Identifier)
    { "value-reading effect for {v} and {id}" }
;

////////////////////////////////////////////////////////////////////////////////
// Generic Functions and Relations
////////////////////////////////////////////////////////////////////////////////

typing function te_check(condition: Bool, code: type_error_code) -> CheckResult | type_error
  {
    "returns $\True$ if {condition} holds and a type error with {code} otherwise.",
    prose_application = "checking whether {condition} holds returns $\True\terminateas\TypeError({code})$",
  } =
  case te_check_true {
    condition = True;
    --
    True;
  }
  case te_check_false {
    condition = False;
    --
    TypeError(code);
  }
;

semantics function de_check(condition: Bool, code: dynamic_error_code) -> CheckResult | TDynError
  {
    "returns $\True$ if {condition} holds and a dynamic error with {code} otherwise.",
    prose_application = "checking whether {condition} holds returns $\True\terminateas\DynamicError({code})$",
  } =
  case de_check_true {
    condition = True;
    --
    True;
  }
  case de_check_false {
    condition = False;
    --
    DynamicError(code);
  }
;

function bool_transition(condition: Bool) -> (result: Bool)
{
    math_macro = \booltrans,
    "is the identity function for Booleans.",
    prose_application = "testing whether {condition} holds returns {result}",
} =
  --
  condition;
;

function rexpr(le: lexpr) -> (re: expr)
{
  "transforms the \assignableexpression{} {le} to the \rhsexpression{} {re}.",
  prose_application = "transforming the \assignableexpression{} {le} to a \rhsexpression{} yields {re}",
  math_macro = \torexpr,
};

////////////////////////////////////////////////////////////////////////////////
// Relations and functions for Literals
////////////////////////////////////////////////////////////////////////////////

typing function annotate_literal(tenv: static_envs, l: literal) -> (t: ty)
{
    "annotates a literal {l} in the \staticenvironmentterm{} {tenv}, resulting in a type {t}.",
    prose_application = "annotating {l} in {tenv} yields {t}",
} =
  case Int {
    l =: L_Int(n);
    cs := make_singleton_list(Constraint_Exact(E_Literal(L_Int(n))));
    --
    T_Int(WellConstrained(cs));
  }

  case Bool {
    l = L_Bool(_);
    --
    T_Bool;
  }

  case Real {
    l = L_Real(_);
    --
    T_Real;
  }

  case String {
    l = L_String(_);
    --
    T_String;
  }

  case Bits {
    l =: L_Bitvector(bits);
    n := list_len(bits);
    --
    T_Bits(E_Literal(L_Int(n)), empty_list);
  }

  case Label {
    l =: L_Label(label);
    tenv.static_envs_G.declared_types(label) =: (t, _);
    --
    t;
  }
;

////////////////////////////////////////////////////////////////////////////////
// Relations and functions for Expressions
////////////////////////////////////////////////////////////////////////////////

typing relation annotate_expr(tenv: static_envs, e: expr) -> (t: ty, new_e: expr, ses: powerset(TSideEffect)) | type_error
{
    "annotates the expression {e} in the \staticenvironmentterm{} {tenv},
   resulting in the following:
   {t} is the type inferred for {e};
   {new_e} is the \typedast{} for {e}, also known as the \emph{annotated expression};
   and {ses} is the \sideeffectsetterm{} inferred for {e}. \ProseOtherwiseTypeError",
    prose_application = "annotating {e} in {tenv} yields
    {t}, the annotated expression {new_e} and {ses}\ProseOrTypeError",
} =
  case ELit {
    e =: E_Literal(v);
    annotate_literal(tenv, v) -> t;
    --
    (t, e, empty_set);
  }

  case EVar {
    e =: E_Var(x);
    case local {
      tenv.static_envs_L.local_storage_types(x) =: (t, k);
      ses_ldk(k) -> ses;
      --
      (t, E_Var(x), ses);
    }

    case global {
      tenv.static_envs_L.local_storage_types(x) = bot;
      tenv.static_envs_G.global_storage_types(x) =: (ty, k);
      case const {
        k = GDK_Constant;
        tenv.static_envs_G.constant_values(x) =: v;
        --
        (ty, E_Literal(v), empty_set);
      }

      case non_const {
        k != GDK_Constant || tenv.static_envs_G.constant_values(x) = bot;
        ses_gdk(k) -> ses;
        --
        (ty, E_Var(x), ses);
      }
    }

    case error_undefined {
      tenv.static_envs_L.local_storage_types(x) = bot;
      tenv.static_envs_G.global_storage_types(x) = bot;
      --
      TypeError(TE_UI);
    }
  }

  case EBinop {
    e =: E_Binop(op, e1, e2);
    annotate_expr(tenv, e1) -> (t1, e1', ses1);
    annotate_expr(tenv, e2) -> (t2, e2', ses2);
    apply_binop_types(tenv, op, t1, t2) -> t;
    ses := union(ses1, ses2);
    --
    (t, E_Binop(op, e1', e2'), ses);
  }

  case EUnop {
    e =: E_Unop(op, e');
    annotate_expr(tenv, e') -> (t'', e'', ses);
    apply_unop_type(tenv, op, t'') -> t;
    --
    (t, E_Unop(op, e''), ses);
  }

  case ECond {
    e =: E_Cond(e_cond, e_true, e_false);
    annotate_expr(tenv, e_cond) -> (t_cond, e_cond', ses_cond);
    check_structure_label(tenv, t_cond, label_T_Bool) -> True;
    annotate_expr(tenv, e_true) -> (t_true, e_true', ses_true);
    annotate_expr(tenv, e_false) -> (t_false, e_false', ses_false);
    lowest_common_ancestor(tenv, t_true, t_false) -> t;
    ses := union(ses_cond, ses_true, ses_false);
    --
    (t, E_Cond(e_cond', e_true', e_false'), ses);
  }

  case ECall {
    e =: E_Call(call);
    annotate_call(tenv, call) -> (call', some(t), ses);
    --
    (t, E_Call(call'), ses);
  }

  case ESlice {
    e =: E_Slice(e', slices);
    annotate_expr(tenv, e') -> (t_e', e'', ses1);
    get_structure(tenv, t_e') -> struct_t_e';
    case okay {
      ast_label(struct_t_e') in make_set(label_T_Bits, label_T_Int);
      te_check(slices != empty_list, TE_BS) -> True;
      annotate_slices(tenv, slices) -> (slices', ses2);
      slices_width(tenv, slices) -> w;
      ses := union(ses1, ses2);
      --
      (T_Bits(w, empty_list), E_Slice(e'', slices'), ses);
    }
    case error {
      ast_label(struct_t_e') not_in make_set(label_T_Bits, label_T_Int);
      --
      TypeError(TE_BS);
    }
  }

  case EGetArray {
    e =: E_GetArray(e_base, e_index);
    annotate_expr(tenv, e_base) -> (t_base, e_base', ses_base);
    make_anonymous(tenv, t_base) -> t_anon_base;
    te_check(ast_label(t_anon_base) = label_T_Array, TE_UT) -> True;
    t_anon_base =: T_Array(size, t_elem);
    annotate_get_array(tenv, (size, t_elem), (e_base', ses_base, e_index)) -> (t, new_e, ses)
    { math_layout = [_] };
    --
    (t, new_e, ses);
  }

  case EGetField {
    e =: E_GetField(e1, field_name);
    annotate_expr(tenv, e1) -> (t_e1, e2, ses1);
    make_anonymous(tenv, t_e1) -> t_e2;
    case structured {
      t_e2 =: make_structured(L, fields);
      case record_or_exception {
        L in make_set(label_T_Exception, label_T_Record);
        case okay {
          assoc_opt(fields, field_name) =: some(t);
          --
          (t, E_GetField(e2, field_name), ses1);
        }
        case error {
          assoc_opt(fields, field_name) = None;
          --
          TypeError(TE_BF);
        }
      }
      case collection {
        L = label_T_Collection;
        e2 =: E_Var(collection_var_name);
        case okay {
          assoc_opt(fields, field_name) =: some(t);
          --
          (t, E_GetCollectionFields(collection_var_name, make_singleton_list(field_name)), ses1)
          { math_layout = [_] };
        }
        case error {
          assoc_opt(fields, field_name) = None;
          --
          TypeError(TE_BF);
        }
      }
    }

    case bitfield {
      T_Bits(_, bitfields) := t_e2;
      case simple {
        find_bitfield_opt(field_name, bitfields) -> some(BitField_Simple(_, slices));
        e3 := E_Slice(e2, slices);
        annotate_expr(tenv, e3) -> (t, new_e, ses);
        --
        (t, new_e, ses);
      }
      case nested {
        find_bitfield_opt(field_name, bitfields) -> some(BitField_Nested(_, slices, bitfields'))
        { math_layout = [_] };
        e3 := E_Slice(e2, slices);
        annotate_expr(tenv, e3) -> (t_e4, new_e, ses_new);
        t_e4 =: T_Bits(width, _);
        t := T_Bits(width, bitfields');
        --
        (t, new_e, ses_new);
      }
      case typed {
        find_bitfield_opt(field_name, bitfields) -> some(BitField_Type(_, slices, t))
        { math_layout = [_] };
        e3 := E_Slice(e2, slices);
        annotate_expr(tenv, e3) -> (t_e4, new_e, ses_new);
        check_type_satisfies(tenv, t_e4, t) -> True;
        --
        (t, new_e, ses_new);
      }
      case error {
        find_bitfield_opt(field_name, bitfields) -> None;
        --
        TypeError(TE_BF);
      }
    }

    case tuple_item {
      t_e2 =: T_Tuple(tys);
      field_name =: numbered_identifier(item, index);
      te_check(zero <= index && index < list_len(tys), TE_BTI) -> True;
      t := tys[index];
      new_e := E_GetItem(e2, index);
      --
      (t, new_e, ses1);
    }

    case error {
      ast_label(t_e2) not_in make_set(label_T_Bits, label_T_Collection, label_T_Exception, label_T_Record, label_T_Tuple)
      { math_layout = (_, [_]) };
      --
      TypeError(TE_UT);
    }
  }

  case EGetFields {
    e =: E_GetFields(e_base, fields);
    annotate_expr(tenv, e_base) -> (t_base_annot, e_base_annot, ses_base);

    case bits {
      make_anonymous(tenv, t_base_annot) -> T_Bits(_, bitfields);
      INDEX(i, fields: find_bitfields_slices(fields[i], bitfields) -> slices[i]);
      e_slice := E_Slice(e_base, list_flatten(slices));
      annotate_expr(tenv, e_slice) -> (t, new_e, ses);
      --
      (t, new_e, ses);
    }
    case record {
      make_anonymous(tenv, t_base_annot) -> T_Record(base_fields);
      INDEX(i, fields: get_bitfield_width(tenv, fields[i], base_fields) -> e_width[i]);
      width_plus(tenv, e_width) -> e_slice_width;
      --
      (T_Bits(e_slice_width, empty_list), E_GetFields(e_base_annot, fields), ses_base)
      { math_layout = (_, [_]) };
    }
    case collection {
      make_anonymous(tenv, t_base_annot) -> T_Collection(base_fields);
      e_base_annot =: E_Var(base_collection_name);
      INDEX(i, fields: get_bitfield_width(tenv, fields[i], base_fields) -> e_width[i]);
      width_plus(tenv, e_width) -> e_slice_width;
      --
      (T_Bits(e_slice_width, empty_list), E_GetCollectionFields(base_collection_name, fields), ses_base)
      { math_layout = [_, [_]] };
    }
    case error {
      make_anonymous(tenv, t_base_annot) -> t_base_annot_anon;
      ast_label(t_base_annot_anon) not_in make_set(label_T_Bits, label_T_Collection, label_T_Record);
      --
      TypeError(TE_UT);
    }
  }

  case EATC {
    e =: E_ATC(e', ty);
    annotate_expr(tenv, e') -> (t, e'', ses_e);
    get_structure(tenv, t) -> t_struct;
    annotate_type(False, tenv, ty) -> (ty', ses_ty);
    get_structure(tenv, ty') -> ty_struct;
    check_atc(tenv, t_struct, ty_struct) -> True;
    ses' := union(ses_ty, ses_e);
    subtype_satisfies(tenv, t_struct, ty_struct) -> always_succeeds;
    (new_e, ses) := if always_succeeds then (e'', ses_e) else (E_ATC(e'', ty'), ses');
    --
    (ty', new_e, ses);
  }

  case EPattern {
    e =: E_Pattern(e1, pat);
    annotate_expr(tenv, e1) -> (t_e2, e2, ses_e);
    annotate_pattern(tenv, t_e2, pat) -> (pat', ses_pat);
    ses := union(ses_e, ses_pat);
    --
    (T_Bool, E_Pattern(e2, pat'), ses);
  }

  case EArbitrary {
    e =: E_Arbitrary(ty);
    annotate_type(False, tenv, ty) -> (ty1, ses_ty);
    get_structure(tenv, ty1) -> ty2;
    ses := union(ses_ty,
          make_set(
            GlobalEffect(SE_Readonly),
            Immutability(False),
            LocalEffect(SE_Readonly)))
    { math_layout = (lhs, (_, [_])) };
    --
    (ty1, E_Arbitrary(ty2), ses);
  }

  case ERecord {
    e =: E_Record(ty, fields);
    te_check(is_named(ty), TE_UT) -> True;
    make_anonymous(tenv, ty) -> ty_anon;
    te_check(is_structured(ty_anon), TE_UT) -> True;
    ty_anon =: make_structured(L, field_types);
    initialized_fields := list_fst(fields);
    names := list_fst(field_types);
    te_check(make_set(names) = make_set(initialized_fields), TE_BF) -> True;
    check_no_duplicates(initialized_fields) -> True;
    INDEX(i, fields: annotate_field_init(tenv, fields[i], field_types) ->
                     (field_names[i], field_inits[i], field_effects[i]))
    { math_layout = (_, [_]) };
    fields' := list_combine(field_names, field_inits);
    ses := union_list(field_effects);
    --
    (ty, E_Record(ty, fields'), ses);
  }

  case ETuple {
    e =: E_Tuple(li);
    case parenthesized {
      li =: make_singleton_list(e');
      annotate_expr(tenv, e') -> (t, new_e, ses);
      --
      (t, new_e, ses);
    }

    case list {
      list_len(li) > one;
      INDEX(i, li: annotate_expr(tenv, li[i]) -> (t[i], es[i], xs[i]));
      ses := union_list(xs);
      --
      (T_Tuple(t), E_Tuple(match_non_empty_list(es)), ses);
    }
  }
;

render rule annotate_expr_ELit = annotate_expr(ELit);
render rule annotate_expr_EVar = annotate_expr(EVar);
render rule annotate_expr_EBinop = annotate_expr(EBinop);
render rule annotate_expr_EUnop = annotate_expr(EUnop);
render rule annotate_expr_ECond = annotate_expr(ECond);
render rule annotate_expr_ECall = annotate_expr(ECall);
render rule annotate_expr_ESlice = annotate_expr(ESlice);
render rule annotate_expr_EGetArray = annotate_expr(EGetArray);
render rule annotate_expr_EGetField_record_or_exception = annotate_expr(EGetField.structured.record_or_exception);
render rule annotate_expr_EGetField_collection = annotate_expr(EGetField.structured.collection);
render rule annotate_expr_EGetField_bitfield = annotate_expr(EGetField.bitfield);
render rule annotate_expr_EGetField_tuple_item = annotate_expr(EGetField.tuple_item);
render rule annotate_expr_EGetField_error = annotate_expr(EGetField.error);
render rule annotate_expr_EGetFields = annotate_expr(EGetFields);
render rule annotate_expr_EATC = annotate_expr(EATC);
render rule annotate_expr_EPattern = annotate_expr(EPattern);
render rule annotate_expr_EArbitrary = annotate_expr(EArbitrary);
render rule annotate_expr_ERecord = annotate_expr(ERecord);
render rule annotate_expr_ETuple = annotate_expr(ETuple);

typing function find_bitfields_slices(name: Identifier, bitfields: list0(bitfield)) -> (slices: list0(slice)) | type_error
{
  "returns the slices associated with the bitfield named {name} among the list of bitfields {bitfields}
  in {slices}. \ProseOtherwiseTypeError",
  prose_application = "finding the slices associated with the bitfield named {name} among the list of bitfields {bitfields}
  yields {slices}\ProseOtherwiseTypeError",
} =
  case non_empty {
    bitfields =: match_cons(field, bitfields1);
    bitfield_get_name(field) -> name';
    case found {
      name' = name;
      bitfield_get_slices(field) -> slices;
      --
      slices;
    }

    case tail {
      name' != name;
      find_bitfields_slices(name, bitfields1) -> slices;
      --
      slices;
    }
  }

  case empty {
    bitfields = empty_list;
    --
    TypeError(TE_BF);
  }
;

typing relation annotate_field_init(
  tenv: static_envs,
  (name: Identifier, e': expr),
  field_types: list0(field)) ->
        (name: Identifier, e'': expr, ses: powerset(TSideEffect)) | type_error
{
  "annotates a field initializer $({name}, {e'})$ in a record expression
  with list of fields \\ {field_types} and returns the annotated initializing expression {e''}
  and its \sideeffectdescriptorterm\ {ses}. \ProseOtherwiseTypeError",
  prose_application = "annotating the field initializer $({name}, {e'})$ with respect to
  the list of fields {field_types} yields {e''} and {ses}\ProseOrTypeError",
  math_layout = [_,_],
} =
  annotate_expr(tenv, e') -> (t', e'', ses);
  te_check(field_type(field_types, name) != None, TE_BF) -> True;
  field_type(field_types, name) =: some(t_spec');
  check_type_satisfies(tenv, t', t_spec') -> True;
  --
  (name, e'', ses);
;

typing relation annotate_get_array(
        tenv: static_envs,
        (size: array_index, t_elem: ty),
        (e_base: expr, ses_base: powerset(TSideEffect), e_index: expr)) ->
        (t: ty, new_e: expr, ses: powerset(TSideEffect))
{
  "annotates an array access expression with the
  following elements: {size} is the expression
  representing the array size, {t_elem} is the type of
  array elements, {e_base} is the annotated expression
  for the array base, {e_index} is the index expression.
  The function returns the type of the annotated
  expression in {t}, the annotated expression {new_e},
  and the inferred \sideeffectdescriptorterm{} {ses}.",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_expr(tenv, e_index) -> (t_index', e_index', ses_index);
  type_of_array_length(size) -> wanted_t_index;
  check_type_satisfies(tenv, t_index', wanted_t_index) -> True;
  ses := union(ses_index, ses_base);
  new_e :=
    if ast_label(size) = label_ArrayLength_Expr then
      E_GetArray(e_base, e_index')
    else
      E_GetEnumArray(e_base, e_index')
  { math_layout = (lhs, [_]) };
  --
  (t_elem, new_e, ses)
  { math_layout = [_] };
;

typing function get_bitfield_width(tenv: static_envs, name: Identifier, tfields: list0(field)) ->
         (e_width: expr) | type_error
{
  "returns the expression {e_width} that describes the
  width of the bitfield named {name} in the list of
  fields {tfields}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-getbitfieldwidth}{computing} the width of bitfield {name} in fields {tfields} yields expression {e_width}",
} =
  case okay {
    assoc_opt(tfields, name) =: some(t);
    get_bitvector_width(tenv, t) -> e_width;
    --
    e_width;
  }

  case error {
    assoc_opt(tfields, name) = None;
    --
    TypeError(TE_BF);
  }
;

typing function width_plus(tenv: static_envs, exprs: list0(expr)) -> (e_width: expr) | type_error
{
  "generates the expression {e_width}, which represents the summation of all expressions in the list {exprs},
  normalized in the \staticenvironmentterm{} {tenv}. \ProseOtherwiseTypeError",
  prose_application = "generating the expression representing the summation of {exprs} in {tenv} yields {e_width}",
} =
  case empty {
    exprs = empty_list;
    --
    ELint(zero);
  }

  case non_empty {
    exprs =: match_cons(e, exprs1);
    width_plus(tenv, exprs1) -> e_width1;
    normalize(tenv, AbbrevEBinop(ADD, e, e_width1)) -> e_width;
    --
    e_width;
  }
;

typing function check_atc(tenv: static_envs, t1: ty, t2: ty) ->
         (CheckResult) | type_error
{
  "checks whether the types {t1} and {t2}, which are
  assumed to not be named types, are compatible for a
  type assertion in the \staticenvironmentterm{} {tenv},
  yielding $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkatc}{checking} type compatibility between {t1} and {t2} in {tenv} yields True",
} =
  case equal {
    type_equal(tenv, t1, t2) -> True;
    --
    True;
  }

  case different_labels_error {
    type_equal(tenv, t1, t2) -> False;
    ast_label(t1) != ast_label(t2);
    --
    TypeError(TE_TAF);
  }

  case int_bits {
    type_equal(tenv, t1, t2) -> False;
    ast_label(t1) = ast_label(t2);
    ast_label(t1) in make_set(label_T_Bits, label_T_Int);
    --
    True;
  }

  case tuple {
    type_equal(tenv, t1, t2) -> False;
    t1 =: T_Tuple(l1);
    t2 =: T_Tuple(l2);
    te_check(same_length(l1, l2), TE_TAF) -> True;
    INDEX(i, l1: check_atc(tenv, l1[i], l2[i]) -> True);
    --
    True;
  }

  case other_error {
    type_equal(tenv, t1, t2) -> False;
    ast_label(t1) = ast_label(t2);
    ast_label(t1) not_in make_set(label_T_Bits, label_T_Int, label_T_Tuple);
    --
    TypeError(TE_TAF);
  }
;

semantics relation eval_expr(env: envs, e: expr) ->
  | ResultExpr((v: native_value, g: XGraphs), new_env: envs)
  | TThrowing
  | TDynError
  | TDiverging
{
    "evaluates the expression {e} in an environment {env} and terminates normally with
    a \nativevalueterm{} {v}, an \executiongraphterm{} {g}, and a modified environment
    {new_env}. \ProseOtherwiseAbnormal",
    prose_application = "\hyperlink{relation-eval_expr}{evaluating} {e} in {env} yields
    {v}, {g}, and {new_env}",
    math_layout = (_, [_,_,_,_]),
} =
  case ELit {
    e =: E_Literal(l);
    --
    ResultExpr((NV_Literal(l), empty_graph), env);
  }

  case EVar {
    e =: E_Var(name);
    env =: (_, denv);
    case local {
      map_apply_opt(denv.dynamic_envs_L, name) =: some(v);
    }

    case global {
      map_apply_opt(denv.dynamic_envs_G.storage, name) =: some(v);
    }
    read_identifier(name, v) -> g;
    --
    ResultExpr((v, g), env);
  }

  case BinopAnd {
    e =: AbbrevEBinop(BAND, e1, e2);
    e' := AbbrevECond(e1, e2, E_Literal(L_Bool(False)));
    eval_expr(env, e') -> ResultExpr((v, g), new_env);
    --
    ResultExpr((v, g), new_env);
  }

  case BinopOr {
    e =: AbbrevEBinop(BOR, e1, e2);
    e' := AbbrevECond(e1, E_Literal(L_Bool(True)), e2);
    eval_expr(env, e') -> ResultExpr((v, g), new_env);
    --
    ResultExpr((v, g), new_env);
  }

  case BinopImpl {
    e =: AbbrevEBinop(IMPL, e1, e2);
    e' := AbbrevECond(e1, e2, E_Literal(L_Bool(True)));
    eval_expr(env, e') -> ResultExpr((v, g), new_env);
    --
    ResultExpr((v, g), new_env);
  }

  case Binop {
    e =: E_Binop(op, e1, e2);
    op not_in make_set(BAND, BOR, IMPL);
    eval_expr(env, e1) -> ResultExpr((v1, g1), env1);
    eval_expr(env1, e2) -> ResultExpr((v2, g2), new_env);
    eval_binop(op, v1, v2) -> v;
    g := parallel(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case Unop {
    e =: E_Unop(op, e1);
    eval_expr(env, e1) -> ResultExpr((v1, g), new_env);
    eval_unop(op, v1) -> v;
    --
    ResultExpr((v, g), new_env);
  }

  case ECond {
    e =: E_Cond(e_cond, e1, e2);
    eval_expr(env, e_cond) -> ResultExpr((v_cond, g1), env1);
    v_cond =: nvbool(b);
    e_next := if_then_else(b, e1, e2);
    eval_expr(env1, e_next) -> ResultExpr((v, g2), new_env);
    g := ordered_ctrl(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case ECall {
    e =: E_Call(call);
    eval_call(env, call.call_name, call.params, call.call_args) -> (vms, new_env);
    case single {
      vms =: match_singleton_list((v, g));
    }

    case multi {
      list_len(vms) > one;
      vms =: list_combine(values, graphs);
      g := parallel_graphs(graphs);
      v := NV_Vector(values);
    }
    --
    ResultExpr((v, g), new_env);
  }

  case ESlice {
    e =: E_Slice(e_bv, slices);
    eval_expr(env, e_bv) -> ResultExpr((v_bv, g1), env1);
    eval_slices(env1, slices) -> ResultSlices((slice_ranges, g2), new_env);
    read_from_bitvector(v_bv, slice_ranges) -> v;
    g := parallel(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case EGetArray {
    e =: E_GetArray(e_array, e_index);
    eval_expr(env, e_array) -> ResultExpr((v_array, g1), env1);
    eval_expr(env1, e_index) -> ResultExpr((v_index, g2), new_env);
    v_index =: nvint(i_index);
    get_index(i_index, v_array) -> v;
    g := parallel(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case EGetEnumArray {
    e =: E_GetEnumArray(e_array, e_index);
    eval_expr(env, e_array) -> ResultExpr((v_array, g1), env1);
    eval_expr(env1, e_index) -> ResultExpr((v_index, g2), new_env);
    v_index =: NV_Literal(L_Label(label));
    get_field(label, v_array) -> v;
    g := parallel(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case EGetTupleItem {
    e =: E_GetItem(e_tuple, index);
    eval_expr(env, e_tuple) -> ResultExpr((v_tuple, g), new_env);
    get_index(index, v_tuple) -> v;
    --
    ResultExpr((v, g), new_env);
  }

  case ERecord {
    e =: E_Record(_, field_inits);
    field_inits =: list_combine(field_names, field_exprs);
    eval_expr_list(env, field_exprs) -> ResultExprList((field_values, g), new_env)
    { [_] };
    field_map := bindings_to_map(list_combine(field_names, field_values));
    v := NV_Record(field_map);
    --
    ResultExpr((v, g), new_env);
  }

  case EGetField {
    e =: E_GetField(e_record, field_name);
    eval_expr(env, e_record) -> ResultExpr((v_record, g), new_env);
    get_field(field_name, v_record) -> v;
    --
    ResultExpr((v, g), new_env);
  }

  case EGetFields {
    e =: E_GetFields(e_record, field_names);
    eval_expr(env, e_record) -> ResultExpr((v_record, g), new_env);
    ( INDEX(i, field_names: get_field(field_names[i], v_record) -> NV_Literal(L_Bitvector(field_bits[i]))) )
    { ([_,[_]]) };
    field_bitvectors := list_map(bits, field_bits, nvbitvector(bits));
    concat_bitvectors(field_bitvectors) -> v;
    --
    ResultExpr((v, g), new_env);
  }

  case ETuple {
    e =: E_Tuple(es);
    eval_expr_list(env, es) -> ResultExprList((vs, g), new_env);
    v := NV_Vector(vs);
    --
    ResultExpr((v, g), new_env);
  }

  case EArray {
    e =: E_Array[length: e_length, array_value: e_value];
    eval_expr(env, e_value) -> ResultExpr((value, g1), new_env);
    eval_expr_sef(env, e_length) -> ResultExprSEF(v_length, g2);
    v_length =: nvint(n_length);
    de_check(n_length >= zero, DE_NAL) -> True;
    values := if_then_else(
      n_length = zero,
      empty_list,
      list_map(i, range_list(one, n_length), value)
    );
    v := NV_Vector(values);
    g := parallel(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case EEnumArray {
    e =: E_EnumArray[ enum: _, labels: labels, enum_array_value: e_value ];
    eval_expr(env, e_value) -> ResultExpr((value, g), new_env);
    label_value_pairs := list_map(l, labels, (l, value));
    field_map := bindings_to_map(label_value_pairs);
    v := NV_Record(field_map);
    --
    ResultExpr((v, g), new_env);
  }

  case EArbitrary {
    e =: E_Arbitrary(ty);
    dynamic_domain(env, ty) -> d;
    case okay {
      d =: disjoint_union(match_set(v),  d');
      --
      ResultExpr((v, empty_graph), env);
    }
    case error {
      d = empty_set;
      --
      DynamicError(DE_AET);
    }
  }

  case EPattern {
    e =: E_Pattern(e1, p);
    eval_expr(env, e1) -> ResultExpr((v1, g1), new_env);
    eval_pattern(env, v1, p) -> ResultPattern(v, g2);
    g := ordered_data(g1, g2);
    --
    ResultExpr((v, g), new_env);
  }

  case EGetCollectionFields {
    e =: E_GetCollectionFields(base, field_names);
    env =: (_, denv);
    map_apply_opt(denv.dynamic_envs_G.storage, base) =: some(record);
    record =: NV_Record(field_map);
    ( INDEX(i, field_names: get_field(field_names[i], record) -> NV_Literal(L_Bitvector(bits[i]))) )
    { ([_, [_]]) };
    vs := list_map(b, bits, nvbitvector(b));
    concat_bitvectors(vs) -> v;
    field_ids := list_map(f, field_names, concat(base, dot_str, f));
    INDEX(i, field_ids: read_identifier(field_ids[i], vs[i]) -> gs[i]);
    g := parallel_graphs(gs);
    new_env := env;
    --
    ResultExpr((v, g), new_env);
  }

  case EATC {
    e =: E_ATC(e1, t);
    eval_expr(env, e1) -> ResultExpr((v, g1), new_env);
    is_val_of_type(env, v, t) -> (b, g2);
    case okay {
      b = True;
      g := ordered_data(g1, g2);
      --
      ResultExpr((v, g), new_env);
    }
    case error {
      b = False;
      --
      DynamicError(DE_TAF);
    }
  }
;

render rule eval_expr_ELit = eval_expr(ELit);
render rule eval_expr_EVar = eval_expr(EVar);
render rule eval_expr_BinopAnd = eval_expr(BinopAnd);
render rule eval_expr_BinopOr = eval_expr(BinopOr);
render rule eval_expr_BinopImpl = eval_expr(BinopImpl);
render rule eval_expr_Binop = eval_expr(Binop);
render rule eval_expr_Unop = eval_expr(Unop);
render rule eval_expr_EATC = eval_expr(EATC);
render rule eval_expr_ECond = eval_expr(ECond);
render rule eval_expr_ECall = eval_expr(ECall);
render rule eval_expr_ESlice = eval_expr(ESlice);
render rule eval_expr_EGetArray = eval_expr(EGetArray);
render rule eval_expr_EGetEnumArray = eval_expr(EGetEnumArray);
render rule eval_expr_EGetTupleItem = eval_expr(EGetTupleItem);
render rule eval_expr_ERecord = eval_expr(ERecord);
render rule eval_expr_EGetField = eval_expr(EGetField);
render rule eval_expr_EGetFields = eval_expr(EGetFields);
render rule eval_expr_ETuple = eval_expr(ETuple);
render rule eval_expr_EArray = eval_expr(EArray);
render rule eval_expr_EEnumArray = eval_expr(EEnumArray);
render rule eval_expr_EArbitrary = eval_expr(EArbitrary);
render rule eval_expr_EPattern = eval_expr(EPattern);
render rule eval_expr_EGetCollectionFields = eval_expr(EGetCollectionFields);

semantics relation eval_expr_sef(env: envs, e: expr) -> ResultExprSEF(v: native_value, g: XGraphs) | TDynError | TDiverging
{
   prose_description = "specializes the expression evaluation relation for
                        side-effect-free expressions by omitting throwing
                        configurations as possible output configurations.",
 prose_application = "",
} =
  eval_expr(env, e) -> ResultExpr((v, g), _) | DynErrorConfig(), DivergingConfig();
  --
  ResultExprSEF(v, g);
;

semantics relation is_val_of_type(env: envs, v: native_value, t: ty) ->
         (b: Bool, g: XGraphs) | TDynError | TDiverging
{
  "tests whether the value {v} can be stored in a
  variable of type {t} in the environment {env},
  resulting in a Boolean value {b} and execution graph
  {g}. \ProseOtherwiseDynamicErrorOrDiverging",
  prose_application = "\hyperlink{relation-isvaloftype}{testing} if value {v} matches type {t} in {env} yields result {b} and graph {g}",
} =
  case type_equal {
    ast_label(t) not_in make_set(label_T_Bits, label_T_Int, label_T_Tuple);
    --
    (True, empty_graph);
  }

  case int_unconstrained {
    t = unconstrained_integer;
    --
    (True, empty_graph);
  }

  case int_wellconstrained {
    t =: T_Int(typed_WellConstrained(cs, _));
    v =: nvint(n);
    list_combine(cs_sat, cs_graphs) := list_map(c, cs, is_constraint_sat(env, c, n));
    any_sat := list_or(cs_sat);
    g := parallel_graphs(cs_graphs);
    --
    (any_sat, g);
  }

  case bits {
    t =: T_Bits(e, _);
    v =: nvbitvector(bits);
    eval_expr_sef(env, e) -> ResultExprSEF(v_len, g);
    v_len =: nvint(n);
    --
    (n_to_n_pos(n) = list_len(bits), g);
  }

  case tuple {
    t =: T_Tuple(tys);
    v =: NV_Vector(values);
    INDEX(i, tys: is_val_of_type(env, values[i], tys[i]) -> (bs[i], gs[i]));
    --
    (list_and(bs), parallel_graphs(gs));
  }
;

semantics relation is_constraint_sat(env: envs, c: int_constraint, n: Z) ->
         (b: Bool, g: XGraphs)
{
  "tests whether the integer value $n$ \emph{satisfies
  the constraint} {c} (that is, whether $n$ is within
  the range of values defined by {c}) in the environment
  {env} and returns a Boolean answer {b} and the
  execution graph {g} resulting from evaluating the
  expressions appearing in {c}.",
  prose_application = "\hyperlink{relation-isconstraintsat}{verifying} integer {n} satisfies constraint {c} in {env} yields {b} and graph {g}",
} =
  case exact {
    c =: Constraint_Exact(e);
    eval_expr_sef(env, e) -> ResultExprSEF(nvint(m), g);
    --
    (m = n, g);
  }

  case range {
    c =: Constraint_Range(e1, e2);
    eval_expr_sef(env, e1) -> ResultExprSEF(nvint(a), g1);
    eval_expr_sef(env, e2) -> ResultExprSEF(nvint(b), g2);
    g := parallel(g1, g2);
    --
    (and(a <= n && n <= b), g);
  }
;

semantics relation eval_expr_list(env: envs, le: list0(expr)) ->
         ResultExprList((v: list0(native_value), g: XGraphs), new_env: envs) | TThrowing | TDynError | TDiverging
{
  "evaluates the list of expressions {le} in
  left-to-right order in the initial environment {env}
  and returns the resulting list of values {v}, the
  parallel composition of the execution graphs generated
  from evaluating each expression, and the new
  environment {new_env}. \ProseOtherwiseAbnormal",
  prose_application = "\hyperlink{relation-evalexprlist}{evaluating} expressions {le} in {env} yields values {v}, graph {g}, and environment {new_env}",
  math_layout = [_,_],
} =
  case empty {
    le = empty_list;
    --
    ResultExprList((empty_list, empty_graph), env);
  }

  case non_empty {
    le =: match_cons(e, le1);
    eval_expr(env, e) -> ResultExpr((v1, g1), env1);
    eval_expr_list(env1, le) -> ResultExprList((vs, g2), new_env);
    v := match_cons(v1, vs);
    g := parallel(g1, g2);
    --
    ResultExprList((v, g), new_env);
  }
;

////////////////////////////////////////////////////////////////////////////////
// Assignable Expressions Relations
////////////////////////////////////////////////////////////////////////////////

typing relation annotate_lexpr(tenv: static_envs, le: lexpr, t_e: ty) ->
   (new_le: lexpr, ses: powerset(TSideEffect)) | type_error
{
    "annotates the \assignableexpression{} {le} with type {t_e}
        (the type of the corresponding \rhsexpression{})
        in the \staticenvironmentterm{} {tenv},
        resulting in the following:
        {new_le} is the annotated \assignableexpression{},
        and {ses} is the \sideeffectsetterm{} inferred for {le}. \ProseOtherwiseTypeError",
    prose_application = "annotating {le} with {t_e} in {tenv} yields {new_le} and {ses}\ProseOrTypeError",
} =
  case LEDiscard {
    le = LE_Discard;
    --
    (LE_Discard, empty_set);
  }

  case LEVar {
    le =: LE_Var(x);

    case local {
      tenv.static_envs_L.local_storage_types(x) =: (t, k);
      te_check(k = LDK_Var, TE_AIM) -> True;
      check_type_satisfies(tenv, t_e, t) -> True;
      ses := make_set(Immutability(False), LocalEffect(SE_Impure));
      --
      (LE_Var(x), ses);
    }

    case global {
      tenv.static_envs_L.local_storage_types(x) = bot;
      tenv.static_envs_G.global_storage_types(x) =: (t, k);
      te_check(k = GDK_Var, TE_AIM) -> True;
      check_type_satisfies(tenv, t_e, t) -> True;
      ses := make_set(GlobalEffect(SE_Impure), Immutability(False));
      --
      (LE_Var(x), ses);
    }

    case error_undefined {
      tenv.static_envs_L.local_storage_types(x) = bot;
      tenv.static_envs_G.global_storage_types(x) = bot;
      --
      TypeError(TE_UI);
    }
  }

  case LEDestructuring {
    le =: LE_Destructuring(les);
    te_check(ast_label(t_e) = label_T_Tuple, TE_UT) -> True;
    t_e =: T_Tuple(tys);
    te_check(same_length(les, tys), TE_UT) -> True;
    (
      INDEX(i, les: annotate_lexpr(tenv, les[i], tys[i]) -> (les'[i], xs[i]))
    );
    ses := union_list(xs);
    --
    (LE_Destructuring(les'), ses);
  }

  case LESetArray {
    le =: LE_SetArray(e_base, e_index);
    annotate_expr(tenv, rexpr(e_base)) -> (t_base, _, _);
    make_anonymous(tenv, t_base) -> t_anon_base;
    te_check(ast_label(t_anon_base) = label_T_Array, TE_UT) -> True;
    t_anon_base =: T_Array(size, t_elem);
    annotate_lexpr(tenv, e_base, t_base) -> (e_base', ses_base);
    annotate_set_array(tenv, (size, t_elem), t_e, (e_base', ses_base, e_index)) -> (new_le, ses)
    { math_layout = [_] };
    --
    (new_le, ses);
  }

  case LESlice {
    le =: LE_Slice(le1, slices);
    annotate_expr(tenv, rexpr(le1)) -> (t_le1, _, _);
    make_anonymous(tenv, t_le1) -> t_le1_anon;
    te_check(ast_label(t_le1_anon) = label_T_Bits, TE_UT) -> True;
    annotate_lexpr(tenv, le1, t_le1) -> (le2, ses1);
    annotate_slices(tenv, slices) -> (slices_annot, ses_slices);
    slices_width(tenv, slices_annot) -> e_w;
    normalize(tenv, e_w) -> v_w;
    t := T_Bits(v_w, empty_list);
    check_type_satisfies(tenv, t_e, t) -> True;
    check_disjoint_slices(tenv, slices_annot) -> True;
    te_check(slices_annot != empty_list, TE_BS) -> True;
    ses := union(ses1, ses_slices);
    --
    (LE_Slice(le2, slices_annot), ses);
  }

  case LESetField {
    le =: LE_SetField(le1, field_name);
    annotate_expr(tenv, rexpr(le1)) -> (t_le1, _, _);
    annotate_lexpr(tenv, le1, t_le1) -> (le2, ses);
    make_anonymous(tenv, t_le1) -> t_le1_anon;

    case structured {
      t_le1_anon =: make_structured(L, fields);
      L in make_set(label_T_Exception, label_T_Record);
      assoc_opt(fields, field_name) =: t_opt;
      te_check(t_opt != None, TE_BF) -> True;
      t_opt =: some(t_field);
      check_type_satisfies(tenv, t_e, t_field) -> True;
      --
      (LE_SetField(le2, field_name), ses);
    }

    case collection {
      t_le1_anon =: T_Collection(fields);
      le2 =: LE_Var(collection_var_name);
      assoc_opt(fields, field_name) =: t_opt;
      te_check(t_opt != None, TE_BF) -> True;
      t_opt =: some(t);
      check_type_satisfies(tenv, t_e, t) -> True;
      get_bitvector_const_width(tenv, t) -> n;
      --
      (LE_SetCollectionFields(collection_var_name, make_singleton_list(field_name), make_singleton_list((zero, n))), ses)
      { math_layout = [_] };
    }

    case bitfield {
      t_le1_anon =: T_Bits(_, bitfields);
      find_bitfield_opt(field_name, bitfields) -> bitfield_opt;
      case found {
        case simple {
          bitfield_opt =: some(BitField_Simple(_, slices));
          slices_width(tenv, slices) -> vw;
          t := T_Bits(vw, empty_list);
        }

        case nested {
          bitfield_opt =: some(BitField_Nested(_, slices, bitfields'))
          { math_layout = [_] };
          slices_width(tenv, slices) -> vw;
          t := T_Bits(vw, bitfields');
        }

        case typed {
          bitfield_opt =: some(BitField_Type(_, slices, t_field))
          { math_layout = [_] };
          slices_width(tenv, slices) -> w;
          t := T_Bits(w, empty_list);
          check_type_satisfies(tenv, t, t_field) -> True;
        }
        check_type_satisfies(tenv, t_e, t) -> True;
        annotate_lexpr(tenv, LE_Slice(le1, slices), t_e) -> (new_le, ses2);
        --
        (new_le, ses2);
      }

      case missing {
        bitfield_opt = None;
        --
        TypeError(TE_BF);
      }
    }

    case error {
      ast_label(t_le1_anon) not_in make_set(label_T_Bits, label_T_Collection, label_T_Exception, label_T_Record)
      { math_layout = (_, [_]) };
      --
      TypeError(TE_UT);
    }
  }

 case LESetFields {
   le =: LE_SetFields(le_base, le_fields);
   annotate_expr(tenv, rexpr(le_base)) -> (t_base, _, _);
   annotate_lexpr(tenv, le_base, t_base) -> (le_base_annot, ses_base);
   make_anonymous(tenv, t_base) -> t_base_anon;

   case bits {
     t_base_anon =: T_Bits(_, bitfields);
     ( INDEX(i, le_fields: find_bitfields_slices(le_fields[i], bitfields) -> slices[i]) )
     { ([_]) };
     le_slice := LE_Slice(le_base_annot, list_flatten(slices));
     annotate_lexpr(tenv, le_slice, t_e) -> (new_le, ses);
     --
     (new_le, ses);
   }

   case record {
     t_base_anon =: T_Record(base_fields);
     fold_bitvector_fields(tenv, base_fields, le_fields) -> (length, slices);
     vt_lhs := T_Bits(ELint(length), empty_list);
     check_type_satisfies(tenv, t_e, vt_lhs) -> True;
     --
     (typed_LE_SetFields(le_base_annot, le_fields, slices), ses_base)
     { math_layout = [_] };
   }

   case collection {
     t_base_anon =: T_Collection(base_fields);
     le_base =: LE_Var(base_name);
     fold_bitvector_fields(tenv, base_fields, le_fields) -> (length, slices);
     t_lhs := T_Bits(ELint(length), empty_list);
     check_type_satisfies(tenv, t_e, t_lhs) -> True;
     --
     (LE_SetCollectionFields(base_name, le_fields, slices), ses_base)
     { math_layout = [_, [_]] };
   }

   case error {
     ast_label(t_base_anon) not_in make_set(label_T_Bits, label_T_Collection, label_T_Record);
     --
     TypeError(TE_UT);
   }
 }
;

render rule annotate_lexpr_LEDiscard = annotate_lexpr(LEDiscard);
render rule annotate_lexpr_LEVar = annotate_lexpr(LEVar);
render rule annotate_lexpr_LEDestructuring = annotate_lexpr(LEDestructuring);
render rule annotate_lexpr_LESetArray = annotate_lexpr(LESetArray);
render rule annotate_lexpr_LESlice = annotate_lexpr(LESlice);
render rule annotate_lexpr_LESetStructuredField = annotate_lexpr(LESetField.structured);
render rule annotate_lexpr_LESetCollectionField = annotate_lexpr(LESetField.collection);
render rule annotate_lexpr_LESetField_BitField = annotate_lexpr(LESetField.bitfield);
render rule annotate_lexpr_LESetBadField_error = annotate_lexpr(LESetField.error);
render rule annotate_lexpr_LESetFields = annotate_lexpr(LESetFields);

// TODO: change aslspec to allow the signature in comment.
//semantics relation eval_lexpr(env: envs, le: lexpr, m: (v: native_value, g: XGraphs)) ->
semantics relation eval_lexpr(env: envs, le: lexpr, m: (native_value, XGraphs)) ->
        | ResultLexpr(new_g: XGraphs, new_env: envs)
        | TThrowing
        | TDynError
        | TDiverging
{
    "evaluates the assignment of the value-graph pair {m}
        to the \assignableexpression{} {le} in the environment {env},
        resulting in the configuration $\ResultLexpr({new_g}, {new_env})$. \ProseOtherwiseAbnormal",
    prose_application = "evaluating the assignment of {m} to {le} in {env}
        yields $\ResultLexpr({new_g}, {new_env})$\ProseOrAbnormal",
    math_layout = (_, [_,_,_,_]),
} =
  case LEDiscard {
    le = LE_Discard;
    m =: (v, g);
    --
    ResultLexpr(g, env);
  }

  case LEVar {
    le =: LE_Var(name);
    env =: (tenv, denv);
    m =: (v, g);
    case local {
      map_apply_opt(denv.dynamic_envs_L, name) = some(_);
      updated_local := map_update(denv.dynamic_envs_L, name, v);
      new_denv := denv(dynamic_envs_L: updated_local);
    }

    case global {
      map_apply_opt(denv.dynamic_envs_G.storage, name) = some(_);
      updated_storage := map_update(denv.dynamic_envs_G.storage, name, v);
      new_gdenv := denv.dynamic_envs_G(storage: updated_storage);
      new_denv := denv(dynamic_envs_G: new_gdenv);
    }
    new_env := (tenv, new_denv);
    write_identifier(name, v) -> g1;
    new_g := ordered_data(g, g1);
    --
    ResultLexpr(new_g, new_env);
  }

  case LEDestructuring {
    le =: LE_Destructuring(le_list);
    m =: (v, g);
    INDEX(i, le_list: get_index(i, v) -> r_vals[i]);
    vmlist := list_map(r_val, r_vals, (r_val, g));
    eval_multi_assignment(env, le_list, vmlist) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESetArray {
    le =: LE_SetArray(re_array, e_index);
    m =: (v, g);
    eval_expr(env, rexpr(re_array)) -> ResultExpr(rm_array, env1);
    eval_expr(env1, e_index) -> ResultExpr(m_index, env2);
    m_index =: (index, g1);
    index =: nvint(n_to_n_pos(i));
    rm_array =: (rv_array, g2);
    set_index(i, v, rv_array) -> v1;
    m1 := (v1, ordered_data(g, parallel(g1, g2)));
    eval_lexpr(env2, re_array, m1) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESlice {
    le =: LE_Slice(e_bv, slices);
    m =: (v, g);
    eval_expr(env, rexpr(e_bv)) -> ResultExpr(m_bv, env1);
    eval_slices(env1, slices) -> ResultSlices((slice_ranges, g1), env2);
    m_bv =: (v_bv, g2);
    check_non_overlapping_slices(slice_ranges) -> True;
    write_to_bitvector(slice_ranges, v, v_bv) -> v1;
    g3 := ordered_data(g, parallel(g1, g2));
    eval_lexpr(env2, e_bv, (v1, g3)) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESetEnumArray {
    le =: LE_SetEnumArray(re_array, e_index);
    m =: (v, g);
    eval_expr(env, rexpr(re_array)) -> ResultExpr(rm_array, env1);
    eval_expr(env1, e_index) -> ResultExpr(m_index, env2);
    m_index =: (index, g1);
    index =: NV_Literal(L_Label(l));
    rm_array =: (rv_array, g2);
    set_field(l, v, rv_array) -> v1;
    m1 := (v1, ordered_data(g, parallel(g1, g2)));
    eval_lexpr(env2, re_array, m1) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESetField {
    le =: LE_SetField(re_record, field_name);
    m =: (v, g);
    eval_expr(env, rexpr(re_record)) -> ResultExpr(rm_record, env1);
    rm_record =: (rv_record, g1);
    set_field(field_name, v, rv_record) -> v1;
    m1 := (v1, ordered_data(g, g1));
    eval_lexpr(env1, re_record, m1) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESetFields {
    le =: typed_LE_SetFields(le_record, fields, slices);
    m =: (v, g);
    eval_expr(env, rexpr(le_record)) -> ResultExpr(rm_record, env1);
    rm_record =: (v_record, g1);
    assign_bitvector_fields(v, v_record, fields, slices) -> v2;
    m2 := (v2, ordered_data(g, g1));
    eval_lexpr(env1, le_record, m2) -> ResultLexpr(new_g, new_env);
    --
    ResultLexpr(new_g, new_env);
  }

  case LESetCollectionFields {
    le =: LE_SetCollectionFields(base, field_names, slices);
    m =: (v, g);
    env =: (tenv, denv);
    map_apply_opt(denv.dynamic_envs_G.storage, base) =: some(record);
    assign_bitvector_fields(v, record, field_names, slices) -> record1;
    field_ids := list_map(f, field_names, concat(base, dot_str, f));
    INDEX(i, field_ids: write_identifier(field_ids[i], v) -> gs[i]);
    g_zero := parallel_graphs(gs);
    updated_storage := map_update(denv.dynamic_envs_G.storage, base, record1);
    new_gdenv := denv.dynamic_envs_G(storage: updated_storage);
    new_denv := denv(dynamic_envs_G: new_gdenv);
    new_env := (tenv, new_denv);
    new_g := ordered_data(g, g_zero);
    --
    ResultLexpr(new_g, new_env);
  }
;

render rule eval_lexpr_LEDiscard = eval_lexpr(LEDiscard);
render rule eval_lexpr_LEVar = eval_lexpr(LEVar);
render rule eval_lexpr_LEDestructuring = eval_lexpr(LEDestructuring);
render rule eval_lexpr_LESetArray = eval_lexpr(LESetArray);
render rule eval_lexpr_LESetEnumArray = eval_lexpr(LESetEnumArray);
render rule eval_lexpr_LESlice = eval_lexpr(LESlice);
render rule eval_lexpr_LESetField = eval_lexpr(LESetField);
render rule eval_lexpr_LESetFields = eval_lexpr(LESetFields);
render rule eval_lexpr_LESetCollectionFields = eval_lexpr(LESetCollectionFields);

semantics relation eval_multi_assignment(env: envs, lelist: list0(lexpr), vmlist: list0((native_value, XGraphs))) ->
        | ResultLexpr(new_g: XGraphs, new_env: envs)
        | TThrowing
        | TDynError
        | TDiverging
{
    "evaluates multi-assignments. That is, the simultaneous assignment of the list of value-\executiongraphterm{} pairs {vmlist}
    to the corresponding list of \assignableexpressions{} {lelist}, in the environment {env}.
    The result is either the \executiongraphterm{} {new_g} and new environment {new_env} or an abnormal configuration",
    prose_application = "evaluating multi-assignment of {vmlist} to {lelist} in {env} yields $\ResultLexpr({new_g}, {new_env})$ or abnormal configuration",
    math_macro = \evalmultiassignment,
    math_layout = (_, [_,_,_,_]),
} =
  case empty {
    lelist = empty_list;
    vmlist = empty_list;
    new_g := empty_graph;
    new_env := env;
    --
    ResultLexpr(new_g, new_env);
  }

  case non_empty {
    lelist =: match_cons(le, lelist1);
    vmlist =: match_cons(vm, vmlist1);
    eval_lexpr(env, le, vm) -> ResultLexpr(g1, env1);
    eval_multi_assignment(env1, lelist1, vmlist1) -> ResultLexpr(g2, new_env);
    new_g := ordered_po(g1, g2);
    --
    ResultLexpr(new_g, new_env);
  }
;

typing relation annotate_set_array(
  tenv: static_envs,
  (size: array_index, t_elem: ty),
  rhs_ty: ty,
  (e_base: lexpr, ses_base: powerset(TSideEffect), e_index: expr)
  ) ->
    (new_le: lexpr, ses: powerset(TSideEffect)) | type_error
{
  "annotates an array update in the \staticenvironmentterm{} {tenv}
  where {size} is kind of array index and {t_elem} is the type of array elements,
  {rhs_ty} is the type of the \rhsexpression{},
  the annotated array based expression is {e_base},
  the \sideeffectsetterm{} {ses_base} inferred for the base,
  and the index expression {e_index}.
  The result is the annotated \assignableexpression{} {new_le} and \sideeffectsetterm{} for the annotated expression {ses}. \ProseOtherwiseTypeError",
  prose_application = "annotating array update in {tenv} with {size}, {t_elem}, {rhs_ty}, {e_base}m {ses_base}, and {e_index} yields {new_le} and {ses}\ProseOrTypeError",
  math_layout = [[_,_,_,_],_],
} =
  check_type_satisfies(tenv, rhs_ty, t_elem) -> True;
  annotate_expr(tenv, e_index) -> (t_index', e_index', ses_index);
  type_of_array_length(size) -> wanted_t_index;
  check_type_satisfies(tenv, t_index', wanted_t_index) -> True;
  ses := union(ses_base, ses_index);
  new_le := if_then_else(
    equal(ast_label(size), label_ArrayLength_Expr),
    LE_SetArray(e_base, e_index'),
    LE_SetEnumArray(e_base, e_index')
  ) { (_, [_]) };
  --
  (new_le, ses) { ([_], _) };
;

typing function check_disjoint_slices(tenv: static_envs, slices: list0(slice)) ->
         CheckResult | type_error
{
    "checks whether the list of slices {slices} do not overlap in {tenv}, yielding $\True$. \ProseOtherwiseTypeError",
    prose_application = "checking whether {slices} are disjoint in {tenv} yields $\True$\ProseOrTypeError",
} =
  disjoint_slices_to_positions(tenv, False, slices) -> positions;
  --
  True;
;

semantics function check_non_overlapping_slices(value_ranges: list0((native_value, native_value))) ->
         CheckResult | TDynError
{
    "checks whether the sets of integers represented by the list of ranges {value_ranges} overlap, yielding $\True$. \ProseOtherwiseDynamicErrorOrDiverging",
    prose_application = "checking whether {value_ranges} are non-overlapping yields $\True$\ProseOrDynamicErrorOrDiverging",
} =
  case empty {
    value_ranges = empty_list;
    --
    True;
  }

  case non_empty {
    value_ranges =: match_cons(head_range, tail_ranges);
    ( INDEX(i, tail_ranges: check_two_ranges_non_overlapping(head_range, tail_ranges[i]) -> True) )
    { ([_, [_]]) };
    check_non_overlapping_slices(tail_ranges) -> True;
    --
    True;
  }
;

semantics function check_two_ranges_non_overlapping(
  range1: (native_value, native_value),
  range2: (native_value, native_value)) ->
         CheckResult | TDynError
{
    "checks whether two sets of integers represented by the ranges {range1} and {range2} do not intersect, yielding $\True$. \ProseOtherwiseDynamicError",
    prose_application = "checking whether {range1} and {range2} do not intersect yields $\True$\ProseOrError",
    math_layout = [_, _],
} =
  range1 =: (s1, l1);
  range2 =: (s2, l2);
  eval_binop(ADD, s1, l1) -> s1_l1;
  eval_binop(LE, s1_l1, s2) -> s1_l1_le_s2;
  eval_binop(ADD, s2, l2) -> s2_l2;
  eval_binop(LE, s2_l2, s1) -> s2_l2_le_s1;
  eval_binop(BOR, s1_l1_le_s2, s2_l2_le_s1) -> v_b;
  v_b =: nvbool(b);
  de_check(b, DE_OSA) -> True;
  --
  True;
;

typing function fold_bitvector_fields(tenv: static_envs, base_fields: list0(field), le_fields: list0(Identifier)) ->
         (length: Z, slices: list0((start: Z, width: Z)))
{
    "accepts a \staticenvironmentterm{} {tenv}, the list of all fields {base_fields} for a record type, and a list of fields {le_fields} that are the subset of the names of fields
    in {base_fields} about to be assigned to, and yields the total width across the fields named in {le_fields} and the ranges corresponding to them in terms of pairs where the first component is the start position and the second component is the width of the field.",
    prose_application = "folding bitvector fields {le_fields} from {base_fields} in {tenv} yields length {length} and slices {slices}",
} =
  case empty {
    le_fields = empty_list;
    --
    (zero, empty_list);
  }

  case non_empty {
    le_fields =: concat(le_fields1, match_singleton_list(field));
    fold_bitvector_fields(tenv, base_fields, le_fields1) -> (start, slices1);
    assoc_opt(base_fields, field) =: ty_opt;
    te_check(ty_opt != None, TE_BF) -> True;
    ty_opt =: some(t_field);
    get_bitvector_const_width(tenv, t_field) -> field_width;
    --
    (start + field_width, concat(make_singleton_list((start, field_width)), slices1))
    { [_] };
  }
;

semantics function assign_bitvector_fields(
  bitvector: native_value,
  record: native_value,
  fields: list0(Identifier),
  slices: list0((Z, Z))) ->
    (result: native_value) | TDynError
{
    "updates the list of fields {fields} of {record} with the slices given by {slices} from \\
    {bitvector}, yielding the \nativevalueterm{} {result}",
    prose_application = "assigning {bitvector} slices {slices} to fields {fields} of {record} yields {result}",
} =
  case empty {
    fields = empty_list;
    slices = empty_list;
    --
    record;
  }

  case non_empty {
    fields =: match_cons(field_name, fields1);
    slices =: match_cons((i1, i2), slices1);
    slice := make_singleton_list((nvint(i1), nvint(i2)));
    read_from_bitvector(bitvector, slice) -> record_slices;
    set_field(field_name, record_slices, record) -> record1;
    assign_bitvector_fields(bitvector, record1, fields1, slices1) -> result;
    --
    result;
  }
;

//////////////////////////////////////////////////
// Relations for Base Values

typing function base_value(tenv: static_envs, t: ty) ->
         (e_init: expr) | type_error
{
  "returns the expression {e_init} which can be used to
  initialize a storage element of type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-basevalue}{computing} initial value for type {t} in {tenv} yields expression {e_init}",
} =
  case t_bool {
    t = T_Bool;
    --
    E_Literal(L_Bool(False));
  }

  case t_bits_static {
    t =: T_Bits(e, _);
    reduce_to_z_opt(tenv, e) -> some(length);
    te_check(length >= zero, TE_NBV) -> True;
    --
    E_Literal(L_Bitvector(list_map(i, range_list(one, length), zero_bit)));
  }

  case t_bits_non_static {
    t =: T_Bits(e, _);
    reduce_to_z_opt(tenv, e) -> None;
    e_init := E_Slice(ELint(zero), make_singleton_list(Slice_Length(ELint(zero), e)));
    --
    e_init;
  }

  case t_enum {
    t =: T_Enum(match_non_empty_cons(name, _));
    lookup_constant(tenv, name) -> some(l);
    --
    E_Literal(l);
  }

  case t_int_unconstrained {
    t = unconstrained_integer;
    --
    E_Literal(L_Int(zero));
  }

  case t_int_parameterized {
    t =: T_Int(Parameterized(id));
    --
    TypeError(TE_NBV);
  }

  case t_int_wellconstrained {
    t =: T_Int(WellConstrained(cs));
    INDEX(i, cs: constraint_abs_min(tenv, cs[i]) -> z_min_lists[i]);
    z_min_list := list_flatten(z_min_lists);
    te_check(z_min_list != empty_list, TE_NBV) -> True;
    list_min_abs(z_min_list) -> z_min;
    --
    E_Literal(L_Int(z_min));
  }

  case t_named {
    t =: T_Named(id);
    make_anonymous(tenv, T_Named(id)) -> t';
    base_value(tenv, t') -> e_init;
    --
    e_init;
  }

  case t_real {
    t = T_Real;
    --
    E_Literal(L_Real(rational_zero));
  }

  case structured {
    is_structured(t) -> True;
    t =: make_structured(L, fields);
    fields =: list_combine(field_names, field_types);
    ( INDEX(i, field_types: base_value(tenv, field_types[i]) -> field_base_values[i]) ) { ( [_] ) };
    e := list_combine(field_names, field_base_values);
    --
    E_Record(t, e);
  }

  case t_string {
    t = T_String;
    --
    E_Literal(L_String(empty_list));
  }

  case t_tuple {
    t =: T_Tuple(ts);
    INDEX(i, ts: base_value(tenv, ts[i]) -> es[i]);
    --
    E_Tuple(match_non_empty_list(es));
  }

  case t_array_enum {
    t =: T_Array(ArrayLength_Enum(enum, labels), ty);
    base_value(tenv, ty) -> value;
    --
    E_EnumArray [ enum : enum, labels : labels, enum_array_value : value ];
  }

  case t_array_expr {
    t =: T_Array(ArrayLength_Expr(length), ty);
    base_value(tenv, ty) -> value;
    --
    E_Array[ length : length, array_value : value ];
  }
;

typing function constraint_abs_min(tenv: static_envs, c: int_constraint) ->
         (zs: list0(Z)) | type_error
{
  "returns a single element list containing the integer
  closest to $0$ that satisfies the constraint {c} in
  {tenv}, if one exists, and an empty list if the
  constraint represents an empty set. Otherwise, the
  result is $\TypeErrorVal{\NoBaseValue}$.",
  prose_application = "\hyperlink{relation-constraintabsmin}{finding} minimal absolute value satisfying constraint {c} in {tenv} yields {zs}",
} =
  case exact {
    c =: Constraint_Exact(e);
    reduce_to_z_opt(tenv, e) -> z_opt;
    te_check(z_opt != None, TE_NBV) -> True;
    z_opt =: some(z);
    --
    make_singleton_list(z);
  }

  case range {
    c =: Constraint_Range(e1, e2);
    reduce_to_z_opt(tenv, e1) -> z_opt1;
    te_check(z_opt1 != None, TE_NBV) -> True;
    z_opt1 =: some(v1);
    reduce_to_z_opt(tenv, e2) -> z_opt2;
    te_check(z_opt2 != None, TE_NBV) -> True;
    z_opt2 =: some(v2);
    zs := cond(
            v1 > v2                 : empty_list,
            v1 <= v2 && v2 < zero   : make_singleton_list(v2),
            v1 < zero && zero <= v2 : make_singleton_list(zero),
            zero <= v1 && v1 <= v2  : make_singleton_list(v1)
          );
    --
    zs;
  }
;

typing function list_min_abs(l: list0(Z)) ->
         (z: Z)
{
  "returns {z} --- the integer closest to $0$ among the
  list of integers in the list {l}. The result is biased
  towards positive integers. That is, if two integers
  $x$ and $y$ have the same absolute value and $x$ is
  positive and $y$ is negative then $x$ is considered
  closer to $0$.",
  prose_application = "\hyperlink{relation-listminabs}{finding} integer closest to zero in list {l} yields {z}",
} =
  case one {
    l =: match_singleton_list(z);
    --
    z;
  }

  case more_than_one {
    l =: match_cons(z1, l2);
    l2 != empty_list;
    list_min_abs(l2) -> z2;
    z := cond(
           abs_value(z1) < abs_value(z2)              : z1,
           abs_value(z1) > abs_value(z2)              : z2,
           z1 = z2                                    : z1,
           abs_value(z1) = abs_value(z2) && z1 != z2  : abs_value(z1)
          );
    --
    z;
  }
;

//////////////////////////////////////////////////
// Relations for Bitfields

typing relation annotate_bitfields(tenv: static_envs, e_width: expr, fields: list0(bitfield)) ->
         (new_fields: list0(bitfield), ses: powerset(TSideEffect)) | type_error
{
  "annotates a list of bitfields {fields} with an
  expression denoting the overall number of bits in the
  containing bitvector type {e_width}, in an
  environment {tenv}, resulting in {new_fields} --- the
  \typedast{} for {fields} and {e_width} as well as a set
  of \sideeffectdescriptorsterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-annotatebitfields}{annotating} bitfields {fields} with width {e_width} in {tenv} yields {new_fields} and side effects {ses}",
  math_layout = [_,_],
} =
  names := list_map(field, fields, bitfield_get_name(field));
  check_no_duplicates(names) -> True;
  static_eval(tenv, e_width) -> L_Int(width);
  (
    INDEX(i, fields: annotate_bitfield(tenv, width, fields[i]) -> (fields'[i], xs[i]))
  ) { math_layout = ([_])};
  ses := union_list(xs);
  --
  (fields', ses);
;

typing function bitfield_get_name(bf: bitfield) ->
         (name: Identifier)
{
  "given a bitfield {bf}, returns {name}, the name of the bitfield {bf}.",
  prose_application = "\hyperlink{relation-bitfieldgetname}{extracting} name from bitfield {bf} yields {name}"
} =
  case simple {
    bf =: BitField_Simple(name, _);
  }
  case nested {
    bf =: BitField_Nested(name, _, _);
  }
  case typed {
    bf =: BitField_Type(name, _, _);
  }
  --
  name;
;

typing function bitfield_get_slices(bf: bitfield) ->
         (slices: list0(slice))
{
  "returns the list of slices {slices} associated with
  the bitfield {bf}.",
  prose_application = "\hyperlink{relation-bitfieldgetslices}{extracting} slices from bitfield {bf} yields {slices}",
} =
  case simple {
    bf =: BitField_Simple(_, slices);
  }
  case nested {
    bf =: BitField_Nested(_, slices, _);
  }
  case typed {
    bf =: BitField_Type(_, slices, _);
  }
  --
  slices;
;

typing function bitfield_get_nested(bf: bitfield) ->
         (nested: list0(bitfield))
{
  "returns the list of bitfields {nested} nested within
  the bitfield {bf}, if there are any, and an empty list
  if there are none.",
  prose_application = "\hyperlink{relation-bitfieldgetnested}{extracting} nested bitfields from {bf} yields {nested}",
} =
  case nested {
    bf =: BitField_Nested(_, _, nested);
    --
    nested;
  }
  case other {
    not(bf = BitField_Nested(_, _, _));
    --
    empty_list;
  }
;

typing relation annotate_bitfield(tenv: static_envs, width: Z, field: bitfield) ->
         (new_field: bitfield, ses: powerset(TSideEffect)) | type_error
{
  "annotates a bitfield {field} with an integer
  {width} indicating the number of bits in the
  bitvector type that contains {field}, in an
  environment {tenv}, resulting in an annotated bitfield
  {new_field} or a \typingerrorterm{}, if one is
  detected.",
  prose_application = "\hyperlink{relation-annotatebitfield}{annotating} bitfield {field} with width {width} in {tenv} yields {new_field} and {ses}",
} =
  case simple {
    field =: BitField_Simple(name, slices);
    annotate_slices(tenv, slices) -> (slices1, ses_slices);
    check_slices_in_width(tenv, width, slices1) -> True;
    --
    (BitField_Simple(name, slices1), ses_slices)
    { math_layout = [_] };
  }

  case nested {
    field =: BitField_Nested(name, slices, bitfields');
    annotate_slices(tenv, slices) -> (slices1, ses_slices);
    disjoint_slices_to_positions(tenv, True, slices1) -> positions;
    check_positions_in_width(width, positions) -> True;
    width' := ELint(cardinality(positions));
    annotate_bitfields(tenv, width', bitfields') -> (bitfields'', ses_bitfields)
    { math_layout = [_] };
    ses := union(ses_slices, ses_bitfields);
    --
    (BitField_Nested(name, slices1, bitfields''), ses)
    { math_layout = [_]};
  }

  case type {
    field =: BitField_Type(name, slices, t);
    annotate_slices(tenv, slices) -> (slices1, ses_slices);
    annotate_type(False, tenv, t) -> (t', ses_ty);
    check_slices_in_width(tenv, width, slices1) -> True;
    disjoint_slices_to_positions(tenv, True, slices1) -> positions;
    check_positions_in_width(width, positions) -> True;
    width' := ELint(cardinality(positions));
    check_bits_equal_width(tenv, T_Bits(width', empty_list), t) -> True;
    ses := union(ses_slices, ses_ty);
    --
    (BitField_Type(name, slices1, t'), ses)
    { math_layout = [_]};
  }
;

typing function check_slices_in_width(tenv: static_envs, width: Z, slices: list0(slice)) ->
         CheckResult | type_error
{
  "checks whether the slices in {slices} fit within the
  bitvector width given by {width} in {tenv}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkslicesinwidth}{verifying} slices {slices} fit within width {width} in {tenv} yields True",
} =
  disjoint_slices_to_positions(tenv, True, slices) -> positions;
  check_positions_in_width(width, positions) -> True;
  --
  True;
;

typing function check_positions_in_width(width: Z, positions: powerset(Z)) ->
         CheckResult | type_error
{
  "checks whether the set of positions in {positions} fit
  within the bitvector width given by {width}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkpositionsinwidth}{verifying} positions {positions} fit within width {width} yields True",
} =
  min_pos := set_min(positions);
  max_pos := set_max(positions);
  te_check( zero <= min_pos && max_pos < width, TE_BS ) -> True;
  --
  True;
;

typing function disjoint_slices_to_positions(tenv: static_envs, is_static: Bool, slices: list0(slice)) ->
         (positions: powerset_finite(Z)) | type_error
{
  "returns the set of integers defined by the list of
  slices in {slices} in {positions}. In particular, this
  rule checks that the following properties:
  \begin{itemize}
  \item bitfield slices do not overlap; and
  \item bitfield slices are not defined in reverse (e.g., \texttt{0:1} rather than \texttt{1:0})
  \end{itemize}
  Conducting the checks for
  these properties requires evaluating the expressions
  comprising the slices, either via static evaluation of
  via normalization. The flag {is_static} determines
  whether the slice is assumed to consist of
  \staticallyevaluableterm{} expressions. If so, the
  slice expressions are statically evaluated, and
  otherwise they are normalized.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-disjointslicestopositions}{converting} disjoint slices {slices} in {tenv} with static flag {is_static} yields positions {positions}",
} =
  case empty {
    slices = empty_list;
    --
    empty_set;
  }

  case non_empty {
    slices =: match_cons(s, slices1);
    bitfield_slice_to_positions(tenv, is_static, s) -> positions1_opt;
    positions1 := if positions1_opt =: some(s1) then s1 else empty_set;
    disjoint_slices_to_positions(tenv, is_static, slices1) -> positions2;
    te_check(intersect(positions1, positions2) = empty_set, TE_BS) -> True;
    --
    union_finite(positions1, positions2);
  }
;

typing function bitfield_slice_to_positions(tenv: static_envs, is_static: Bool, slice: slice) ->
         (positions: option(powerset_finite(Z))) | type_error
{
  "returns the set of integers defined by the bitfield
  slice {slice} in {positions}, if it can be determined
  via static evaluation or normalization, depending on
  {is_static}, and $\None$ if it cannot be determined.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-bitfieldslicetopositions}{converting} slice {slice} in {tenv} with static flag {is_static} yields optional positions {positions}",
} =
  slice =: Slice_Length(e1, e2);
  eval_slice_expr(tenv, is_static, e1) -> some(offset);
  eval_slice_expr(tenv, is_static, e2) -> some(length);
  te_check(offset <= (offset + length - one), TE_BS) -> True;
  --
  some(range_set(offset, offset + length - one)) { math_layout = [_] };
;

semantics relation eval_slice_expr(tenv: static_envs, is_static: Bool, e: expr) ->
         (z_opt: option(Z)) | type_error
{
  "attempts to transform the expression {e} into a
  constant integer in the \staticenvironmentterm{}
  {tenv}, yielding the result in {z_opt}, where $\None$
  indicates it could not be transformed into a constant
  integer. If {is_static} is $\True$, then {e} is known
  to be \staticallyevaluableterm, and the transformation
  is carried out via static evaluation. Otherwise, the
  transformation is carried out via normalization.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-evalsliceexpr}{evaluating} slice expression {e} in {tenv} with static flag {is_static} yields optional integer {z_opt}",
} =
  case static {
    is_static = True;
    static_eval(tenv, e) -> L_Int(z);
    --
    some(z);
  }

  case symbolic {
    is_static = False;
    reduce_to_z_opt(tenv, e) -> z_opt;
    --
    z_opt;
  }
;

typing function check_common_bitfields_align(tenv: static_envs, bitfields: list0(bitfield), width: N) ->
         CheckResult | type_error
{
  "checks \RequirementRef{BitfieldAlignment} for every
  pair of bitfields in {bitfields}, contained in a
  bitvector type of width {width} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkcommonbitfieldsalign}{checking} alignment of bitfields {bitfields} of width {width} in {tenv} yields True",
} =
  last_index := width - one;
  top_absolute := (empty_list, range_list(last_index, zero));
  bitfields_to_absolute(tenv, bitfields, top_absolute) -> fs;
  te_check(forall(f1, fs, forall(f2, fs, absolute_bitfields_align(f1, f2))), TE_BS) -> True
  { math_layout = [_] };
  --
  True;
;

typing function bitfields_to_absolute(tenv: static_envs, bitfields: list0(bitfield), absolute_parent: TAbsField) ->
         (abs_bitfields: powerset(TAbsField))
{
  "returns the set of \absolutebitfields{} {abs_bitfields}
  that correspond to the list of bitfields {bitfields},
  whose \bitfieldscope{} and \absoluteslice{} is given by
  {absolute_parent}, in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "\hyperlink{relation-bitfieldstoabsolute}{converting} bitfields {bitfields} with parent {absolute_parent} in {tenv} yields absolute bitfields {abs_bitfields}",
} =
  abs_field_sets := list_map(i, indices(bitfields), bitfield_to_absolute(tenv, bitfields[i], absolute_parent))
  { math_layout = (_, [_])};
  --
  union_list(abs_field_sets)
  { [_] };
;

typing function bitfield_to_absolute(tenv: static_envs, bf: bitfield, absolute_parent: TAbsField) ->
         (abs_bitfields: powerset(TAbsField))
{
  "returns the set of \absolutebitfields{} {abs_bitfields}
  that correspond to the bitfields nested in {bf},
  including itself, where the \bitfieldscope{} and
  \absoluteslice{} of the bitfield containing {bf} are
  {absolute_parent}, in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "\hyperlink{relation-bitfieldtoabsolute}{converting} bitfield {bf} with parent {absolute_parent} in {tenv} yields absolute bitfields {abs_bitfields}",
} =
  bitfield_get_name(bf) -> name;
  (absolute_name, absolute_slices) := absolute_parent;
  bf_name := concat(absolute_name, make_singleton_list(name));
  bitfield_get_slices(bf) -> slices;
  INDEX(i, slices: slice_to_indices(tenv, slices[i]) -> indices[i]);
  slices_as_indices := list_flatten(indices);
  select_indices_by_slices(absolute_slices, slices_as_indices) -> bf_indices;
  bf_absolute := (bf_name, bf_indices);
  bitfield_get_nested(bf) -> nested;
  bitfields_to_absolute(tenv, nested, bf_absolute) -> abs_bitfields1;
  --
  union(make_set(bf_absolute), abs_bitfields1)
  { math_layout = [_] };
;

typing function select_indices_by_slices(indices: list0(Z), slice_indices: list0(Z)) ->
         (absolute_slice: list0(Z))
{
  "considers the list {indices} as a list of indices into
  a bitvector type (essentially, a slice of it), and the
  list {slice_indices} as a list of indices into
  {indices} (a slice of a slice), and returns the
  sub-list of {indices} indicated by the indices in
  {slice_indices}.",
  prose_application = "\hyperlink{relation-selectindicesbyslices}{selecting} indices from {indices} using slice indices {slice_indices} yields absolute slice {absolute_slice}",
} =
  case empty {
    slice_indices = empty_list;
    --
    empty_list;
  }

  case cons {
    slice_indices =: match_cons(idx, rest);
    v := indices[idx];
    select_indices_by_slices(indices, rest) -> rest_vals;
    --
    concat(make_singleton_list(v), rest_vals);
  }
;

typing function absolute_bitfields_align(f: TAbsField, g: TAbsField) ->
         (b: Bool)
{
  "tests whether the \absolutebitfields{} {f} and {g}
  share the same name and exist in the same scope. If
  they do, {b} indicates whether their \absoluteslices\
  are equal. Otherwise, the result is $\True$.",
  prose_application = "\hyperlink{relation-absolutebitfieldsalign}{checking} alignment between absolute bitfields {f} and {g} yields {b}",
} =
  f =: (f_names, slice_one);
  g =: (g_names, slice_two);
  f_names =: concat(scope_one, match_singleton_list(name_one));
  g_names =: concat(scope_two, match_singleton_list(name_two));
  same_scope := listprefix(scope_one, scope_two) || listprefix(scope_two, scope_one);
  b := implies((name_one = name_two && same_scope), slice_one = slice_two);
  --
  b;
;

typing function slice_to_indices(tenv: static_envs, s: slice) ->
         (indices: list0(Z))
{
  "returns the list of indices {indices} represented by
  the bitvector slice {s} in the
  \staticenvironmentterm{} {tenv}.",
  prose_application = "\hyperlink{relation-slicetoindices}{converting} slice {s} in {tenv} yields indices {indices}",
} =
  s =: Slice_Length(i, w);
  static_eval(tenv, i) -> L_Int(z_i) | ; // This evaluation always succeeds since i is a bound variable.
  static_eval(tenv, w) -> L_Int(z_w) | ; // This evaluation always succeeds since i is a bound variable.
  v_start := z_i;
  v_end := z_i + z_w - one;
  --
  range(v_end, v_start);
;

//////////////////////////////////////////////////
// Relations for Block Statements

typing relation annotate_block(tenv: static_envs, s: stmt) ->
         (new_stmt: stmt, ses: powerset(TSideEffect)) | type_error
{
  "annotates a block statement {s} in
  \staticenvironmentterm{} {tenv} and returns the
  annotated statement {new_stmt} and inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-annotateblock}{annotating} block statement {s} in {tenv} yields statement {new_stmt} and side effects {ses}",
} =
  annotate_stmt(tenv, s) -> (new_stmt, _, ses);
  --
  (new_stmt, ses);
;

semantics relation eval_block(env: envs, stm: stmt) -> Continuing(new_g: XGraphs, new_env: envs) | TReturning | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a statement {stm} as a \emph{block}. That
                        is, {stm} is evaluated in a fresh local environment,
                        which drops back to the original local environment of
                        {env} when the evaluation terminates.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
 } =
  case returning {
    env =: (tenv, denv);
    eval_stmt(env, stm) -> Returning((vs, new_g), env_ret) | DynErrorConfig(), DivergingConfig();
    env_ret =: (tenv1, denv1);
    pop_local_scope(denv, denv1) -> new_denv;
    new_env := (tenv, new_denv);
    --
    Returning((vs, new_g), new_env);
  }

  case continuing {
    env =: (tenv, denv);
    eval_stmt(env, stm) -> Continuing(new_g, env_cont) | DynErrorConfig(), DivergingConfig();
    env_cont =: (tenv1, denv1);
    pop_local_scope(denv, denv1) -> new_denv;
    new_env := (tenv, new_denv);
    --
    Continuing(new_g, new_env);
  }

  case throwing {
    env =: (tenv, denv);
    eval_stmt(env, stm) -> Throwing(v, t, new_g, env_throw) | DynErrorConfig(), DivergingConfig();
    env_throw =: (tenv1, denv1);
    pop_local_scope(denv, denv1) -> new_denv;
    new_env := (tenv, new_denv);
    --
    Throwing(v, t, new_g, new_env);
  }
;

semantics function pop_local_scope(outer_denv: dynamic_envs, inner_denv: dynamic_envs) -> (new_denv: dynamic_envs)
{
  "discards from {inner_denv} the bindings to local storage elements that are not in\\ {outer_denv}, yielding {new_denv}.",
  prose_application = "dropping from {inner_denv} the bindings to local storage elements that are not in {outer_denv}
  yields {new_denv}",
} =
  outer_ids := dom(outer_denv.dynamic_envs_L);
  --
  inner_denv(dynamic_envs_L: restrict_map(inner_denv.dynamic_envs_L, outer_ids))
  { [_] };
;

//////////////////////////////////////////////////
// Relations for Catching Exceptions

typing relation annotate_catcher(tenv: static_envs, ses_in: powerset(TSideEffect), c: catcher) ->
         (ses_in: powerset(TSideEffect), (new_catcher: catcher, ses: powerset(TSideEffect))) | type_error
{
  "annotates a catcher {c} in the
  \staticenvironmentterm{} {tenv} and
  \sideeffectsetterm{} {ses_in}. The result is the
  \sideeffectsetterm{} {ses_in}, the annotated catcher
  {new_catcher} and the \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-annotatecatcher}{annotating} catcher {c} in {tenv} with side effects {ses_in} yields catcher {new_catcher} and side effects {ses}",
  math_layout = [_,_],
} =
  case none {
    c =: (None, ty, stmt);
    annotate_type(False, tenv, ty) -> (ty', ses_ty);
    check_structure_label(tenv, ty', label_T_Exception) -> True;
    annotate_block(tenv, stmt) -> (new_stmt, ses_block);
    new_catcher := (None, ty', new_stmt);
    ses := union(ses_block, ses_ty);
    --
    (ses_in, (new_catcher, ses));
  }
  case some {
    c =: (some(name), ty, stmt);
    annotate_type(False, tenv, ty) -> (ty', ses_ty);
    check_structure_label(tenv, ty', label_T_Exception) -> True;
    check_var_not_in_env(tenv, name) -> True;
    add_local(tenv, name, ty', LDK_Let) -> tenv';
    annotate_block(tenv', stmt) -> (new_stmt, ses_block);
    new_catcher := (some(name), ty', new_stmt);
    ses := union(ses_block, ses_ty);
    --
    (ses_in, (new_catcher, ses));
  }
;

semantics relation eval_catchers(env: envs, catchers: list0(catcher), otherwise_opt: option(stmt), s_m: TOutConfig) ->
  TContinuing | TReturning | TThrowing | TDynError
{
   prose_description = "evaluates a list of \texttt{catch} clauses
                        {catchers}, an optional \texttt{otherwise} clause,
                        and a configuration {s_m} resulting from the
                        evaluation of the throwing expression, in the
                        environment {env}. The result is
                        either a continuation configuration, an early return
                        configuration, or an abnormal configuration.",
 prose_application = "",
  math_layout = [_,_],
 } =
  case catch {
    s_m =: Throwing(v, v_ty, sg, env_throw);
    env =: (tenv, denv);
    env_throw =: (tenv1, denv_throw);
    find_catcher(tenv, v_ty, catchers) -> some((None, _, s));
    eval_block(env_throw, s) -> C | DynErrorConfig(), DivergingConfig();
    new_g := ordered_po(sg, graph_of(C));
    --
    with_graph(C, new_g);
  }

  case catch_named {
    s_m =: Throwing(v, v_ty, sg, env_throw);
    env =: (tenv, denv);
    env_throw =: (tenv1, denv_throw);
    find_catcher(tenv, v_ty, catchers) -> some((some(name), _, s));
    read_value_from(v) -> (v1, g1);
    declare_local_identifier_m(env_throw, name, (v1, g1)) -> (env2, g2);
    eval_block(env2, s) -> C | DynErrorConfig(), DivergingConfig();
    env3 := environ_of(C);
    remove_local(env3, name) -> env4;
    D := with_environ(C, env4);
    new_g := ordered_po(sg, ordered_po(ordered_po(g1, g2), graph_of(D)));
    --
    with_graph(D, new_g);
  }

  case catch_otherwise {
    s_m =: Throwing(v, v_ty, s_g, env_throw);
    otherwise_opt =: some(s);
    env =: (tenv, denv);
    env_throw =: (_, denv_throw);
    find_catcher(tenv, v_ty, catchers) -> None;
    eval_block(env_throw, s) -> C | DynErrorConfig(), DivergingConfig();
    new_g := ordered_po(s_g, graph_of(C));
    --
    with_graph(C, new_g);
  }

  case catch_none {
    s_m =: Throwing(v, v_ty, sg, env_throw);
    otherwise_opt = None;
    env =: (tenv, denv);
    find_catcher(tenv, v_ty, catchers) -> None;
    --
    Throwing(v, v_ty, sg, env_throw)
    { [_] };
  }

  case catch_no_throw {
    case continuing {
      s_m =: Continuing(g, env1);
      --
      Continuing(g, env1);
    }

    case returning {
      s_m =: Returning(vs_g, env1);
      --
      Returning(vs_g, env1);
    }
  }
;

render rule eval_catchers_catch = eval_catchers(catch);
render rule eval_catchers_catch_named = eval_catchers(catch_named);
render rule eval_catchers_catch_otherwise = eval_catchers(catch_otherwise);
render rule eval_catchers_catch_none = eval_catchers(catch_none);
render rule eval_catchers_no_throw = eval_catchers(catch_no_throw);

semantics function find_catcher(tenv: static_envs, v_ty: ty, catchers: list0(catcher)) ->
  (catcher_opt: option(catcher))
{
  "returns the first catcher clause in {catchers} that matches the type {v_ty} in {catcher_opt}, if one exists.
  Otherwise, it returns $\None$",
  prose_application = "finding the first catcher clause in {catchers} that matches the type {v_ty} in the context of {tenv} yields {catcher_opt}",
} =
  case empty {
    catchers = empty_list;
    --
    None;
  }

  case match {
    catchers =: match_cons(c, catchers1);
    c =: (name_opt, e_ty, s);
    is_subtype(tenv, v_ty, e_ty) -> True;
    --
    some(c);
  }

  case no_match {
    catchers =: match_cons(c, catchers1);
    c =: (name_opt, e_ty, s);
    is_subtype(tenv, v_ty, e_ty) -> False;
    find_catcher(tenv, v_ty, catchers1) -> d;
    --
    d;
  }
;

//////////////////////////////////////////////////
// Relations for Global Pragmas

typing function check_global_pragma(genv: global_static_envs, d: decl) ->
         CheckResult | type_error
{
  "typechecks a global pragma declaration {d} in the
  \globalstaticenvironmentterm{} {genv}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkglobalpragma}{checking} global pragma declaration {d} in {genv} yields True",
} =
  d =: D_Pragma(_, args);
  with_empty_local(genv) -> tenv;
  annotate_exprs(tenv, args) -> args';
  --
  True;
;

//////////////////////////////////////////////////
// Relations for Global Storage Declarations

typing relation declare_global_storage(genv: global_static_envs, gsd: global_decl) ->
         (new_genv: global_static_envs, new_gsd: global_decl) | type_error
{
  "annotates the global storage declaration {gsd} in the
  \globalstaticenvironmentterm{} {genv}, yielding a
  modified \globalstaticenvironmentterm{} {new_genv} and
  annotated global storage declaration {new_gsd}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-declareglobalstorage}{declaring} global storage {gsd} in {genv} yields environment {new_genv} and declaration {new_gsd}",
  math_layout = [_,_],
} =
  gsd =: [keyword: keyword, global_decl_name: name, global_decl_ty: ty_opt, initial_value: initial_value];
  check_var_not_in_genv(genv, name) -> True;
  with_empty_local(genv) -> tenv;
  must_be_pure := keyword in make_set(GDK_Config, GDK_Constant);
  annotate_ty_opt_initial_value(tenv, keyword, must_be_pure, ty_opt, initial_value) -> (typed_initial_value, ty_opt', declared_t)
  { [[_], [_]] };
  add_global_storage(genv, name, keyword, declared_t) -> genv1;
  with_empty_local(genv1) -> tenv1;
  typed_initial_value =: (initial_value', t_init, ses_initial_value);
  update_global_storage(tenv1, name, keyword, (t_init, initial_value', ses_initial_value)) -> tenv2
  { [[_], _] };
  new_gsd := [keyword: keyword, global_decl_name: name, global_decl_ty: ty_opt', initial_value: some(initial_value')];
  new_genv := tenv2.static_envs_G;
  --
  (new_genv, new_gsd);
;

typing relation annotate_ty_opt_initial_value(
    tenv: static_envs,
    gdk: global_decl_keyword,
    must_be_pure: Bool,
    ty_opt': option(ty),
    initial_value: option(expr)) ->
         (typed_initial_value: (expr, ty, powerset(TSideEffect)), ty_opt': option(ty), declared_t: ty) | type_error
{
  "is used in the context of a declaration of a global
  storage element with optional type annotation
  {ty_opt'} and optional initializing expression
  {initial_value}, in the \staticenvironmentterm{}
  {tenv}. It determines {typed_initial_value}, which
  consists of an expression, a type, and a
  \sideeffectsetterm, the annotation of the type in
  {ty_opt'} (in case there is a type), and the type that
  should be associated with the storage element
  {declared_t}.",
  prose_application = "\hyperlink{relation-annotatetyoptinitialvalue}{annotating} type {ty_opt'} and initializer {initial_value} in {tenv} yields value {typed_initial_value} and type {declared_t}",
  math_layout = [input[_,_,_,_,_], ([_,_,_],_)],
} =
  case some_some_config {
    gdk = GDK_Config;
    ty_opt' =: some(t);
    initial_value =: some(e);
    annotate_expr(tenv, e) -> (t_e, e', ses_e);
    annotate_type(False, tenv, t) -> (t', ses_t);
    typed_e := (e', t_e, ses_e);
    check_type_satisfies(tenv, t_e, t') -> True;
    te_check(not_single(must_be_pure) || ses_is_pure(union(ses_t, ses_e)), TE_SEV) -> True;
    --
    (typed_e, some(t'), t')
    { ([_], [_]) };
  }

  case some_some {
    gdk != GDK_Config;
    ty_opt' =: some(t);
    initial_value =: some(e);
    annotate_expr(tenv, e) -> (t_e, e', ses_e);
    get_structure(tenv, t_e) -> t_e';
    inherit_integer_constraints(t, t_e') -> t'';
    annotate_type(False, tenv, t'') -> (t', ses_t);
    typed_e := (e', t_e, ses_e);
    check_type_satisfies(tenv, t_e, t') -> True;
    te_check(not_single(must_be_pure) || ses_is_pure(union(ses_t, ses_e)), TE_SEV) -> True;
    --
    (typed_e, some(t'), t')
    { ([_], [_]) };
  }

  case some_none {
    ty_opt' =: some(t);
    initial_value = None;
    annotate_type(False, tenv, t) -> (t', ses_t);
    te_check(not_single(must_be_pure) || ses_is_pure(ses_t), TE_SEV) -> True;
    base_value(tenv, t') -> e';
    typed_initial_value := (e', t', empty_set);
    --
    (typed_initial_value, some(t'), t')
    { [[_], [_]] };
  }

  case none_some {
    ty_opt' = None;
    initial_value =: some(e);
    annotate_expr(tenv, e) -> (t_e, e', ses_e);
    check_no_precision_loss(t_e) -> True;
    typed_e := (e', t_e, ses_e);
    te_check(not_single(must_be_pure) || ses_is_pure(ses_e), TE_SEV) -> True;
    --
    (typed_e, None, t_e)
    { ([_], [_]) };
  }
;

typing relation update_global_storage(
    tenv: static_envs,
    name: Identifier,
    gdk: global_decl_keyword,
    typed_initial_value: (ty, expr, powerset(TSideEffect))) ->
         (new_tenv: static_envs)
{
  "updates the \staticenvironmentterm{} {tenv} for the
  global storage element named {name} with global
  declaration keyword {gdk}, and a tuple (obtained via
  \\ \TypingRuleRef{AnnotateTyOptInitialValue})
  {typed_initial_value}, which consists a type for the
  initializing value, the annotated initializing
  expression, and the inferred \sideeffectsetterm{} for
  the initializing value. The result is the updated
  \staticenvironmentterm{} {new_tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-updateglobalstorage}{updating} global storage {name} with keyword {gdk} and value {typed_initial_value} in {tenv} yields {new_tenv}",
  math_layout = (input[_,_,_,_], _),
} =
  typed_initial_value =: (initial_value_type, initial_value', ses_initial_value)
  { (_, [_]) };
  case update_constant {
    gdk = GDK_Constant;
    static_eval(tenv, initial_value') -> v;
    add_global_constant(tenv.static_envs_G, name, v) -> G';
    --
    tenv(static_envs_G : G');
  }

  case let_normalizable {
    gdk = GDK_Let;
    normalize_opt(tenv, initial_value') -> some(e');
    add_global_immutable_expr(tenv, name, e') -> new_tenv;
    --
    new_tenv;
  }

  case let_non_normalizable {
    gdk = GDK_Let;
    normalize_opt(tenv, initial_value') -> None;
    --
    tenv;
  }

  case config {
    gdk = GDK_Config;
    is_singular(tenv, initial_value_type) -> True;
    --
    tenv;
  }

  case var {
    gdk = GDK_Var;
    --
    tenv;
  }
;

typing function add_global_storage(
    genv: global_static_envs,
    name: Identifier,
    keyword: global_decl_keyword,
    declared_t: ty) ->
         (new_genv: global_static_envs) | type_error
{
  "returns a \globalstaticenvironmentterm{} {new_genv}
  which is identical to the
  \globalstaticenvironmentterm{} {genv}, except that the
  identifier {name}, which is assumed to name a global
  storage element, is bound to the global storage
  keyword {keyword} and type {declared_t}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-addglobalstorage}{adding} global storage {name} with keyword {keyword} and type {declared_t} to {genv} yields {new_genv}",
  math_layout = [_,_],
} =
  check_var_not_in_genv(genv, name) -> True;
  updated_map := map_update(genv.global_storage_types, name, (declared_t, keyword));
  --
  genv(global_storage_types : updated_map);
;

semantics relation eval_globals(decls: list0(decl), envm: (envs, XGraphs)) -> (C: (envs, XGraphs)) | TThrowing | TDynError | TDiverging
{
   prose_description = "updates the input environment and execution graph by
                        initializing the global storage declarations.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
} =
  case empty {
    decls = empty_list;
    --
    envm;
  }

  case non_empty {
    decls =: match_cons(d, decls');
    d =: D_GlobalStorage(gd);
    gd =: [
      keyword: _,
      global_decl_name: name,
      global_decl_ty: _,
      initial_value: some(e)
    ];
    envm =: (env, g1);
    eval_expr(env, e) -> ResultExpr((v, g2), env2);
    declare_global(name, v, env2) -> env3;
    g := ordered_po(g1, g2);
    eval_globals(decls', (env3, g)) -> (new_env, new_g);
    --
    (new_env, new_g);
  }
;

semantics function declare_global(name: Identifier, v: native_value, env: envs) -> (new_env: envs)
{
   prose_description = "updates the environment {env} by mapping {name} to
                        {v} in the $\storage$ map of the global dynamic
                        environment $\denv.\dynamicenvsG$.",
 prose_application = "",
} =
  env =: (tenv, denv);
  updated_storage := map_update(denv.dynamic_envs_G.storage, name, v);
  new_gdenv := denv.dynamic_envs_G(storage: updated_storage);
  new_denv := denv(dynamic_envs_G: new_gdenv);
  --
  (tenv, new_denv);
;

//////////////////////////////////////////////////
// Relations for Local Storage Declarations

typing relation annotate_local_decl_item(
  tenv: static_envs,
  ty: ty,
  ldk: local_decl_keyword,
  e_opt: option((expr, powerset(TSideEffect))),
  ldi: local_decl_item
) -> (new_tenv: static_envs) | type_error
{
  "annotates the \localdeclarationitem{} {ldi} and \localdeclarationkeyword{} {ldk},
  given a type {ty}, and {e_opt} --- an optional initializing expression and \sideeffectsetterm{},
  in the context of the \staticenvironmentterm{} {tenv} --- yielding the updated \staticenvironmentterm{}
  {new_tenv}. \ProseOtherwiseTypeError",
  prose_application = "annotating the local storage declaration with {ldi} and {ldk} with
  {ty} and optional initializing expression and \sideeffectsetterm{} {e_opt} yields {new_tenv}\OrTypeError",
  math_layout = [[_,_,_,_,_], _],
} =
  case var {
    ldi =: LDI_Var(x);
    check_var_not_in_env(tenv, x) -> True;
    check_no_precision_loss(ty) -> True;
    add_local(tenv, x, ty, ldk) -> tenv2;
    add_immutable_expression(tenv2, ldk, e_opt, x) -> new_tenv;
    --
    new_tenv;
  }

  case tuple {
    ldi =: LDI_Tuple(ids);
    make_anonymous(tenv, ty) -> t';
    te_check(ast_label(t') = label_T_Tuple, TE_UT) -> True;
    t' =: T_Tuple(tys);
    te_check(same_length(ids, tys), TE_UT) -> True;
    add_local_vars(tenv, ldk, list_combine(ids, tys)) -> new_tenv;
    --
    new_tenv;
  }
;

typing function add_local_vars(
  tenv: static_envs,
  ldk: local_decl_keyword,
  typed_ids: list0((Identifier, ty))
 ) ->
  (new_tenv: static_envs) | type_error
{
  "updates {tenv} by binding the variables in {typed_ids} to their corresponding types,
   in right-to-left order, yielding {new_tenv}. \ProseOtherwiseTypeError",
   prose_application = "",
} =
  case empty {
    typed_ids = empty_list;
    --
    tenv;
  }

  case non_empty {
    typed_ids =: concat(typed_ids_prefix, match_singleton_list((id, t)));
    check_var_not_in_env(tenv, id) -> True;
    add_local(tenv, id, t, ldk) -> tenv1;
    add_local_vars(tenv1, ldk, typed_ids_prefix) -> new_tenv;
    --
    new_tenv;
  }
;

semantics relation eval_local_decl(env: envs, ldi: local_decl_item, m: (native_value, XGraphs)) ->
  ResultLDI(new_g: XGraphs, new_env: envs)
{
  "evaluates a \localdeclarationitem{} {ldi} in an environment {env} with an initialization value {m},
  yielding the \executiongraphterm{} {new_g} and new environment {new_env}.",
  prose_application = "evaluating the \localdeclarationitem{} {ldi} in {env} with the initializing
  value {m} yields {new_g} and {new_env}.",
} =
  case LDI_Var {
    ldi =: LDI_Var(name);
    (v, g1) := m;
    declare_local_identifier(env, name, v) -> (new_env, g2);
    new_g := ordered_data(g1, g2);
    --
    ResultLDI(new_g, new_env);
  }

  case LDI_Tuple {
    ldi =: LDI_Tuple(ids);
    (v, g) := m;
    INDEX(i, ids: get_index(i, v) -> values[i]);
    liv := list_map(i, indices(ids), (values[i], g));
    declare_ldi_tuple(env, ids, liv) -> ResultLDI(new_g, new_env);
    --
    ResultLDI(new_g, new_env);
  }
;

render rule eval_local_decl_LDI_Var = eval_local_decl(LDI_Var);
render rule eval_local_decl_LDI_Tuple = eval_local_decl(LDI_Tuple);

semantics relation declare_ldi_tuple(env: envs, ids: list0(Identifier), liv: list0((native_value, XGraphs))) ->
  ResultLDI(g: XGraphs, new_env: envs)
{
  "declares in {env} the local storage elements whose names are given by {ids} and initialization values and \executiongraphterm{} by {liv}.
  The lists {ids} and {liv} are assumed to have equal lengths.
  The result is the updated environment {new_env} and resulting \executiongraphterm{} {g}.",
  prose_application = "declaring the local storage elements whose identifiers are given by {ids} and initializers are
  given by {liv} yields the updated environment {new_env} and \executiongraphterm{} {g}.",
} =
  case empty {
    ids = empty_list;
    liv = empty_list;
    --
    ResultLDI(empty_graph, env);
  }

  case non_empty {
    ids =: match_cons(id, ids');
    liv =: match_cons((v, g1), liv');
    declare_local_identifier(env, id, v) -> (env1, g2);
    declare_ldi_tuple(env1, ids', liv') -> ResultLDI(g3, new_env);
    g := parallel(ordered_data(g1, g2), g3);
    --
    ResultLDI(g, new_env);
  }
;

typing function check_is_not_collection(tenv: static_envs, t: ty) ->
         CheckResult | type_error
{
  "checks whether the type {t} has the structure of a
  \collectiontypeterm{}, and if so, raises a
  \typingerrorterm{}. Otherwise, the result is $\True$.",
  prose_application = "\hyperlink{relation-checkisnotcollection}{verifying} type {t} in {tenv} is not a collection type yields True",
} =
  make_anonymous(tenv, t) -> t_struct;
  case collection {
    ast_label(t_struct) = label_T_Collection;
    --
    TypeError(TE_UT);
  }

  case not_collection {
    ast_label(t_struct) not_in make_set(label_T_Collection, label_T_Tuple);
    --
    True;
  }

  case tuple {
    t_struct =: T_Tuple(tys);
    INDEX(i, tys: check_is_not_collection(tenv, tys[i]) -> True);
    --
    True;
  }
;

//////////////////////////////////////////////////
// Relations for Pattern Matching

typing relation annotate_pattern(tenv: static_envs, t: ty, p: pattern) ->
         (new_p: pattern, ses: powerset(TSideEffect)) | type_error
{
  "annotates a pattern {p} in a \staticenvironmentterm{}
  {tenv} given a type {t}, resulting in {new_p}, which
  is the typed AST node for {p} and the inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError.",
  prose_application = "",
} =
  case all {
    p = Pattern_All;
    --
    (Pattern_All, empty_set);
  }

  case single {
    p =: Pattern_Single(e);
    annotate_expr(tenv, e) -> (t_e, e', ses);
    check_symbolically_evaluable(ses) -> True;
    make_anonymous(tenv, t) -> t_struct;
    make_anonymous(tenv, t_e) -> t_e_struct;

    case simple {
      ast_label(t_struct) in make_set(label_T_Bool, label_T_Int, label_T_Real, label_T_String);
      te_check(ast_label(t_struct) = ast_label(t_e_struct), TE_BO) -> True;
      --
      (Pattern_Single(e'), ses);
    }

    case bits {
      ast_label(t_struct) = label_T_Bits;
      te_check(ast_label(t_struct) = ast_label(t_e_struct), TE_BO) -> True;
      check_bits_equal_width(tenv, t_struct, t_e_struct) -> b;
      te_check(b, TE_BO) -> True;
      --
      (Pattern_Single(e'), ses);
    }

    case enum_type {
      ast_label(t_struct) = label_T_Enum;
      te_check(ast_label(t_struct) = ast_label(t_e_struct), TE_BO) -> True;
      t_struct =: T_Enum(li1);
      t_e_struct =: T_Enum(li2);
      te_check(li1 = li2, TE_BO) -> True;
      --
      (Pattern_Single(e'), ses);
    }

    case error {
      te_check(ast_label(t_struct) = ast_label(t_e_struct), TE_BO) -> True;
      ast_label(t_struct) not_in make_set(label_T_Bits, label_T_Bool, label_T_Enum, label_T_Int, label_T_Real);
      --
      TypeError(TE_UT);
    }
  }

  case range {
    p =: Pattern_Range(e1, e2);
    annotate_symbolically_evaluable_expr(tenv, e1) -> (t_e1, e1', ses1);
    annotate_symbolically_evaluable_expr(tenv, e2) -> (t_e2, e2', ses2);
    ses := union(ses1, ses2);
    make_anonymous(tenv, t) -> t_struct;
    make_anonymous(tenv, t_e1) -> t_e1_struct;
    make_anonymous(tenv, t_e2) -> t_e2_struct;
    b := and(
      ast_label(t_struct) = ast_label(t_e1_struct),
      (ast_label(t_e1_struct) = ast_label(t_e2_struct)),
      (ast_label(t_struct) in make_set(label_T_Int, label_T_Real))
    )
    { (_, [_]) };
    te_check(b, TE_BO) -> True;
    --
    (Pattern_Range(e1', e2'), ses);
  }

  case leq {
    p =: Pattern_Leq(e);
    annotate_expr(tenv, e) -> (t_e, e', ses);
    check_symbolically_evaluable(ses) -> True;
    make_anonymous(tenv, t) -> t_struct;
    make_anonymous(tenv, t_e) -> t_e_struct;
    b := ( (ast_label(t_struct) = ast_label(t_e_struct)) && (ast_label(t_struct) in make_set(label_T_Int, label_T_Real)) )
    { (_, ([_])) };
    te_check(b, TE_BO) -> True;
    --
    (Pattern_Leq(e'), ses);
  }

  case geq {
    p =: Pattern_Geq(e);
    annotate_expr(tenv, e) -> (t_e, e', ses);
    check_symbolically_evaluable(ses) -> True;
    make_anonymous(tenv, t) -> t_struct;
    make_anonymous(tenv, t_e) -> t_e_struct;
    b := ( (ast_label(t_struct) = ast_label(t_e_struct)) && (ast_label(t_struct) in make_set(label_T_Int, label_T_Real)) )
    { (_, ([_])) };
    te_check(b, TE_BO) -> True;
    --
    (Pattern_Geq(e'), ses);
  }

  case mask {
    p =: Pattern_Mask(m);
    check_structure_label(tenv, t, label_T_Bits) -> True;
    n := ELint(list_len(m));
    check_type_satisfies(tenv, t, T_Bits(n, empty_list)) -> True;
    --
    (Pattern_Mask(m), empty_set);
  }

  case tuple {
    p =: Pattern_Tuple(li);
    get_structure(tenv, t) -> t_struct;
    te_check(ast_label(t_struct) = label_T_Tuple, TE_UT) -> True;
    t_struct =: T_Tuple(ts);
    te_check(same_length(li, ts), TE_UT) -> True;
    INDEX(i, li: annotate_pattern(tenv, ts[i], li[i]) -> (li'[i], xs[i]));
    new_li := li';
    ses := union_list(xs);
    --
    (Pattern_Tuple(new_li), ses);
  }

  case any {
    p =: Pattern_Any(li);
    INDEX(i, li: annotate_pattern(tenv, t, li[i]) -> (new_l[i], xs[i]));
    new_li := new_l;
    ses := union_list(xs);
    --
    (Pattern_Any(new_li), ses);
  }

  case neg {
    p =: Pattern_Not(q);
    annotate_pattern(tenv, t, q) -> (new_q, ses);
    --
    (Pattern_Not(new_q), ses);
  }
;

render rule annotate_pattern_all = annotate_pattern(all);
render rule annotate_pattern_single = annotate_pattern(single);
render rule annotate_pattern_range = annotate_pattern(range);
render rule annotate_pattern_leq = annotate_pattern(leq);
render rule annotate_pattern_geq = annotate_pattern(geq);
render rule annotate_pattern_mask = annotate_pattern(mask);
render rule annotate_pattern_tuple = annotate_pattern(tuple);
render rule annotate_pattern_any = annotate_pattern(any);
render rule annotate_pattern_neg = annotate_pattern(neg);

semantics relation eval_pattern(env: envs, v: native_value, p: pattern) -> ResultPattern(b: tbool, new_g: XGraphs) | TDynError | TDiverging
{
   prose_description = "determines whether a value {v} matches the pattern
                        {p} in an environment {env} resulting in either
                        $\ResultPattern(\vb, \newg)$ or an abnormal
                        configuration.",
 prose_application = "",
} =
  case PAll {
    p = Pattern_All;
    --
    ResultPattern(nvbool(True), empty_graph);
  }

  case PSingle {
    p =: Pattern_Single(e);
    eval_expr_sef(env, e) -> ResultExprSEF(v1', g');
    eval_binop(EQ, v, v1') -> b;
    --
    ResultPattern(native_value_as_tbool(b), g');
  }

  case PRange {
    p =: Pattern_Range(e1, e2);
    eval_expr_sef(env, e1) -> ResultExprSEF(v1, g1);
    eval_binop(GE, v, v1) -> b1;
    eval_expr_sef(env, e2) -> ResultExprSEF(v2, g2);
    eval_binop(LE, v, v2) -> b2;
    eval_binop(BAND, b1, b2) -> b;
    g := parallel(g1, g2);
    --
    ResultPattern(native_value_as_tbool(b), g);
  }

  case PLeq {
    p =: Pattern_Leq(e);
    eval_expr_sef(env, e) -> ResultExprSEF(v1, new_g);
    eval_binop(LE, v, v1) -> b;
    --
    ResultPattern(native_value_as_tbool(b), new_g);
  }

  case PGeq {
    p =: Pattern_Geq(e);
    eval_expr_sef(env, e) -> ResultExprSEF(v1, new_g);
    eval_binop(GE, v, v1) -> b;
    --
    ResultPattern(native_value_as_tbool(b), new_g);
  }

  case PMask {
    case empty {
      p = Pattern_Mask(empty_list);
      v =: nvbitvector(empty_list);
      --
      ResultPattern(nvbool(True), empty_graph);
    }

    case non_empty {
      p =: Pattern_Mask(mask);
      v =: nvbitvector(bits);
      b := list_and(list_map(i, indices(mask), mask_match(mask[i], bits[i])));
      --
      ResultPattern(nvbool(b), empty_graph);
    }
  }

  case PTuple {
    p =: Pattern_Tuple(ps);
    INDEX(i, ps: get_index(i, v) -> values[i]);
    INDEX(i, ps: eval_pattern(env, values[i], ps[i]) -> ResultPattern(nvbool(bs[i]), gs[i]))
    { [_] };
    b := list_and(bs);
    g := parallel_graphs(gs);
    --
    ResultPattern(nvbool(b), g);
  }

  case PAny {
    p =: Pattern_Any(ps);
    INDEX(i, ps: eval_pattern(env, v, ps[i]) -> ResultPattern(nvbool(bs[i]), gs[i]))
    { [_] };
    b := list_or(bs);
    g := parallel_graphs(gs);
    --
    ResultPattern(nvbool(b), g);
  }

  case PNot {
    p =: Pattern_Not(p1);
    eval_pattern(env, v, p1) -> ResultPattern(b', new_g);
    eval_unop(BNOT, b') -> b;
    --
    ResultPattern(native_value_as_tbool(b), new_g);
  }
;

render rule eval_pattern_PAll = eval_pattern(PAll);
render rule eval_pattern_PSingle = eval_pattern(PSingle);
render rule eval_pattern_PRange = eval_pattern(PRange);
render rule eval_pattern_PLeq = eval_pattern(PLeq);
render rule eval_pattern_PGeq = eval_pattern(PGeq);
render rule eval_pattern_PMask = eval_pattern(PMask);
render rule eval_pattern_PTuple = eval_pattern(PTuple);
render rule eval_pattern_PAny = eval_pattern(PAny);
render rule eval_pattern_PNot = eval_pattern(PNot);

semantics function mask_match(mv: constants_set(zero_bit, one_bit, x_bit), b: Bit) -> (res: Bool)
{
  "tests whether the bit {b} matches the mask value {mv}, yielding the result in {res}.",
  prose_application = "testing whether the bit {b} matches the mask value {mv} yields {res}",
} =
  --
  bot; // This rule is defined by a LaTeX table.
;

//////////////////////////////////////////////////
// Relations for Primitive Operations

constant unop_signatures =
  make_set(
    (BNOT,  label_L_Bool),
    (NEG,   label_L_Int),
    (NEG,   label_L_Real),
    (NOT,   label_L_Bitvector))
  { math_layout = [_] }
;

constant binop_arith_signatures =
  make_set(
    (ADD,    label_L_Int,       label_L_Int),
    (ADD,    label_L_Real,      label_L_Real),
    (DIV,    label_L_Int,       label_L_Int),
    (DIVRM,  label_L_Int,       label_L_Int),
    (EQ,     label_L_Label,     label_L_Label),
    (EQ,     label_L_String,    label_L_String),
    (MOD,    label_L_Int,       label_L_Int),
    (MUL,    label_L_Int,       label_L_Int),
    (MUL,    label_L_Int,       label_L_Real),
    (MUL,    label_L_Real,      label_L_Int),
    (MUL,    label_L_Real,      label_L_Real),
    (NE,     label_L_Label,     label_L_Label),
    (NE,     label_L_String,    label_L_String),
    (POW,    label_L_Int,       label_L_Int),
    (POW,    label_L_Real,      label_L_Int),
    (RDIV,   label_L_Real,      label_L_Real),
    (SHL,    label_L_Int,       label_L_Int),
    (SHR,    label_L_Int,       label_L_Int),
    (SUB,    label_L_Int,       label_L_Int),
    (SUB,    label_L_Real,      label_L_Real))
  { math_layout = [_] }
;

constant binop_rel_signatures =
  make_set(
    (EQ,     label_L_Bitvector, label_L_Bitvector),
    (EQ,     label_L_Bool,      label_L_Bool),
    (EQ,     label_L_Int,       label_L_Int),
    (EQ,     label_L_Real,      label_L_Real),
    (GE,     label_L_Int,       label_L_Int),
    (GE,     label_L_Real,      label_L_Real),
    (GT,     label_L_Int,       label_L_Int),
    (GT,     label_L_Real,      label_L_Real),
    (LE,     label_L_Int,       label_L_Int),
    (LE,     label_L_Real,      label_L_Real),
    (LT,     label_L_Int,       label_L_Int),
    (LT,     label_L_Real,      label_L_Real),
    (NE,     label_L_Bitvector, label_L_Bitvector),
    (NE,     label_L_Bool,      label_L_Bool),
    (NE,     label_L_Int,       label_L_Int),
    (NE,     label_L_Real,      label_L_Real))
  { math_layout = [_] }
;

constant binop_bool_signatures =
  make_set(
    (BAND,   label_L_Bool,      label_L_Bool),
    (BEQ,    label_L_Bool,      label_L_Bool),
    (BOR,    label_L_Bool,      label_L_Bool),
    (IMPL,   label_L_Bool,      label_L_Bool))
  { math_layout = [_] }
;

constant binop_bits_signatures =
  make_set(
    (ADD,    label_L_Bitvector, label_L_Bitvector),
    (ADD,    label_L_Bitvector, label_L_Int),
    (AND,    label_L_Bitvector, label_L_Bitvector),
    (BV_CONCAT, label_L_Bitvector, label_L_Bitvector),
    (OR,     label_L_Bitvector, label_L_Bitvector),
    (SUB,    label_L_Bitvector, label_L_Bitvector),
    (SUB,    label_L_Bitvector, label_L_Int),
    (XOR,    label_L_Bitvector, label_L_Bitvector))
  { math_layout = [_] }
;

constant binop_str_signatures =
  make_set(
    (STR_CONCAT, label_L_Bitvector,   label_L_Bitvector),
    (STR_CONCAT, label_L_Bitvector,   label_L_Bool),
    (STR_CONCAT, label_L_Bitvector,   label_L_Int),
    (STR_CONCAT, label_L_Bitvector,   label_L_Label),
    (STR_CONCAT, label_L_Bitvector,   label_L_Real),
    (STR_CONCAT, label_L_Bitvector,   label_L_String),
    (STR_CONCAT, label_L_Bool,   label_L_Bitvector),
    (STR_CONCAT, label_L_Bool,   label_L_Bool),
    (STR_CONCAT, label_L_Bool,   label_L_Int),
    (STR_CONCAT, label_L_Bool,   label_L_Label),
    (STR_CONCAT, label_L_Bool,   label_L_Real),
    (STR_CONCAT, label_L_Bool,   label_L_String),
    (STR_CONCAT, label_L_Int,    label_L_Bitvector),
    (STR_CONCAT, label_L_Int,    label_L_Bool),
    (STR_CONCAT, label_L_Int,    label_L_Int),
    (STR_CONCAT, label_L_Int,    label_L_Label),
    (STR_CONCAT, label_L_Int,    label_L_Real),
    (STR_CONCAT, label_L_Int,    label_L_String),
    (STR_CONCAT, label_L_Label,   label_L_Bitvector),
    (STR_CONCAT, label_L_Label,   label_L_Bool),
    (STR_CONCAT, label_L_Label,   label_L_Int),
    (STR_CONCAT, label_L_Label,   label_L_Label),
    (STR_CONCAT, label_L_Label,   label_L_Real),
    (STR_CONCAT, label_L_Label,   label_L_String),
    (STR_CONCAT, label_L_Real,   label_L_Bitvector),
    (STR_CONCAT, label_L_Real,   label_L_Bool),
    (STR_CONCAT, label_L_Real,   label_L_Int),
    (STR_CONCAT, label_L_Real,   label_L_Label),
    (STR_CONCAT, label_L_Real,   label_L_Real),
    (STR_CONCAT, label_L_Real,   label_L_String),
    (STR_CONCAT, label_L_String, label_L_Bitvector),
    (STR_CONCAT, label_L_String, label_L_Bool),
    (STR_CONCAT, label_L_String, label_L_Int),
    (STR_CONCAT, label_L_String, label_L_Label),
    (STR_CONCAT, label_L_String, label_L_Real),
    (STR_CONCAT, label_L_String, label_L_String))
  { math_layout = [_] }
;

constant binop_signatures =
  union(
    binop_arith_signatures,
    binop_rel_signatures,
    binop_bool_signatures,
    binop_bits_signatures,
    binop_str_signatures
  )
  { math_layout = [_] }
;

typing function unop_literals(op: unop, l: literal) ->
         (r: literal) | type_error
{
  "statically evaluates a unary operator {op} (a terminal
  derived from the AST non-terminal for unary operators)
  over a literal {l} and returns the resulting literal
  {r}. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case Error {
    (op, ast_label(l)) not_in unop_signatures;
    --
    TypeError(TE_BO);
  }

  case negate_int {
    op = NEG;
    l =: L_Int(n);
    --
    L_Int(negate(n));
  }

  case negate_real {
    op = NEG;
    l =: L_Real(n);
    --
    L_Real(negate(n));
  }

  case not_bool {
    op = BNOT;
    l =: L_Bool(b);
    --
    L_Bool(not_single(b));
  }

  case not_bits {
    l =: L_Bitvector(bits);
    op = NOT;
    c := list_map(b, bits, negate_bit(b));
    --
    L_Bitvector(c);
  }
;

typing function binop_literals(op: binop, v1: literal, v2: literal) ->
         (r: literal) | type_error
{
  "statically evaluates a binary operator {op} (a
  terminal derived from the AST non-terminal for binary
  operators) over a pair of literals {v1} and {v2} and
  returns the resulting literal {r}. The result is a
  \typingerrorterm{}, if it is illegal to apply the
  operator to the given values, or a different kind of
  \typingerrorterm{} is detected.",
  prose_application = "",
} =
  case error {
    (op, ast_label(v1), ast_label(v2)) not_in binop_signatures;
    --
    TypeError(TE_BO);
  }

  // Arithmetic Operators Over Integer Values
  case arithmetic {
    v1 =: L_Int(a);
    v2 =: L_Int(b);
    case add_int {
      op = ADD;
      --
      L_Int(a + b);
    }

    case sub_int {
      op = SUB;
      --
      L_Int(a - b);
    }

    case mul_int {
      op = MUL;
      --
      L_Int(a * b);
    }

    case div_int {
      op = DIV;
      te_check(b > zero, TE_BO) -> True;
      n := round_down(fraction(a, b));
      te_check(a = n * b, TE_BO) -> True;
      --
      L_Int(n);
    }

    case fdiv_int {
      op = DIVRM;
      te_check(b > zero, TE_BO) -> True;
      n := if a >= zero then round_down(as_rational(a) / as_rational(b)) else num_negate(round_up((num_negate(as_rational(a))) / as_rational(b)));
      --
      L_Int(n);
    }

    case frem_int {
      op = MOD;
      binop_literals(DIVRM, L_Int(a), L_Int(b)) -> L_Int(c);
      --
      L_Int(a - (c * b));
    }

    case exp_int {
      op = POW;
      te_check(b >= zero, TE_BO) -> True;
      --
      L_Int(a ^ b);
    }

    case shl {
      op = SHL;
      te_check(b >= zero, TE_BO) -> True;
      binop_literals(POW, L_Int(two), L_Int(b)) -> L_Int(e);
      binop_literals(MUL, L_Int(a), L_Int(e)) -> r;
      --
      r;
    }

    case shr {
      op = SHR;
      te_check(b >= zero, TE_BO) -> True;
      binop_literals(POW, L_Int(two), L_Int(b)) -> L_Int(e);
      binop_literals(DIVRM, L_Int(a), L_Int(e)) -> r;
      --
      r;
    }
  }

  // Comparison Operators Over Integer Values
  case comparison_int {
    v1 =: L_Int(a);
    v2 =: L_Int(b);
    case eq_int {
      op = EQ;
      --
      L_Bool(a = b);
    }

    case ne_int {
      op = NE;
      --
      L_Bool(not_equal(a, b));
    }

    case le_int {
      op = LE;
      --
      L_Bool(a <= b);
    }

    case lt_int {
      op = LT;
      --
      L_Bool(a < b);
    }

    case ge_int {
      op = GE;
      --
      L_Bool(a >= b);
    }

    case gt_int {
      op = GT;
      --
      L_Bool(a > b);
    }
  }

  // Boolean Operators Over Boolean Values
  case boolean {
    v1 =: L_Bool(a);
    v2 =: L_Bool(b);
    case and_bool {
      op = BAND;
      --
      L_Bool(a && b);
    }

    case or_bool {
      op = BOR;
      --
      L_Bool(a || b);
    }

    case implies_bool {
      op = IMPL;
      --
      L_Bool(not_single(a) || b);
    }

    case eq_bool {
      op in make_set(BEQ, EQ);
      --
      L_Bool(a = b);
    }

    case ne_bool {
      op = NE;
      --
      L_Bool(not_equal(a, b));
    }
  }

  // Arithmetic Operators Over Real Values
  case arithmetic_real {
    case mul_int_real {
      op = MUL;
      v1 =: L_Int(a);
      v2 =: L_Real(b);
      --
      L_Real(as_rational(a) * b);
    }

    case mul_real_int {
      op = MUL;
      v1 =: L_Real(a);
      v2 =: L_Int(b);
      --
      L_Real(a * as_rational(b));
    }

    case add_real {
      op = ADD;
      v1 =: L_Real(a);
      v2 =: L_Real(b);
      --
      L_Real(a + b);
    }

    case sub_real {
      op = SUB;
      v1 =: L_Real(a);
      v2 =: L_Real(b);
      --
      L_Real(a - b);
    }

    case mul_real {
      op = MUL;
      v1 =: L_Real(a);
      v2 =: L_Real(b);
      --
      L_Real(a * b);
    }

    case div_real {
      op = RDIV;
      v1 =: L_Real(a);
      v2 =: L_Real(b);
      te_check(not_equal(b, rational_zero), TE_BO) -> True;
      --
      L_Real(a / b);
    }

    case exp_real {
      op = POW;
      v1 =: L_Real(a);
      v2 =: L_Int(b);
      te_check(not_equal(a, rational_zero) || b >= zero, TE_BO) -> True;
      --
      L_Real(a ^ as_rational(b));
    }
  }

  // Comparison Operators Over Real Values
  case comparison_real {
    v1 =: L_Real(a);
    v2 =: L_Real(b);
    case eq_real {
      op = EQ;
      --
      L_Bool(equal(a, b));
    }

    case ne_real {
      op = NE;
      --
      L_Bool(not_equal(a, b));
    }

    case le_real {
      op = LE;
      --
      L_Bool(a <= b);
    }

    case lt_real {
      op = LT;
      --
      L_Bool(a < b);
    }

    case ge_real {
      op = GE;
      --
      L_Bool(a >= b);
    }

    case gt_real {
      op = GT;
      --
      L_Bool(a > b);
    }
  }

  // Operators Over Bitvectors
  case bitvector {
    v1 =: L_Bitvector(a);
    v2 =: L_Bitvector(b);
    case bitwise_different_bitwidths {
      list_len(a) != list_len(b);
      --
      TypeError(TE_BO);
    }

    case bitwise_empty {
      op in make_set(ADD, AND, OR, SUB, XOR);
      a = empty_list;
      b = empty_list;
      --
      L_Bitvector(empty_list);
    }

    case eq_bits {
      op = EQ;
      --
      L_Bool(a = b);
    }

    case ne_bits {
      op = NE;
      binop_literals(EQ, L_Bitvector(a), L_Bitvector(b)) -> L_Bool(result);
      --
      L_Bool(not_single(result));
    }

    case or_bits {
      op = OR;
      same_length(a, b);
      c := list_map(i, indices(a), or_bit(a[i], b[i]));
      --
      L_Bitvector(c);
    }

    case and_bits {
      op = AND;
      same_length(a, b);
      c := list_map(i, indices(a), and_bit(a[i], b[i]));
      --
      L_Bitvector(c);
    }

    case xor_bits {
      op = XOR;
      same_length(a, b);
      c := list_map(i, indices(a), if a[i] = b[i] then zero_bit else one_bit);
      --
      L_Bitvector(c);
    }

    case add_bits {
      op = ADD;
      same_length(a, b);
      binary_to_unsigned(a) -> a_val;
      binary_to_unsigned(b) -> b_val;
      int_to_bits(a_val + b_val, list_len(a)) -> c;
      --
      L_Bitvector(c);
    }

    case sub_bits {
      op = SUB;
      same_length(a, b);
      binary_to_unsigned(a) -> a_val;
      binary_to_unsigned(b) -> b_val;
      int_to_bits(a_val - b_val, list_len(a)) -> c;
      --
      L_Bitvector(c);
    }

    case concat_bits {
      op = BV_CONCAT;
      --
      L_Bitvector(concat(a, b));
    }
  }

  case bits_int {
    v1 =: L_Bitvector(a);
    v2 =: L_Int(b);
    case add_bits_int {
      op = ADD;
      binary_to_unsigned(a) -> a_val;
      int_to_bits(a_val + b, list_len(a)) -> c;
      --
      L_Bitvector(c);
    }

    case sub_bits_int {
      op = SUB;
      binary_to_unsigned(a) -> a_val;
      int_to_bits(a_val - b, list_len(a)) -> c;
      --
      L_Bitvector(c);
    }
  }

  // Operators Over String Values
  case string {
    v1 =: L_String(a);
    v2 =: L_String(b);
    case eq_string {
      op = EQ;
      --
      L_Bool(a = b);
    }

    case ne_string {
      op = NE;
      --
      L_Bool(not_equal(a, b));
    }
  }

  case concat_strings {
    op = STR_CONCAT;
    ast_label(v1) != label_L_Bitvector || ast_label(v1) != label_L_Bitvector;
    literal_to_string(v1) -> s1;
    literal_to_string(v2) -> s2;
    --
    L_String(concat(s1, s2));
  }

  // Operators Over Label Values
  case label {
    v1 =: L_Label(a);
    v2 =: L_Label(b);
    case eq_label {
      op = EQ;
      --
      L_Bool(a = b);
    }

    case ne_label {
      op = NE;
      --
      L_Bool(not_equal(a, b));
    }
  }
;

render rule binop_literals_error = binop_literals(error);
render rule binop_literals_arithmetic = binop_literals(arithmetic);
render rule binop_literals_comparison_int = binop_literals(comparison_int);
render rule binop_literals_boolean = binop_literals(boolean);
render rule binop_literals_arithmetic_real = binop_literals(arithmetic_real);
render rule binop_literals_comparison_real = binop_literals(comparison_real);
render rule binop_literals_bitvector = binop_literals(bitvector);
render rule binop_literals_bits_int = binop_literals(bits_int);
render rule binop_literals_string = binop_literals(string);
render rule binop_literals_concat_strings = binop_literals(concat_strings);
render rule binop_literals_label = binop_literals(label);

typing function binary_to_unsigned(bits: list0(Bit)) -> (num: N)
{
  "converts the bit sequence {bits} into the natural number {num} or $0$ if {bits} is empty.",
  prose_application = "converting the bit sequence {bits} into a natural number yields {num}",
} =
  case empty {
    bits = empty_list;
    --
    zero;
  }

  case non_empty {
    bits =: match_cons(b, lower_bits);
    binary_to_unsigned(lower_bits) -> tail_value;
    n := list_len(lower_bits);
    bit_value := if b = one_bit then two ^ n else zero;
    --
    tail_value + bit_value;
  }
;

typing function int_to_bits(val: Z, width: Z) -> (bits: list0(Bit))
{
  "converts the integer {val} to its two's complement representation with {width} bits, yielding the result in {bits}.",
  prose_application = "converting the integer {val} to its two's complement representation of {width} bits yields {bits}",
} =
  --
  bot; // This is a well-defined mathematical opeation. No rule needed.
;

semantics function eval_unop(op: unop, v: native_value) ->
         (w: native_value) | TDynError
{
  "evaluates a unary operator {op} over a
  \nativevalueterm{} {v} and returns the
  \nativevalueterm{} {w} or an error.",
  prose_application = "",
} =
  case ok {
    v =: NV_Literal(l);
    unop_literals(op, l) -> lit | ;
    --
    NV_Literal(lit);
  }

  case static_error {
    v =: NV_Literal(l);
    unop_literals(op, l) -> TypeError(_) | ;
    --
    DynamicError(DE_BO);
  }

  case non_literal {
    not(v = NV_Vector(_));
    --
    DynamicError(DE_BO);
  }
;

semantics function eval_binop(op: binop, v1: native_value, v2: native_value) ->
         (w: native_value) | TDynError
{
  "evaluates a binary operator {op} over a pair of
  \nativevaluesterm{}  {v1} and {v2} and returns
  the \nativevalueterm{}  {w} or an error.",
  prose_application = "",
} =
  case ok {
    v1 =: NV_Literal(l1);
    v2 =: NV_Literal(l2);
    binop_literals(op, l1, l2) -> lit | ;
    --
    NV_Literal(lit);
  }

  case static_error {
    v1 =: NV_Literal(l1);
    v2 =: NV_Literal(l2);
    binop_literals(op, l1, l2) -> TypeError(_) | ;
    --
    DynamicError(DE_BO);
  }

  // I'm not sure this case is possible, even during static evaluation.
  case non_literal {
    not(v1 = NV_Literal(_)) || not(v2 = NV_Literal(_));
    --
    DynamicError(DE_BO);
  }
;

//////////////////////////////////////////////////
// Relations for Relations On Types

typing function is_subtype(tenv: static_envs, t1: ty, t2: ty) -> (b: Bool)
{
    "defines whether the type {t1} \subtypesterm{} the type {t2} in the \staticenvironmentterm{} {tenv},
    yielding the result in {b}.",
    prose_application = "testing whether {t1} \subtypesterm{} {t2} in {tenv} yields {b}",
} =
  case reflexive {
    t1 =: T_Named(id1);
    t2 =: T_Named(id2);
    id1 = id2;
    --
    True;
  }

  case transitive {
    t1 =: T_Named(id1);
    t2 =: T_Named(id2);
    id1 != id2;
    tenv.static_envs_G.subtypes(id1) =: id3;
    is_subtype(tenv, T_Named(id3), t2) -> b;
    --
    b;
  }

  case no_supertype {
    t1 =: T_Named(id1);
    t2 =: T_Named(id2);
    id1 != id2;
    tenv.static_envs_G.subtypes(id1) = bot;
    --
    False;
  }

  case not_named {
    (ast_label(t1) != label_T_Named) || (ast_label(t2) != label_T_Named);
    --
    False;
  }
;

typing function subtype_satisfies(tenv: static_envs, t: ty, s: ty) -> (b: Bool) | type_error
{
    "determines whether a type {t} \emph{\subtypesatisfiesterm} a type {s} in the static environment {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {t} \subtypesatisfiesterm{} {s} in {tenv} yields {b}\ProseOrTypeError",
} =
  case different_labels {
    make_anonymous(tenv, t) -> t2;
    make_anonymous(tenv, s) -> s2;
    ast_label(t2) != ast_label(s2);
    --
    False;
  }

  case simple {
    make_anonymous(tenv, t) -> t2;
    make_anonymous(tenv, s) -> s2;
    ast_label(t2) in make_set(label_T_Bool, label_T_Real, label_T_String);
    --
    ast_label(s2) = ast_label(t2);
  }

  case t_int {
    make_anonymous(tenv, t) -> t2;
    make_anonymous(tenv, s) -> s2;
    ast_label(t2) = label_T_Int;
    ast_label(s2) = label_T_Int;
    symdom_of_type(tenv, s) -> d_s;
    symdom_of_type(tenv, t) -> d_t;
    symdom_subset_unions(tenv, d_s, d_t) -> b;
    --
    b;
  }

  case t_enum {
    make_anonymous(tenv, t) -> T_Enum(li_t);
    make_anonymous(tenv, s) -> T_Enum(li_s);
    --
    li_t = li_s;
  }

  case t_bits {
    make_anonymous(tenv, s) -> T_Bits(w_s, bfs_s);
    make_anonymous(tenv, t) -> T_Bits(w_t, bfs_t);
    bitfields_included(tenv, bfs_s, bfs_t) -> True;
    symdom_of_width_expr(w_s) -> d_s;
    symdom_of_width_expr(w_t) -> d_t;
    symdom_subset_unions(tenv, d_s, d_t) -> b;
    --
    b;
  }

  case t_array_expr {
    make_anonymous(tenv, s) -> T_Array(length_s, ty_s);
    make_anonymous(tenv, t) -> T_Array(length_t, ty_t);
    type_equal(tenv, ty_s, ty_t) -> True | False;
    bool_transition(ast_label(length_s) = ast_label(length_t)) -> True | False;
    length_s =: ArrayLength_Expr(length_expr_s);
    length_t =: ArrayLength_Expr(length_expr_t);
    expr_equal(tenv, length_expr_s, length_expr_t) -> b;
    --
    b;
  }

  case t_array_enum {
    make_anonymous(tenv, s) -> T_Array(length_s, ty_s);
    make_anonymous(tenv, t) -> T_Array(length_t, ty_t);
    type_equal(tenv, ty_s, ty_t) -> True;
    bool_transition(ast_label(length_s) = ast_label(length_t)) -> True | False;
    length_s =: ArrayLength_Enum(name_s, _);
    length_t =: ArrayLength_Enum(name_t, _);
    --
    name_s = name_t;
  }

  case t_tuple {
    make_anonymous(tenv, s) -> T_Tuple(li_s);
    make_anonymous(tenv, t) -> T_Tuple(li_t);
    bool_transition(same_length(li_s, li_t)) -> True | False;
    ( INDEX(i, li_s: type_satisfies(tenv, li_t[i], li_s[i]) -> component_type_satisfies[i]) )
    { ([_, [_]]) };
    --
    list_and(component_type_satisfies);
  }

  case structured {
    make_anonymous(tenv, s) -> s_anon;
    make_anonymous(tenv, t) -> t_anon;
    s_anon =: make_structured(L_s, fields_s);
    t_anon =: make_structured(L_t, fields_t);
    L_s = L_t;
    L_s in make_set(label_T_Collection, label_T_Exception, label_T_Record);
    fields_s =: list_combine(names_s, field_types_s);
    fields_t =: list_combine(names_t, _);
    bool_transition(subseteq(list_to_set(names_s), list_to_set(names_t))) -> True | False;
    // Notice that the premise above guarantees that the indexed premises in the next judgment
    // always return `some`.
    // TODO: add the comment above to a special prose-related attribute.
    INDEX(i, names_s: field_type(fields_t, names_s[i]) -> some(field_types_t[i]))
    { [_] };
    ( INDEX(i, field_types_s: type_equal(tenv, field_types_s[i], field_types_t[i]) -> field_tys_equal[i]) )
    { ([_, [_]]) };
    --
    list_and(field_tys_equal);
  }
;

// The rule for this function isn't used. Instead a direct latex definition is used.
typing function field_type(fields: list0((name: Identifier, type: ty)), id: Identifier) -> (ty_opt: option(ty))
{
  "returns the type associated with {id} in {fields}, if there exists a unique one, and $\None$, otherwise.",
  prose_application = "finding the unique type associated with {id} in {fields} yields {ty_opt}",
} =
  case empty {
    fields = empty_list;
    --
    None;
  }

  case found_unique {
    fields =: match_cons((field_id, t), rest);
    field_id = id;
    field_type(rest, id) -> None;
    --
    some(t);
  }

  case found_duplicate {
    fields =: match_cons((field_id, _), rest);
    field_id = id;
    field_type(rest, id) -> some(_);
    --
    None;
  }

  case not_found {
    fields =: match_cons((field_id, _), rest);
    field_id != id;
    field_type(rest, id) -> result;
    --
    result;
  }
;

typing function type_satisfies(tenv: static_envs, t: ty, s: ty) -> (b: Bool) | type_error
{
    "determines whether a type {t} \emph{\typesatisfiesterm} a type {s} in the static environment {tenv},
    yielding the result {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {t} \typesatisfiesterm{} {s} in {tenv} yields {b}\ProseOrTypeError",
} =
  case subtypes {
    is_subtype(tenv, t, s) -> True;
    --
    True;
  }

  case not_subtypes {
    is_subtype(tenv, t, s) -> False;

    case anonymous_or_subtype_satisfies {
      is_anonymous(t) || is_anonymous(s);
      subtype_satisfies(tenv, t, s) -> True;
      --
      True;
    }

    case other {
      subtype_satisfies(tenv, t, s) -> t_subtype_satisfies_s;
      not((is_anonymous(t) || is_anonymous(s)) && t_subtype_satisfies_s);
      get_structure(tenv, s) -> s_struct;
      case t_bits {
        t =: T_Bits(width_t, empty_list);
        s_struct =: T_Bits(width_s, _);
        bitwidth_equal(tenv, width_t, width_s) -> b;
        --
        b;
      }

      case not_tbits {
        or(
          ast_label(t) != label_T_Bits,
          ast_label(s_struct) != label_T_Bits,
          (t =: T_Bits(_, t_fields)) && t_fields = empty_list
        );
        --
        False;
      }
    }
  }
;

typing function check_type_satisfies(tenv: static_envs, t: ty, s: ty) -> CheckResult | type_error
{
  "returns $\True$ if {t} \typesatisfiesterm{} a type {s} in the static environment {tenv}. \ProseOtherwiseTypeError",
  prose_application = "checking whether {t} \typesatisfiesterm{} {s} in {tenv} yields $\True$\ProseOrTypeError",
  math_macro = \checktypesatisfies,
} =
  case okay {
    type_satisfies(tenv, t, s) -> True;
    --
    True;
  }

  case error {
    type_satisfies(tenv, t, s) -> False;
    --
    TypeError(TE_TSF);
  }
;

typing relation lowest_common_ancestor(tenv: static_envs, t: ty, s: ty) -> (ty: ty) | type_error
{
  "returns the \Proselca{} of types {t} and {s} in the \staticenvironmentterm{} {tenv}, yielding {ty}.
  If a \Proselca{} does not exist or a \typingerrorterm{} is detected, the result is a \typingerrorterm{}.",
  prose_application = "the \Proselca{} of {t} and {s} in {tenv} is {ty}\ProseOrTypeError",
  math_macro = \lca,
} =
  case type_equal {
    type_equal(tenv, t, s) -> True;
    --
    s;
  }

  case named_subtype1 {
    t =: T_Named(name_s);
    s =: T_Named(name_t);
    type_equal(tenv, t, s) -> False;
    named_lowest_common_ancestor(tenv, name_s, name_t) -> None;
    make_anonymous(tenv, s) -> s_anon;
    make_anonymous(tenv, t) -> t_anon;
    lowest_common_ancestor(tenv, t_anon, s_anon) -> ty;
    --
    ty;
  }

  case named_subtype2 {
    t =: T_Named(name_s);
    s =: T_Named(name_t);
    type_equal(tenv, t, s) -> False;
    named_lowest_common_ancestor(tenv, name_s, name_t) -> some(name);
    --
    T_Named(name);
  }

  case one_named1 {
    type_equal(tenv, t, s) -> False;
    (ast_label(t) = label_T_Named) || (ast_label(s) = label_T_Named);
    ast_label(t) != ast_label(s);
    make_anonymous(tenv, s) -> s_anon;
    make_anonymous(tenv, t) -> t_anon;
    type_equal(tenv, t_anon, s_anon) -> True;
    ty := if (ast_label(t) = label_T_Named) then t else s;
    --
    ty;
  }

  case one_named2 {
    type_equal(tenv, t, s) -> False;
    (ast_label(t) = label_T_Named) || (ast_label(s) = label_T_Named);
    ast_label(t) != ast_label(s);
    make_anonymous(tenv, s) -> s_anon;
    make_anonymous(tenv, t) -> t_anon;
    type_equal(tenv, t_anon, s_anon) -> False;
    lowest_common_ancestor(tenv, t_anon, s_anon) -> ty;
    --
    ty;
  }

  case t_int_unconstrained {
    type_equal(tenv, t, s) -> False;
    ast_label(t) = label_T_Int && ast_label(s) = label_T_Int;
    t = unconstrained_integer || s = unconstrained_integer;
    --
    unconstrained_integer;
  }

  case t_int_parameterized {
    type_equal(tenv, t, s) -> False;
    ast_label(t) = label_T_Int && ast_label(s) = label_T_Int;
    t != unconstrained_integer;
    s != unconstrained_integer;
    is_parameterized_integer(t) || is_parameterized_integer(s);
    to_well_constrained(t) -> t1;
    to_well_constrained(s) -> s1;
    lowest_common_ancestor(tenv, t1, s1) -> ty;
    --
    ty;
  }

  case t_int_wellconstrained {
    type_equal(tenv, t, s) -> False;
    t =: T_Int(typed_WellConstrained(cs_t, p1));
    s =: T_Int(typed_WellConstrained(cs_s, p2));
    p := precision_join(p1, p2);
    ty := T_Int(typed_WellConstrained(match_non_empty_list(concat(cs_t, cs_s)), p));
    --
    ty;
  }

  case t_bits {
    t =: T_Bits(e_t, _);
    s =: T_Bits(e_s, _);
    type_equal(tenv, t, s) -> False;
    expr_equal(tenv, e_t, e_s) -> b_equal;
    te_check(b_equal, TE_LCA) -> True;
    --
    T_Bits(e_t, empty_list);
  }

  case t_array {
    t =: T_Array(width_t, ty_t);
    s =: T_Array(width_s, ty_s);
    type_equal(tenv, t, s) -> False;
    array_length_equal(tenv, width_t, width_s) -> b_equal_length;
    te_check(b_equal_length, TE_LCA) -> True;
    lowest_common_ancestor(tenv, ty_t, ty_s) -> ty1;
    --
    T_Array(width_t, ty1);
  }

  case t_tuple {
    t =: T_Tuple(li_t);
    s =: T_Tuple(li_s);
    type_equal(tenv, t, s) -> False;
    b := same_length(li_t, li_s);
    te_check(b, TE_LCA) -> True;
    INDEX(i, li_t: lowest_common_ancestor(tenv, li_t[i], li_s[i]) -> li[i]);
    --
    T_Tuple(li);
  }

  case error {
    type_equal(tenv, t, s) -> False;
    (ast_label(t) != ast_label(s)) ||
    (ast_label(t) in make_set(label_T_Collection, label_T_Enum, label_T_Exception, label_T_Record))
    { (lhs, ((_, [_])) ) };
    --
    TypeError(TE_LCA);
  }
;

typing relation apply_unop_type(tenv: static_envs, op: unop, t: ty) ->
         (s: ty) | type_error
{
  "determines the result type of applying a unary
  operator when the type of its operand is known.
  Similarly, we determine the negation of integer
  constraints. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case bnot_t_bool {
    op = BNOT;
    check_type_satisfies(tenv, t, T_Bool) -> True;
    --
    T_Bool;
  }

  case neg_error {
    op = NEG;
    type_satisfies(tenv, t, unconstrained_integer) -> False;
    type_satisfies(tenv, t, T_Real) -> False;
    --
    TypeError(TE_BO);
  }

  case neg_t_real {
    op = NEG;
    type_satisfies(tenv, t, T_Real) -> True;
    --
    T_Real;
  }

  case neg_t_int_unconstrained {
    op = NEG;
    get_well_constrained_structure(tenv, t) -> unconstrained_integer;
    --
    unconstrained_integer;
  }

  case neg_t_int_well_constrained {
    op = NEG;
    get_well_constrained_structure(tenv, t) -> T_Int(typed_WellConstrained(cs, p))
    { [_] };
    neg_cs := list_map(c, cs, negate_constraint(c));
    --
    T_Int(typed_WellConstrained(match_non_empty_list(neg_cs), p));
  }

  case not_t_bits {
    op = NOT;
    check_structure_label(tenv, t, label_T_Bits) -> True;
    --
    t;
  }
;

typing function negate_constraint(c: int_constraint) ->
         (new_c: int_constraint)
{
  "takes an integer constraint {c} and returns the
  constraint {new_c}, which corresponds to the negation
  of all the values that {c} represents.",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    --
    Constraint_Exact(E_Unop(NEG, e));
  }

  case range {
    c =: Constraint_Range(v_start, v_end);
    --
    Constraint_Range(E_Unop(NEG, v_end), E_Unop(NEG, v_start));
  }
;

typing relation apply_binop_types(tenv: static_envs, op: binop, t1: ty, t2: ty) ->
         (t: ty) | type_error
{
  "determines the result type {t} of applying the binary
  operator {op} to operands of type {t1} and {t2} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case named {
    (ast_label(t1) = label_T_Named) || (ast_label(t2) = label_T_Named);
    make_anonymous(tenv, t1) -> t1_anon;
    make_anonymous(tenv, t2) -> t2_anon;
    apply_binop_types(tenv, op, t1_anon, t2_anon) -> t;
    --
    t;
  }

  case boolean {
    op in make_set(BAND, BOR, EQ, IMPL);
    t1 = T_Bool;
    t2 = T_Bool;
    --
    T_Bool;
  }

  case bits_arith {
    op in make_set(ADD, AND, OR, SUB, XOR);
    t1 =: T_Bits(w1, _);
    t2 =: T_Bits(w2, _);
    check_bits_equal_width(tenv, t1, t2) -> True;
    --
    T_Bits(w1, empty_list);
  }

  case bits_int {
    op in make_set(ADD, SUB);
    t1 =: T_Bits(w, _);
    t2 = T_Int(_);
    --
    T_Bits(w, empty_list);
  }

  case bits_concat {
    t1 =: T_Bits(w1, _);
    t2 =: T_Bits(w2, _);
    op = BV_CONCAT;
    w := E_Binop(ADD, w1, w2);
    normalize(tenv, w) -> w';
    --
    T_Bits(w', empty_list);
  }

  case string_concat {
    op = STR_CONCAT;
    (ast_label(t1) != label_T_Bits) || (ast_label(t2) != label_T_Bits);
    is_singular(tenv, t1) -> t1_singular;
    is_singular(tenv, t2) -> t2_singular;
    te_check(t1_singular, TE_UT) -> True;
    te_check(t2_singular, TE_UT) -> True;
    --
    T_String;
  }

  case rel {
    (op, ast_label(t1), ast_label(t2)) in
    make_set(
      (EQ, label_T_Bool, label_T_Bool),
      (EQ, label_T_Int, label_T_Int),
      (EQ, label_T_Real, label_T_Real),
      (EQ, label_T_String, label_T_String),
      (GE, label_T_Int, label_T_Int),
      (GE, label_T_Real, label_T_Real),
      (GT, label_T_Int, label_T_Int),
      (GT, label_T_Real, label_T_Real),
      (LE, label_T_Int, label_T_Int),
      (LE, label_T_Real, label_T_Real),
      (LT, label_T_Int, label_T_Int),
      (LT, label_T_Real, label_T_Real),
      (NE, label_T_Bool, label_T_Bool),
      (NE, label_T_Int, label_T_Int),
      (NE, label_T_Real, label_T_Real),
      (NE, label_T_String, label_T_String)) { (_, [_]) };
    --
    T_Bool;
  }

  case eq_neq_bits {
    op in make_set(EQ, NE);
    t1 =: T_Bits(w1, _);
    t2 =: T_Bits(w2, _);
    check_bits_equal_width(tenv, t1, t2) -> True;
    --
    T_Bool;
  }

  case eq_neq_enum {
    op in make_set(EQ, NE);
    t1 =: T_Enum(li1);
    t2 =: T_Enum(li2);
    te_check(li1 = li2, TE_BO) -> True;
    --
    T_Bool;
  }

  case arith_t_int_unconstrained {
    t1 =: T_Int(c1);
    t2 =: T_Int(c2);
    op in make_set(ADD, DIV, DIVRM, MOD, MUL, POW, SHL, SHR, SUB);
    (c1 = Unconstrained) || (c2 = Unconstrained);
    --
    unconstrained_integer;
  }

  case arith_t_int_parameterized {
    op in make_set(ADD, DIV, DIVRM, MOD, MUL, POW, SHL, SHR, SUB);
    t1 =: T_Int(c1);
    t2 =: T_Int(c2);
    (ast_label(c1) = label_Parameterized) || (ast_label(c2) = label_Parameterized);
    (ast_label(c1) != label_Unconstrained) && (ast_label(c2) != label_Unconstrained);
    to_well_constrained(t1) -> t1_wellconstrained;
    to_well_constrained(t2) -> t2_wellconstrained;
    apply_binop_types(tenv, op, t1_wellconstrained, t2_wellconstrained) -> t;
    --
    t;
  }

  case arith_t_int_wellconstrained {
    t1 =: T_Int(c1);
    t2 =: T_Int(c2);
    op in make_set(ADD, DIV, DIVRM, MOD, MUL, POW, SHL, SHR, SUB);
    c1 =: typed_WellConstrained(cs1, p1);
    c2 =: typed_WellConstrained(cs2, p2);
    // This is somewhat hard to model, as this call is implemented by a functor
    // that fails rather than return CannotOverapproximate/CannotUnderapproximate.
    annotate_constraint_binop(Over, tenv, op, cs1, cs2) -> (cs, p3);
    p := precision_join(p1, precision_join(p2, p3));
    --
    T_Int(typed_WellConstrained(match_non_empty_list(cs), p));
  }

  case arith_real {
    (op, ast_label(t1), ast_label(t2)) in
    make_set(
      (ADD, label_T_Real, label_T_Real),
      (MUL, label_T_Real, label_T_Real),
      (POW, label_T_Real, label_T_Int),
      (RDIV, label_T_Real, label_T_Real),
      (SUB, label_T_Real, label_T_Real)) { (_, [_]) };
    --
    T_Real;
  }

  case error {
    make_anonymous(tenv, t1) -> t1_anon;
    make_anonymous(tenv, t2) -> t2_anon;
    (op, ast_label(t1), ast_label(t2)) not_in binop_signatures;
    --
    TypeError(TE_BO);
  }
;

// REVIEW: signature changed for translation; please confirm expected argument/return types.
// Previous: typing relation named_lowest_common_ancestor(tenv: static_envs, t: ty, s: ty) -> (name_opt: option(Identifier)) | type_error
typing function named_lowest_common_ancestor(tenv: static_envs, t: Identifier, s: Identifier) ->
         (name_opt: option(Identifier)) | type_error
{
  "returns the lowest common named super type
   of types {t} and {s} in {tenv} in {name_opt}.",
  prose_application = "",
} =
  case found {
    supers(tenv, t) -> t_supers;
    s in t_supers;
    --
    some(s);
  }

  case super {
    supers(tenv, t) -> t_supers;
    s not_in t_supers;
    tenv.static_envs_G.subtypes(s) =: s';
    named_lowest_common_ancestor(tenv, t, s') -> name_opt;
    --
    name_opt;
  }

  case none {
    supers(tenv, t) -> t_supers;
    s not_in t_supers;
    tenv.static_envs_G.subtypes(s) = bot;
    --
    None;
  }
;

typing function supers(tenv: static_envs, t: Identifier) ->
         (powerset(Identifier))
{
  "returns the set of \emph{named supertypes} of a type
  {t} in the $\subtypes$ function of a
  \globalstaticenvironmentterm{} {tenv}.",
  prose_application = "",
} =
    --
    bot; // Using direct definition in LaTeX.
;

constant max_constraint_size : N { math_macro = \maxconstraintsize };
constant max_exploded_interal_size : N { math_macro = \maxexplodedintervalsize };

// In the implementation, this function is inside the StaticApprox functor,
// and we model the approximation direction via the extra `approx` argument.
typing relation annotate_constraint_binop(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    op: binop,
    cs1: list0(int_constraint),
    cs2: list0(int_constraint)) ->
         | (annotated_cs: list0(int_constraint), p: precision_loss_indicator)
         | type_error
{
  "annotates the application of the binary operation {op}
  to the lists of integer constraints {cs1} and {cs2},
  yielding a list of constraints {annotated_cs}.
  If the list is empty, the result is either
  $\CannotUnderapproximate$ or $\CannotOverapproximate$,
  based on {approx} (this function is invoked in the
  context of approximating lists of constraints).
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [[_,_,_,_,_],_],
} =
  case exploding {
    binop_filter_rhs(approx, tenv, op, cs2) -> cs2f;
    binop_is_exploding(op) -> TRUE;
    explode_intervals(tenv, cs1) -> (cs1e, p1);
    explode_intervals(tenv, cs2f) -> (cs2e, p2);
    p0 := precision_join(p1, p2);
    expected_constraint_length :=
      if op = MOD
      then list_len(cs2e)
      else list_len(cs1e) * list_len(cs2e);
    (cs1_arg, cs2_arg, p) :=
      if expected_constraint_length < max_constraint_size
      then (cs1e, cs2e, p0)
      else (cs1, cs2f, Precision_Lost)
    { (_, [_]) };
    constraint_binop(op, cs1_arg, cs2_arg) -> cs_vanilla;
    refine_constraint_for_div(approx, op, cs_vanilla) -> refined_cs;
    reduce_constraints(tenv, refined_cs) -> annotated_cs;
    --
    (annotated_cs, p);
  }

  case non_exploding {
    binop_filter_rhs(approx, tenv, op, cs2) -> cs2f;
    binop_is_exploding(op) -> FALSE;
    p := Precision_Full;
    cs1_arg := cs1;
    cs2_arg := cs2f;
    constraint_binop(op, cs1_arg, cs2_arg) -> cs_vanilla;
    refine_constraint_for_div(approx, op, cs_vanilla) -> refined_cs;
    reduce_constraints(tenv, refined_cs) -> annotated_cs;
    --
    (annotated_cs, p);
  }
;

typing relation binop_filter_rhs(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    op: binop,
    cs: list0(int_constraint)) ->
         (new_cs: list1(int_constraint)) | ApproximationFailure
{
  "filters the list of constraints {cs} by removing
  values that will definitely result in a dynamic error
  if found on the right-hand-side of a binary operation
  expression with the operator {op} in any environment
  consisting of the \staticenvironmentterm{} {tenv}. The
  result is the filtered list of constraints {new_cs}.
  If the list is empty, the result is either
  $\CannotUnderapproximate$ or $\CannotOverapproximate$,
  based on {approx} (this function is invoked in the
  context of approximating lists of constraints).",
  prose_application = "",
  math_layout = [_,_],
};

typing relation refine_constraint_by_sign(tenv: static_envs, p: fun Z -> Bool, c: int_constraint) ->
         (c_opt: option(int_constraint))
{
  "takes a predicate {p} that returns $\True$ based on
  the sign of its input. The function conservatively
  refines the constraint {c} in {tenv} by applying
  symbolic reasoning to yield a new constraint (inside
  an optional) that represents the values that satisfy
  the {c} and for which {p} holds. In this context,
  conservatively means that the new constraint may
  represent a superset of the values that a more precise
  reasoning may yield. If the set of those values is
  empty the result is $\None$.",
  prose_application = "",
};

typing function reduce_to_z_opt(tenv: static_envs, e: expr) ->
         (z_opt: option(Z))
{
  "returns an integer inside an optional if {e} can be
  symbolically simplified into an integer in {tenv} and
  $\None$ otherwise. The expression {e} is assumed not
  to yield a \typingerrorterm{} when applying
  $\normalize$ to it.",
  prose_application = "",
} =
  normalize(tenv, e) -> e_normalized;
  z_opt := if e_normalized =: ELint(z) then some(z) else None
  { (_, [_]) };
  --
  z_opt;
;

typing relation refine_constraints(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    f: fun int_constraint -> option(int_constraint),
    cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint)) | ApproximationFailure
{
  "refines a list of constraints {cs} by applying the
  refinement function {f} to each constraint and
  retaining the constraints that do not refine to
  $\None$. The resulting list of constraints is given in
  {new_cs}. If the list is empty, the result is either
  $\CannotUnderapproximate$ or $\CannotOverapproximate$,
  based on {approx} (this function is invoked in the
  context of approximating lists of constraints).",
  prose_application = "",
  math_layout = [input[_,_,_,_], _],
};

typing relation refine_constraint_for_div(approx: constants_set(Over,Under), op: binop, cs: list0(int_constraint)) ->
         (res: list0(int_constraint)) | ApproximationFailure
{
  "filters the list of constraints {cs} for {op},
  removing constraints that represents a division
  operation that will definitely fail when {op} is the
  division operation. If the list is empty, the result
  is either $\CannotUnderapproximate$ or
  $\CannotOverapproximate$, based on {approx} (this
  function is invoked in the context of approximating
  lists of constraints).",
  prose_application = "",
  math_layout = [_,_],
} =
  case div {
    op = DIV;
    INDEX(i, cs: filter_reduce_constraint_div(cs[i]) -> c_opts[i]);
    res := filter_option_list(c_opts);

    case non_empty {
      res != empty_list;
      --
      res;
    }

    case div_empty {
      res = empty_list;
      --
      if approx = Over then CannotOverapproximate else CannotUnderapproximate
      { (_, [_]) };
    }
  }

  case non_div {
    op != DIV;
    --
    cs;
  }
;

typing relation filter_reduce_constraint_div(c: int_constraint) ->
         (c_opt: option(int_constraint))
{
  "returns $\None$ if {c} is an exact constraint for a
  \binopexpressionterm{} for dividing two integer
  literals where the denominator does not divide the
  numerator, and an optional containing {c} otherwise.
  The result is returned in {c_opt}. This is used to
  conservatively test whether {c} would always fail
  dynamically.",
  prose_application = "",
} =
  case exact_literal {
    c =: Constraint_Exact(e);
    get_literal_div_opt(e) -> some((z1, z2)) | None;
    c_opt := if z2 > zero && is_not_integer(fraction(z1, z2)) then None else some(c);
    --
    c_opt;
  }

  case exact_not_literal {
    c =: Constraint_Exact(e);
    get_literal_div_opt(e) -> None;
    --
    None;
  }

  case range {
    c =: Constraint_Range(e1, e2);
    get_literal_div_opt(e1) -> e1_opt;
    get_literal_div_opt(e2) -> e2_opt;
    z1_opt := if (e1_opt =: some((z1, z2))) && z2 > zero then some(round_up(fraction(z1, z2))) else None
    { (_, [_]) };
    z2_opt := if (e2_opt =: some((z3, z4))) && z4 > zero then some(round_down(fraction(z3, z4))) else None
    { (_, [_]) };
    // Build result constraint based on z1_opt and z2_opt
    c_opt :=
    cond(
      (z1_opt =: some(z5a)) && (z2_opt =: some(z6a)) : make_constraint_range_opt(z5a, z6a),
      (z1_opt =: some(z5d)) && (z2_opt = None) : some(AbbrevConstraintRange(ELint(z5d), e2)),
      (z1_opt = None) && (z2_opt =: some(z6e)) : some(AbbrevConstraintRange(e1, ELint(z6e))),
      (z1_opt = None) && (z2_opt = None) : some(c)
    )
    { (_, [c1[_, [_]], c2[_, [_]], c3[_, [_]],_]) };
    --
    c_opt;
  }
;

typing function get_literal_div_opt(e: expr) ->
         (range_opt: option((Z, Z)))
{
  "matches the expression {e} to a binary operation
  expression over the division operation and two literal
  integer expressions. If {e} matches this pattern, the
  result {range_opt} is an optional containing the pair
  of integers appearing in the operand expressions.
  Otherwise, the result is $\None$.",
  prose_application = "",
} =
  range_opt := if e =: AbbrevEBinop(DIV, ELint(z1), ELint(z2)) then some((z1, z2)) else None
  { (_, [_]) };
  --
  range_opt;
;

typing function make_constraint_range_opt(z1: Z, z2: Z) -> option(int_constraint)
{
  "constructs a constraint for the set of numbers greater or equal to {z1} and less or equal to {z2}",
  prose_application = "",
} =
  case exact {
    z1 = z2;
    --
    some(AbbrevConstraintExact(ELint(z1)));
  }

  case range {
    z1 < z2;
    --
    some(AbbrevConstraintRange(ELint(z1), ELint(z2)));
  }

  case invalid {
    z1 > z2;
    --
    None;
  }
;

typing function explode_intervals(tenv: static_envs, cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint), p: precision_loss_indicator)
{
  "applies $\explodeintervals$ to each constraint of {cs}
    in {tenv}, and returns a pair consisting of the list
    of exploded constraints in {new_cs} and a
    \precisionlossindicatorterm{} {p}.",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    cs = empty_list;
    --
    (empty_list, Precision_Full);
  }

  case non_empty {
    cs =: match_cons(c, cs1);
    explode_constraint(tenv, c) -> (c', p1);
    explode_intervals(tenv, cs1) -> (cs1', p2);
    p := precision_join(p1, p2);
    new_cs := concat(c', cs1');
    --
    (new_cs, p);
  }
;

// Transliteration note: the implementation uses folding where explode_constraint
// is a folder. To simplify this function, we remove the input precision lost flag
// and join all precision loss flags in explode_intervals.
typing function explode_constraint(tenv: static_envs, c: int_constraint) ->
  (vcs: list0(int_constraint), new_prec: precision_loss_indicator)
{
  "given the \staticenvironmentterm{} {tenv} and the constraint {c},
  expands {c} into the equivalent list of exact constraints if
  {c} matches an ascending range constraint that is not too large.
  Otherwise, it returns the singleton list for {c}, otherwise.
  The resulting list of constraints and the \precisionlossindicatorterm{}
  are {vcs} and {new_prec}, respectively.",
  prose_application = "exploding the constraint {c} in {tenv} yields {vcs} and {new_prec}",
  math_layout = [_,_],
} =
  case exact {
    c = Constraint_Exact(_);
    --
    (make_singleton_list(c), Precision_Full);
  }

  case range_reduced_too_large {
    c =: Constraint_Range(a, b);
    reduce_to_z_opt(tenv, a) -> some(z_a);
    reduce_to_z_opt(tenv, b) -> some(z_b);
    interval_too_large(z_a, z_b) -> True;
    --
    (make_singleton_list(c), Precision_Lost);
  }

  case range_reduced_not_too_large {
    c =: Constraint_Range(a, b);
    reduce_to_z_opt(tenv, a) -> some(z_a);
    reduce_to_z_opt(tenv, b) -> some(z_b);
    interval_too_large(z_a, z_b) -> False;
    exploded_interval := list_map(z, range_list(z_a, z_b), Constraint_Exact(ELint(z)));
    --
    (exploded_interval, Precision_Full);
  }

  case range_not_reduced {
    c =: Constraint_Range(a, b);
    reduce_to_z_opt(tenv, a) -> z_a_opt;
    reduce_to_z_opt(tenv, b) -> z_b_opt;
    or(z_a_opt = None, z_b_opt = None);
    --
    (make_singleton_list(c), Precision_Full);
  }
;

typing function interval_too_large(z1: Z, z2: Z) ->
         (b: Bool)
{
  "determines whether the set of numbers between {z1} and
  {z2}, inclusive, contains more than
  $\maxexplodedintervalsize$ integers, yielding the
  result in {b}.",
  prose_application = "",
} =
  --
  z2 - z1 > max_exploded_interal_size;
;

typing function binop_is_exploding(op: binop) ->
         (b: Bool)
{
  "determines whether the binary operation {op} should
  lead to applying $\explodeintervals$ when the {op} is
  applied to a pair of constraint lists. It is assumed
  that {op} is one of $\MUL$, $\SHL$, $\POW$, $\ADD$,
  $\DIV$, $\SUB$, $\MOD$, $\SHR$, and $\DIVRM$.",
  prose_application = "",
} =
  --
  op in make_set(DIV, DIVRM, MOD, MUL, POW, SHL, SHR);
;

typing function bitfields_included(tenv: static_envs, bfs1: list0(bitfield), bfs2: list0(bitfield)) -> (b: Bool) | type_error
{
    "tests whether the set of bit fields in {bfs1} is included in the set of bit fields in {bfs2}
    in the static environment {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {bfs1} is included in {bfs2} in {tenv} yields {b}\ProseOrTypeError",
} =
  INDEX(i, bfs1: mem_bfs(tenv, bfs2, bfs1[i]) -> bf_memberships[i]);
  --
  list_and(bf_memberships);
;

typing function mem_bfs(tenv: static_envs, bfs2: list0(bitfield), bf1: bitfield) ->
         (b: Bool)
{
  "checks whether the bitfield {bf1} exists in {bfs2} in
  the context of {tenv}, returning the result in {b}.",
  prose_application = "",
} =
  case none {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> None;
    --
    False;
  }

  case simple_any {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    ast_label(bf2) = label_BitField_Simple;
    bitfield_equal(tenv, bf1, bf2) -> b;
    --
    b;
  }

  case nested_simple {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Nested(name2, slices2, bfs2');
    bf1 = BitField_Simple(_, _);
    bitfield_equal(tenv, bf1, bf2) -> b;
    --
    False;
  }

  case nested_nested {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Nested(name2, slices2, bfs2');
    bf1 =: BitField_Nested(name1, slices1, bfs1);
    bool_transition(name1 = name2) -> True | False;
    slices_equal(tenv, slices1, slices2) -> equal_slices;
    bitfields_included(tenv, bfs1, bfs2') -> bitfields_included;
    --
    equal_slices && bitfields_included;
  }

  case nested_typed {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Nested(name2, slices2, bfs2');
    ast_label(bf1) = label_BitField_Type;
    --
    False;
  }

  case typed_simple {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Type(name2, slices2, t_ty2);
    bf1 = BitField_Simple(_, _);
    bitfield_equal(tenv, bf1, bf2) -> b;
    --
    b;
  }

  case typed_nested {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Type(name2, slices2, t_ty2);
    ast_label(bf1) = label_BitField_Nested;
    --
    False;
  }

  case typed_typed {
    bitfield_get_name(bf1) -> name;
    find_bitfield_opt(name, bfs2) -> some(bf2);
    bf2 =: BitField_Type(name2, slices2, t_ty2);
    bf1 =: BitField_Type(name1, slices1, t_ty1);
    b1 := name1 = name2;
    slices_equal(tenv, slices1, slices2) -> b2;
    subtype_satisfies(tenv, t_ty1, t_ty2) -> b3;
    --
    b1 && b2 && b3;
  }
;

typing function check_structure_label(tenv: static_envs, t: ty, l: ASTLabels) -> CheckResult | type_error
{
  "returns $\True$ if {t} has the \structureterm{} of a type corresponding to the AST label {l}. \ProseOtherwiseTypeError",
  prose_application = "checking whether the \structureterm{} of {t} has the AST label {l} yields $\True$\ProseOrTypeError",
  math_macro = \checkstructurelabel,
} =
  case okay {
    get_well_constrained_structure(tenv, t) -> t';
    ast_label(t') = l;
    --
    True;
  }

  case error {
    get_well_constrained_structure(tenv, t) -> t';
    ast_label(t') != l;
    --
    TypeError(TE_UT);
  }
;

typing function to_well_constrained(t: ty) ->
         (t': ty)
{
  "returns {t'}, the \wellconstrainedversionterm{} of a type {t}, which converts
  \parameterizedintegertypesterm{} to
  \wellconstrainedintegertypesterm{}, and leaves all
  other types as are.",
  prose_application = "",
} =
  case t_int_parameterized {
    t =: T_Int(Parameterized(v));
    --
    T_Int(WellConstrained(make_singleton_list(AbbrevConstraintExact(E_Var(v)))));
  }

  case t_int_other {
    t =: T_Int(i);
    i != Parameterized;
    --
    t;
  }

  case other {
    ast_label(t) != label_T_Int;
    --
    t;
  }
;

typing function get_well_constrained_structure(tenv: static_envs, t: ty) ->
         (t': ty) | type_error
{
  "returns the \wellconstrainedstructureterm{} of a type
  {t} in the \staticenvironmentterm{} {tenv} --- {t'},
  which is defined as follows. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  get_structure(tenv, t) -> t1;
  to_well_constrained(t1) -> t';
  --
  t';
;

typing function get_bitvector_width(tenv: static_envs, t: ty) ->
         (e: expr) | type_error
{
  "returns the expression {e}, which represents the width
  of the bitvector type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case okay {
    get_structure(tenv, t) -> T_Bits(e, _);
    --
    e;
  }

  case error {
    get_structure(tenv, t) -> t';
    ast_label(t') != label_T_Bits;
    --
    TypeError(TE_UT);
  }
;

typing function get_bitvector_const_width(tenv: static_envs, t: ty) ->
         (w: Z) | type_error
{
  "returns the natural number {w}, which represents the
  width of the bitvector type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  get_bitvector_width(tenv, t) -> e_width;
  static_eval(tenv, e_width) -> L_Int(w);
  --
  w;
;

typing function check_bits_equal_width(tenv: static_envs, t1: ty, t2: ty) ->
         CheckResult | type_error
{
  "tests whether the types {t1} and {t2} are bitvector
  types of the same width. If the answer is positive,
  the result is $\True$. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  get_bitvector_width(tenv, t1) -> n;
  get_bitvector_width(tenv, t2) -> m;
  bitwidth_equal(tenv, n, m) -> b;
  te_check(b, TE_UT) -> True;
  --
  True;
;

typing function precision_join(p1: precision_loss_indicator, p2: precision_loss_indicator) ->
         (p: precision_loss_indicator)
{
  "returns the \precisionlossindicatorterm{} {p},
  denoting whether {p1} or {p2} denote a precision loss.",
  prose_application = "",
  math_layout = [_,_],
} =
  case loss {
    p1 = Precision_Lost || p2 = Precision_Lost;
    --
    Precision_Lost;
  }

  case full {
    p1 = Precision_Full && p2 = Precision_Full;
    --
    Precision_Full;
  }
;

//////////////////////////////////////////////////
// Relations for Semantics Utilities

semantics function get_pending_calls(denv: dynamic_envs, name: Identifier) ->
         (s: N)
{
  "retrieves the value associated with {name} in
  $\denv.\pendingcalls$ or $0$ if no value is associated
  with it.",
  prose_application = "\hyperlink{relation-getpendingcalls}{retrieving} pending calls count for {name} in {denv} yields {s}",
} =
  s := if map_apply_opt(denv.dynamic_envs_G.pending_calls, name) =: some(s') then s' else zero;
  --
  s;
;

semantics function set_pending_calls(genv: global_dynamic_envs, name: Identifier, v: N) ->
         (new_genv: global_dynamic_envs)
{
  "updates the value bound to {name} in $\genv.\storage$
  to {v}, yielding the new global dynamic environment
  {new_genv}.",
  prose_application = "",
} =
  updated_pending := map_update(genv.pending_calls, name, v);
  --
  genv(pending_calls: updated_pending);
;

semantics function incr_pending_calls(genv: global_dynamic_envs, name: Identifier) ->
         (new_genv: global_dynamic_envs)
{
  "increments the value associated with {name} in
  $ {genv}.\pendingcalls $, yielding the updated global
  dynamic environment {new_genv}.",
  prose_application = "incrementing the number of pending calls for {name} in {genv} yields {new_genv}",
} =
  denv := [dynamic_envs_G: genv, dynamic_envs_L: empty_denv.dynamic_envs_L];
  get_pending_calls(denv, name) -> prev;
  set_pending_calls(genv, name, prev + one) -> new_genv;
  --
  new_genv;
;

semantics function decr_pending_calls(genv: global_dynamic_envs, name: Identifier) ->
         (new_genv: global_dynamic_envs)
{
  "decrements the value associated with {name} in
  $\genv.\pendingcalls$, yielding the updated global
  dynamic environment {new_genv}. It is assumed that\\
  $\getpendingcalls((\genv, \emptyfunc), \name)$ yields
  a positive value.",
  prose_application = "",
} =
  denv := [dynamic_envs_G: genv, dynamic_envs_L: empty_denv.dynamic_envs_L];
  get_pending_calls(denv, name) -> prev;
  set_pending_calls(genv, name, prev - one) -> new_genv;
  --
  new_genv;
;

semantics function remove_local(env: envs, name: Identifier) -> (new_env: envs)
{
   prose_description = "removes the binding of the identifier {name} from the
                        local storage of the environment {env}, yielding the
                        environment {new_env}.",
 prose_application = ""
} =
  env =: (tenv, denv);
  updated_local := map_update(denv.dynamic_envs_L, name, bot);
  new_denv := denv(dynamic_envs_L: updated_local);
  --
  (tenv, new_denv);
;

semantics relation read_identifier(name: Identifier, v: native_value) -> (XGraphs)
{
  prose_description = "creates an \executiongraphterm{} that represents the
                        reading of the value {v} into a storage element given
                        by the identifier {name}.",
  prose_application = ""
} =
  nodes := make_set(ReadEffect(name));
  edges := empty_set;
  outputs := make_set(ReadEffect(name));
  --
  (nodes, edges, outputs);
;

semantics relation write_identifier(name: Identifier, v: native_value) -> (XGraphs)
{
  prose_description = "creates an \executiongraphterm{} that represents the
                        writing of the value {v} into the storage element
                        given by an identifier {name}.",
  prose_application = ""
} =
  nodes := make_set(WriteEffect(name));
  edges := empty_set;
  outputs := make_set(WriteEffect(name));
  --
  (nodes, edges, outputs);
;

semantics function concat_bitvectors(vs: list0(tbitvector)) ->
         (new_vs: tbitvector)
{
  "transforms a (possibly empty) list of bitvector
  \nativevaluesterm{} {vs} into a single bitvector
  {new_vs}.",
  prose_application = "",
} =
  case empty {
    vs = empty_list;
    --
    nvbitvector(empty_list);
  }

  case non_empty {
    vs =: match_cons(v, vs');
    v =: nvbitvector(bits);
    concat_bitvectors(vs') =: nvbitvector(bits');
    new_bits := concat(bits, bits');
    --
    nvbitvector(new_bits);
  }
;

semantics function slices_to_positions(slices: list0((native_value, native_value))) ->
         (positions: list0(Z)) | TDynError
{
  "returns the list of positions (indices) specified by
  a list of slices expressed as pairs. Each pair specifies the start
  and length of a slice.
  If all slices consist of only
  non-negative integers. \ProseOtherwiseDynamicError",
  prose_application = "",
} =
  case empty {
    slices = empty_list;
    --
    empty_list;
  }

  case non_empty {
    slices =: match_cons(slice, slices1);
    slice =: (s_v, l_v);
    s_v =: NV_Literal(L_Int(s));
    l_v =: NV_Literal(L_Int(l));
    de_check(s >= zero && l >= zero, DE_BI) -> True;
    positions1 := range_list(s, s + l - one);
    slices_to_positions(slices1) -> positions2;
    --
    concat(positions1, positions2);
  }
;

semantics function max_pos_of_slice((s: native_value, l: native_value)) ->
         (max_pos: native_value)
{
  "returns the maximum position specified by the slice
  starting at {s} of length {l}, assuming that all slices consist only of
  non-negative integers.",
  prose_application = "",
} =
  s =: nvint(sv);
  l =: nvint(lv);
  case zero_width {
    lv = zero;
    --
    s;
  }

  case non_zero_width {
    lv != zero;
    --
    NV_Literal(L_Int(sv + lv - one));
  }
;

semantics function read_from_bitvector(v: native_value, slices: list0((native_value, native_value))) ->
         (new_v: tbitvector) | TDynError
{
  "reads from a bitvector {v}, or an integer seen as a
  bitvector, the indices specified by the list of slices
  {slices}, thereby concatenating their values, yielding the new bitvector {new_v}.",
  prose_application = "",
} =
  case bitvector_empty {
    v =: nvbitvector(_);
    slices = empty_list;
    --
    nvbitvector(empty_list);
  }

  case bitvector_non_empty {
    v =: nvbitvector(bv);
    slices_to_positions(slices) -> positions;
    positions != empty_list;
    max_of_slices := list_map(s, slices, max_pos_of_slice(s));
    max_pos := list_max(max_of_slices);
    n := list_len(bv) - one;
    de_check(max_pos <= n, DE_BI) -> True;
    new_bits := list_map(j, positions, bv[n_to_n_pos(j)]);
    --
    nvbitvector(new_bits);
  }

  case integer_empty {
    slices = empty_list;
    --
    nvbitvector(empty_list);
  }

  case integer {
    v =: nvint(i);
    slices_to_positions(slices) -> positions;
    positions != empty_list;
    max_of_slices := list_map(s, slices, max_pos_of_slice(s));
    max_pos := list_max(max_of_slices);
    int_to_bits(i, max_pos + one) -> bits;
    new_bits := list_map(j, positions, bits[abs_value(j)]);
    --
    nvbitvector(new_bits);
  }
;

semantics function write_to_bitvector(slices: list0((native_value, native_value)), src: native_value, dst: native_value) ->
         (v: native_value) | TDynError
{
  "overwrites the bits of {dst} at the positions given by
  {slices} with the bits of {src}.",
  prose_application = "\hyperlink{relation-writetobitvector}{writing} bits from {src} to {dst} at positions {slices} yields bitvector {v}",
} =
  src =: nvbitvector(src_bits);
  dst =: nvbitvector(dst_bits);
  slices_to_positions(slices) -> positions;
  de_check(same_length(positions, src_bits), DE_BI) -> True;
  case empty_positions {
    positions = empty_list;
    --
    dst;
  }

  case non_empty_positions {
    positions != empty_list;
    max_of_slices := list_map(s, slices, max_pos_of_slice(s));
    max_pos := list_max(max_of_slices);
    de_check(max_pos < list_len(dst_bits), DE_BI) -> True;
    pos_map := bindings_to_map(list_combine(positions, src_bits));
    new_bits :=
      list_map(j, indices(dst_bits),
        if map_apply_opt(pos_map, j) =: some(b) then b else dst_bits[j])
    { (_, (_, _, [_])) };
    --
    nvbitvector(new_bits);
  }
;

semantics function get_index(i: N, vec: native_value) -> (r: native_value) | TDynError
{
   prose_description = "reads the value {r} from the vector of values {vec}
                        at the index {i}. \ProseOtherwiseDynamicError",
 prose_application = ""
} =
  vec =: NV_Vector(values);
  case ok {
    zero <= i && i < list_len(values);
    --
    values[i];
  }

  case error {
    i < zero || i >= list_len(values);
    --
    DynamicError(DE_BI);
  }
;

semantics function set_index(i: N, v: native_value, vec: native_value) -> (res: tvector) | TDynError
{
   prose_description = "overwrites the value at the given index {i} in a
                        vector of values {vec} with the new value {v}.
                        \ProseOtherwiseDynamicError",
 prose_application = "",
} =
  vec =: NV_Vector(values);
  case ok {
    zero <= i && i < list_len(values);
    updated := list_map(j, indices(values), if j = i then v else values[j]);
    --
    NV_Vector(updated);
  }

  case error {
    i < zero || i >= list_len(values);
    --
    DynamicError(DE_BI);
  }
;

semantics function get_field(name: Identifier, record: native_value) -> (v: native_value)
{
  prose_description = "retrieves the value {v} corresponding to the field name
                        {name} from the record value {record}.",
  prose_application = ""
} =
  record =: NV_Record(field_map);
  --
  map_apply(field_map, name);
;

semantics function set_field(name: Identifier, v: native_value, record: native_value) -> (trecord)
{
   prose_description = "overwrites the value corresponding to the field name
                        {name} in the record value {record} with the value
                        {v}.",
 prose_application = "",
} =
  record =: NV_Record(field_map);
  field_map' := map_update(field_map, name, v);
  --
  NV_Record(field_map');
;

semantics relation declare_local_identifier(env: envs, name: Identifier, v: native_value) -> (new_env: envs, g: XGraphs)
{
   prose_description = "associates {v} to {name} as a local storage element
                        in the environment {env} and returns the updated
                        environment {new_env} with the execution graph
                        consisting of a Write Effect to {name}.",
 prose_application = "",
} =
  write_identifier(name, v) -> g;
  env =: (tenv, denv);
  updated_local := map_update(denv.dynamic_envs_L, name, v);
  new_denv := denv(dynamic_envs_L: updated_local);
  new_env := (tenv, new_denv);
  --
  (new_env, g);
;

semantics relation declare_local_identifier_m(env: envs, x: Identifier, m: (native_value, XGraphs)) ->
         (new_env: envs, new_g: XGraphs)
{
  "declares the local identifier {x} in the environment
  {env}, in the context of the value-graph pair $(\vv,
  \vg)$, yielding a pair consisting of the environment
  {new_env} and \executiongraphterm{} {new_g}.",
  prose_application = "\hyperlink{relation-declarelocalidentifierm}{declaring} local identifier {x} in {env} with value-graph pair {m} yields environment {new_env} and graph {new_g}",
} =
  m =: (v, g);
  declare_local_identifier(env, x, v) -> (new_env, g1);
  new_g := ordered_data(g, g1);
  --
  (new_env, new_g);
;

semantics relation declare_local_identifier_mm(env: envs, x: Identifier, m: (native_value, XGraphs)) ->
         (new_env: envs, new_g: XGraphs)
{
  "declares the local identifier {x} in the environment
  {env}, in the context of the value-graph pair $(\vv,
  \vg)$, yielding a pair consisting of an environment
  {new_env} and an \executiongraphterm{} {new_g}.",
  prose_application = "",
} =
  m =: (v, g);
  declare_local_identifier_m(env, x, m) -> (new_env, g1);
  new_g := ordered_po(g, g1);
  --
  (new_env, new_g);
;

//////////////////////////////////////////////////
// Relations for Side Effects

typing function side_effect_is_pure(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\pureterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \pureterm{} yields {b}",
} =
  b := s in make_set(GlobalEffect(SE_Pure), Immutability(True), LocalEffect(SE_Pure));
  --
  b;
;

typing function side_effect_is_readonly(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\readonlyterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \readonlyterm{} yields {b}",
} =
  case local_effect {
    s =: LocalEffect(p);
    --
    ge_pure(p, SE_Readonly);
  }

  case global_effect {
    s =: GlobalEffect(p);
    --
    ge_pure(p, SE_Readonly);
  }

  case immutability {
    s = Immutability(_);
    --
    True;
  }
;

typing function side_effect_is_symbolically_evaluable(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\symbolicallyevaluableterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \symbolicallyevaluableterm{} yields {b}",
} =
  case local_effect {
    s =: LocalEffect(p);
    --
    ge_pure(p, SE_Readonly);
  }

  case global_effect {
    s =: GlobalEffect(p);
    --
    ge_pure(p, SE_Readonly);
  }

  case immutability {
    s =: Immutability(b);
    --
    b;
  }
;

typing function ses_ldk(ldk: local_decl_keyword) ->
         (s: powerset(TSideEffect))
{
  "constructs a \sideeffectsetterm{} {s} corresponding to
  a read of a storage element declared with a local
  declaration keyword {ldk}.",
  prose_application = "",
} =
  b := (ldk = LDK_Let);
  --
  make_set(Immutability(b), LocalEffect(SE_Readonly));
;

typing function ses_gdk(gdk: global_decl_keyword) ->
         (s: powerset(TSideEffect))
{
  "constructs a \sideeffectsetterm{} {s} corresponding to
  a read of a storage element declared with a global
  declaration keyword {gdk}.",
  prose_application = "",
} =
  purity := if (gdk = GDK_Constant) then SE_Pure else SE_Readonly;
  b := (gdk != GDK_Var);
  --
  make_set(GlobalEffect(purity), Immutability(b));
;

typing function is_symbolically_evaluable(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether a set of \sideeffectdescriptorsterm\
  {ses} are all \symbolicallyevaluableterm, yielding the
  result in {b}.",
  prose_application = "",
} =
  b := forall(s, ses, side_effect_is_symbolically_evaluable(s));
  --
  b;
;

typing function check_symbolically_evaluable(ses: powerset(TSideEffect)) -> CheckResult | type_error
{
  "returns $\True$ if the set of \sideeffectdescriptorsterm{} {ses} is \symbolicallyevaluableterm.
  \ProseOtherwiseTypeError",
  prose_application = "checking whether {ses} is \symbolicallyevaluableterm{} yields $\True$\OrTypeError",
} =
  is_symbolically_evaluable(ses) -> b;
  te_check(b, TE_SEV) -> True;
  --
  True;
;

typing function ses_is_readonly(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether all side effects in the set {ses} are
  \readonlyterm{}, yielding the result in {b}.",
  prose_application = "",
} =
  b := forall(s, ses, side_effect_is_readonly(s));
  --
  b;
;

typing function ses_is_pure(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether all side effects in the set {ses} are
  \pureterm{}, yielding the result in {b}.",
  prose_application = "",
} =
  b := forall(s, ses, side_effect_is_pure(s));
  --
  b;
;


typing function ses_for_subprogram(qualifier: option(func_qualifier)) ->
         (s: powerset(TSideEffect))
{
  "produces a \sideeffectsetterm{} given a subprogram
  qualifier {qualifier}.",
  prose_application = "",
} =
  case none_or_noreturn {
    qualifier = None || qualifier = some(Noreturn);
    s := make_set(GlobalEffect(SE_Impure), Immutability(False));
    --
    s;
  }

  case some_readonly {
    qualifier = some(Readonly);
    s := make_set(GlobalEffect(SE_Readonly), Immutability(False));
    --
    s;
  }

  case some_pure {
    qualifier = some(Pure);
    s := make_set(GlobalEffect(SE_Pure), Immutability(True));
    --
    s;
  }
;

//////////////////////////////////////////////////
// Relations for Slicing

typing relation annotate_slice(tenv: static_envs, s: slice) -> (s': slice, ses: powerset(TSideEffect)) | type_error
{
  "annotates a single slice {s} in the \staticenvironmentterm{} {tenv},
  resulting in an annotated slice {s'} and a \sideeffectsetterm{}
  {ses}. \ProseOtherwiseTypeError",
  prose_application = "annotating the slice {s} in {tenv} yields {s'}\OrTypeError"
} =
  case single {
    s =: Slice_Single(i);
    annotate_slice(tenv, Slice_Length(i, ELint(one))) -> (s', ses);
  }

  case range {
    s =: Slice_Range(j, i);
    length' := AbbrevEBinop(SUB, j, i);
    length  := AbbrevEBinop(ADD, length', ELint(one));
    annotate_slice(tenv, Slice_Length(i, length)) -> (s', ses);
  }

  case length {
    s =: Slice_Length(offset, length);
    annotate_expr(tenv, offset) -> (t_offset, offset', ses_offset);
    annotate_symbolic_constrained_integer(tenv, length) -> (length', ses_length) { math_layout = [_] };
    te_check(ses_is_readonly(ses_offset), TE_SEV) -> True;
    te_check(ses_is_readonly(ses_length), TE_SEV) -> True;
    check_underlying_integer(tenv, t_offset) -> True;
    ses := union(ses_offset, ses_length);
    s' := Slice_Length(offset', length');
  }

  case scaled {
    s =: Slice_Star(factor, length);
    offset := AbbrevEBinop(MUL, factor, length);
    annotate_slice(tenv, Slice_Length(offset, length)) -> (s', ses);
  }
  --
  (s', ses);
;

typing relation slices_width(tenv: static_envs, slices: list0(slice)) ->
         (width: expr) | type_error
{
  "returns an expression {slices} that represents the
  width of all slices given by {slices} in the
  \staticenvironmentterm{} {tenv}.",
  prose_application = "",
} =
  case empty {
    slices = empty_list;
    --
    ELint(zero);
  }

  case non_empty {
    slices =: match_cons(s, slices1);
    slice_width(s) -> e1;
    slices_width(tenv, slices1) -> e2;
    normalize(tenv, AbbrevEBinop(ADD, e1, e2)) -> width;
    --
    width;
  }
;

typing function slice_width(slice: slice) ->
         (width: expr)
{
  "returns an expression {width} that represents the
  width of the slices given by {slice}.",
  prose_application = "",
} =
  case single {
    slice = Slice_Single(_);
    --
    ELint(one);
  }

  case scaled {
    slice =: Slice_Star(_, e);
    --
    e;
  }

  case length {
    slice =: Slice_Length(_, e);
    --
    e;
  }

  case range {
    slice =: Slice_Range(e1, e2);
    --
    AbbrevEBinop(ADD, ELint(one), AbbrevEBinop(SUB, e1, e2));
  }
;

typing relation annotate_symbolic_constrained_integer(tenv: static_envs, e: expr) ->
         (e'': expr, ses: powerset(TSideEffect)) | type_error
{
  "annotates a \symbolicallyevaluableterm{} integer
  expression {e} of a constrained integer type in the
  \staticenvironmentterm{} {tenv} and returns the
  annotated expression {e''} and a \sideeffectsetterm{}
  {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_symbolically_evaluable_expr(tenv, e) -> (t, e', ses);
  check_constrained_integer(tenv, t) -> True;
  normalize(tenv, e') -> e'';
  --
  (e'', ses);
;

typing relation annotate_slices(tenv: static_envs, slices: list0(slice)) ->
         (slices': list0(slice), ses: powerset(TSideEffect))
{
  "annotates a list of slices {slices} in the
  \staticenvironmentterm{} {tenv}, yielding a list of
  annotated slices (that is, slices in the \typedast)
  and a \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  INDEX(i, slices: annotate_slice(tenv, slices[i]) -> (slices'[i], xs[i]));
  ses := union_list(xs);
  --
  (slices', ses);
;

semantics relation eval_slice(env: envs, s: slice) ->
  | (((v_start: native_value, v_length: native_value), new_g: XGraphs), new_env: envs)
  | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates an individual slice {s} in an environment
                        {env}, resulting in \\
                        $((({v_start}, {v_length}), {new_g}), {new_env})$.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
} =
  case single {
    s =: Slice_Single(e);
    eval_expr(env, e) -> ResultExpr((v_start, new_g), new_env);
    v_length := nvint(one);
  }

  case range {
    s =: Slice_Range(e_top, e_start);
    eval_expr(env, e_top) -> ResultExpr(m_top, env1);
    (v_top, g1) := m_top;
    eval_expr(env1, e_start) -> ResultExpr(m_start, new_env);
    (v_start, g2) := m_start;
    eval_binop(SUB, v_top, v_start) -> v_diff;
    eval_binop(ADD, nvint(one), v_diff) -> v_length;
    new_g := parallel(g1, g2);
  }

  case length {
    s =: Slice_Length(e_start, e_length);
    eval_expr(env, e_start) -> ResultExpr(m_start, env1);
    (v_start, g1) := m_start;
    eval_expr(env1, e_length) -> ResultExpr(m_length, new_env);
    (v_length, g2) := m_length;
    new_g := parallel(g1, g2);
  }

  case scaled {
    s =: Slice_Star(e_factor, e_length);
    eval_expr(env, e_factor) -> ResultExpr(m_factor, env1);
    (v_factor, g1) := m_factor;
    eval_expr(env1, e_length) -> ResultExpr(m_length, new_env);
    (v_length, g2) := m_length;
    eval_binop(MUL, v_factor, v_length) -> v_start;
    new_g := parallel(g1, g2);
  }
  --
  (((v_start, v_length), new_g), new_env);
;

relation eval_slices(env: envs, slices: list0(slice)) ->
  | ResultSlices((ranges: list0((native_value, native_value)), new_g: XGraphs), new_env: envs)
  | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a list of slices {slices} in an environment
                        {env}, resulting in \\
                        $\ResultSlices((\ranges, \newg), \newenv)$.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    slices = empty_list;
    --
    ResultSlices((empty_list, empty_graph), env);
  }

  case non_empty {
    slices =: match_cons(slice, slices1);
    eval_slice(env, slice) -> ((range, g1), env1);
    eval_slices(env1, slices1) -> ResultSlices((ranges1, g2), new_env);
    ranges := match_cons(range, ranges1);
    new_g := parallel(g1, g2);
    --
    ResultSlices((ranges, new_g), new_env);
  }
;

//////////////////////////////////////////////////
// Relations for Specifications

typing relation typecheck_decl(genv: global_static_envs, d: decl) ->
         (new_d: decl, new_genv: global_static_envs) | type_error
{
  "annotates a global declaration {d} in the
  \globalstaticenvironmentterm{} {genv}, yielding an
  annotated global declaration {new_d} and modified
  \globalstaticenvironmentterm{} {new_genv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case global_storage {
    d =: D_GlobalStorage(gsd);
    declare_global_storage(genv, gsd) -> (new_genv, gsd');
    --
    (D_GlobalStorage(gsd'), new_genv);
  }

  case type_decl {
    d =: D_TypeDecl(x, ty, s);
    declare_type(genv, x, ty, s) -> (new_genv, ty', s');
    --
    (D_TypeDecl(x, ty', s'), new_genv);
  }

  case func {
    d =: D_Func(f);
    annotate_and_declare_func(genv, f) -> (tenv1, f1, ses_func_sig);
    annotate_subprogram(tenv1, f1, ses_func_sig) -> (new_f, ses_f);
    add_subprogram(tenv1, new_f.name, new_f, ses_f) -> new_tenv;
    --
    (D_Func(new_f), new_tenv.static_envs_G);
  }
;

render rule typecheck_decl_global_storage = typecheck_decl(global_storage);
render rule typecheck_decl_type_decl = typecheck_decl(type_decl);
render rule typecheck_decl_func = typecheck_decl(func);

typing relation type_check_ast(genv: global_static_envs, decls: list0(decl)) ->
         (new_decls: list0(decl), new_tenv: static_envs) | type_error
{
  "annotates a list of declarations {decls} in an input
  \globalstaticenvironmentterm{} {genv}, yielding an
  output \staticenvironmentterm{} {new_tenv} and
  annotated list of declarations {new_decls}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing relation annotate_decl_comps(genv: global_static_envs, comps: list0((list0(decl)))) ->
         (new_genv: global_static_envs, new_decls: list0(decl)) | type_error
{
  "annotates a list of declaration components {comps} (a
  list of lists) in the \globalstaticenvironmentterm{}
  {genv}, yielding the annotated list of declarations
  {new_decls} and modified
  \globalstaticenvironmentterm{} {new_genv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case empty {
    comps = empty_list;
    --
    (genv, empty_list);
  }

  case single {
    comps =: match_cons(comp, comps1);
    comp =: match_singleton_list(d);
    typecheck_decl(genv, d) -> (d1, genv1);
    annotate_decl_comps(genv1, comps1) -> (new_genv, decls1);
    new_decls := concat(make_singleton_list(d1), decls1);
    --
    (new_genv, new_decls);
  }

  case mutually_recursive {
    comps =: match_cons(comp, comps1);
    list_len(comp) > one;
    type_check_mutually_rec(genv, comp) -> (decls1, genv1);
    annotate_decl_comps(genv1, comps1) -> (new_genv, decls2);
    --
    (new_genv, concat(decls1, decls2));
  }
;

typing relation type_check_mutually_rec(genv: global_static_envs, decls: list0(decl)) ->
         (new_decls: list0(decl), new_genv: global_static_envs) | type_error
{
  "annotates a list of mutually recursive declarations
  {decls} in the \globalstaticenvironmentterm{} {genv},
  yielding the annotated list of subprogram declarations
  {new_decls} and modified
  \globalstaticenvironmentterm{} {new_genv}.",
  prose_application = "",
};

typing function declare_subprograms(
    genv: global_static_envs,
    env_and_fs: list0((local_static_envs, func, powerset(TSideEffect)))) ->
         (new_genv: global_static_envs,
         new_env_and_fs: list0((local_static_envs, func, powerset(TSideEffect))))
         | type_error
{
  "processes a list of pairs, each consisting of a
  \localstaticenvironmentterm{} and a subprogram
  declaration, {env_and_fs}, in the context of a
  \globalstaticenvironmentterm{} {genv}, declaring each
  subprogram in the environment consisting of {genv} and
  the static local environment associated with each
  subprogram. The result is a modified
  \globalstaticenvironmentterm{} {new_genv} and list of
  tuples {new_env_and_fs} consisting of
  \localstaticenvironmentterm, annotated $\func$ AST
  node, and \sideeffectdescriptorsetsterm.",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    env_and_fs = empty_list;
    --
    (genv, empty_list);
  }

  case non_empty {
    env_and_fs =: match_cons((lenv, f, ses_f), env_and_fs1);
    tenv := [static_envs_G: genv, static_envs_L: lenv];
    declare_one_func(tenv, f, ses_f) -> (tenv1, f1);
    declare_subprograms(tenv1.static_envs_G, env_and_fs1) -> (new_genv, env_and_fs2);
    new_env_and_fs := cons((lenv, f1, ses_f), env_and_fs2);
    --
    (new_genv, new_env_and_fs);
  }
;

typing function add_subprogram_decls(tenv: static_envs, funcs: list0((func, powerset(TSideEffect)))) ->
         (new_tenv: static_envs)
{
  "adds each subprogram definition given by a $\func$ AST
  node in {funcs} to the $\subprograms$ map of
  $\tenv.\staticenvsG$, yielding {new_tenv}.",
  prose_application = "",
} =
  case empty {
    funcs = empty_list;
    --
    tenv;
  }

  case non_empty {
    funcs =: match_cons((f, ses_f), funcs1);
    add_subprogram(tenv, f.name, f, ses_f) -> tenv1;
    add_subprogram_decls(tenv1, funcs1) -> new_tenv;
    --
    new_tenv;
  }
;

typing relation override_subprograms(decls: list0(decl)) ->
         (decls': list0(decl)) | type_error
{
  "overrides subprograms in a list of declarations
  {decls}, yielding the new list of declarations
  {decls'}. \ProseOtherwiseTypeError{}",
  prose_application = "",
} =
  override_decls_sort(decls) -> (impdefs, impls, normals);
  check_implementations_unique(impls) -> True;
  process_overrides(impdefs, impls) -> (impdefs', discarded);
  rename_subprograms(discarded) -> renamed_discarded;
  overridden := concat(impdefs', impls);
  decls_overridden := list_map(f, overridden, D_Func(f));
  decls_renamed := list_map(f, renamed_discarded, D_Func(f));
  --
  concat(decls_overridden, decls_renamed, normals);
;

typing function override_decls_sort(
  decls: list0(decl)) ->
  (impdefs: list0(func), impls: list0(func), normals: list0(decl))
{
  "the splitting of {decls} into the sublist of \texttt{impdef} function definitions,
  the sublist of \texttt{implementation} function definitions, and the sublist
  of other declrations",
} =
  case empty {
    decls = empty_list;
    --
    (empty_list, empty_list, empty_list);
  }

  case non_empty {
    decls =: match_cons(d, decls_tail);
    override_decls_sort(decls_tail) -> (impdefs_tail, impls_tail, normals_tail);
    case impdef {
      (d =: D_Func(f)) && f.override = some(Impdef);
      --
      (cons(f, impdefs_tail), impls_tail, normals_tail);
    }

    case impl {
      (d =: D_Func(f)) && f.override = some(Implementation);
      --
      (impdefs_tail, cons(f, impls_tail), normals_tail);
    }

    case impl {
      not(d = D_Func(_)) || (d =: D_Func(f)) && f.override = None;
      --
      (impdefs_tail, impls_tail, cons(d, normals_tail));
    }
  }
;

typing function check_implementations_unique(impls: list0(func)) ->
         CheckResult | type_error
{
  "checks that the \Proseimplementationsubprograms{}
  {impls} have unique signatures.
  \ProseOtherwiseTypeError{}",
  prose_application = "",
} =
  case empty {
    impls = empty_list;
    --
    True;
  }

  case non_empty {
    impls =: match_cons(h, t);
    INDEX(i, t: signatures_match(h, t[i]) -> False);
    check_implementations_unique(t) -> True;
    --
    True;
  }
;

typing function signatures_match(func1: func, func2: func) ->
         (Bool)
{
  "checks whether the signatures of subprograms {func1}
  and {func2} match for overriding purposes.
  \ProseOtherwiseTypeError{}",
  prose_application = "",
} =
  match := and(
    func1.name = func2.name,
    func1.qualifier = func2.qualifier,
    same_length(func1.args, func2.args),
    func1.args = func2.args,
    same_length(func1.parameters, func2.parameters),
    func1.parameters = func2.parameters,
    func1.return_type = func2.return_type
  )
  { (_, [_]) };
  --
  True;
;

typing function process_overrides(impdefs: list0(func), impls: list0(func)) ->
         (impdefs': list0(func), discarded: list0(func)) | type_error
{
  "overrides the \Proseimpdefsubprograms{} {impdefs} with
  the \Proseimplementationsubprograms{} {impls},
  yielding the new \Proseimpdefsubprograms{} {impdefs}
  and the discarded subprograms {discarded}.
  \ProseOtherwiseTypeError{}",
  prose_application = "",
} =
  case empty {
    impls = empty_list;
    --
    (impdefs, empty_list);
  }

  case non_empty {
    impls =: match_cons(impl, imples_tail);
    INDEX(i, impdefs: signatures_match(impl, impdefs[i]) -> matches[i]);
    matching_lists :=
      list_map(i, indices(impdefs),
        if matches[i] then make_singleton_list(impdefs[i]) else empty_list
      ) { (_, (_, _, [_])) };
    nonmatching_lists :=
      list_map(i, indices(impdefs),
        if matches[i] then empty_list else make_singleton_list(impdefs[i])
      ) { (_, (_, _, [_])) };
    // TODO: improve the above with lister_filter_map
    matching := list_flatten(matching_lists);
    nonmatching := list_flatten(nonmatching_lists);
    te_check(list_len(matching) = one, TE_OE) -> True;
    process_overrides(nonmatching, imples_tail) -> (impdefs', discarded_p);
    discarded := concat(matching, discarded_p);
    --
    (impdefs', discarded);
  }
;

typing relation rename_subprograms(discarded: list0(func)) ->
         (renamed_discarded: list0(func))
{
  "renames the subprograms {discarded} to give them fresh
  names.",
  prose_application = "",
} =
  case empty {
    discarded = empty_list;
    --
    empty_list;
  }

  case non_empty {
    discarded =: match_cons(h, t);
    rename_subprograms(t) -> t';
    name := fresh_identifier();
    h' := h(name: name);
    --
    concat(make_singleton_list(h'), t');
  }
;

typing function build_dependencies(decls: list0(decl)) ->
         (defs: list0(def_use_name), depends: list0((def_use_name, def_use_name)))
{
  "takes a set of declarations {decls} and returns a
  graph whose set of nodes {defs} consists of
  the identifiers that are used to name declarations and
  whose set of edges {depends} consists of pairs $(a,b)$
  where the declaration of $a$ uses an identifier
  defined by the declaration of $b$. We refer to this
  graph as the \emph{\dependencygraphterm} (of {decls}).",
  prose_application = "",
  math_layout = [_,_]
} =
  defs_list := list_map(d, decls, def_decl(d));
  enum_lists := list_map(d, decls, list_set(def_enum_labels(d)));
  enum_labels := list_flatten(enum_lists);
  defs := concat(defs_list, enum_labels);
  dep_lists := list_map(d, decls, decl_dependencies(d));
  depends := list_flatten(dep_lists);
  --
  (defs, depends);
;

typing function decl_dependencies(d: decl) ->
         (depends: list0((def_use_name, def_use_name)))
{
  "returns the set of dependent pairs of identifiers
  {depends} induced by the declaration {d}.",
  prose_application = "",
} =
  id1 := def_decl(d);
  labels := list_set(def_enum_labels(d));
  used := list_set(use_decl(d));
  label_deps := list_map(id2, labels, (id1, id2));
  use_deps := list_map(id2, used, (id1, id2));
  --
  concat(label_deps, use_deps);
;

typing function def_decl(d: decl) ->
         (name: def_use_name)
{
  "returns the identifier {name} being defined by the
  declaration {d}.",
  prose_application = "",
} =
  case d_func {
    d =: D_Func(f);
    --
    Subprogram(f.name);
  }

  case d_globalstorage {
    d =: D_GlobalStorage(gs);
    --
    Other(gs.global_decl_name);
  }

  case d_typedecl {
    d =: D_TypeDecl(id, _, _);
    --
    Other(id);
  }
;

typing function def_enum_labels(d: decl) ->
         (labels: powerset(def_use_name))
{
  "takes a declaration {d} and returns the set of
  enumeration labels it defines in {labels}, if it
  defines any.",
  prose_application = "",
} =
  case decl_enum {
    d =: D_TypeDecl(_, T_Enum(labels1), _);
    labels := set_from_list(l, labels1, Other(l));
    --
    labels;
  }

  case other {
    not(d = D_TypeDecl(_, T_Enum(_), _));
    --
    empty_set;
  }
;

typing function use_decl(d: decl) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  declaration {d} depends on.",
  prose_application = "",
} =
  case decl_typedecl {
    d =: D_TypeDecl(_, ty, fields);
    ids := union(use_ty(ty), use_subtypes(fields));
    --
    ids;
  }

  case decl_globalstorage {
    d =: D_GlobalStorage(gs);
    init_ids := if gs.initial_value =: some(e) then use_expr(e) else empty_set;
    ty_ids := if gs.global_decl_ty =: some(ty) then use_ty(ty) else empty_set;
    --
    union(init_ids, ty_ids);
  }

  case decl_func {
    d =: D_Func(f);
    f.args =: list_combine(arg_names, arg_types);
    arg_ids := list_map(arg_ty, arg_types, use_ty(arg_ty));
    f.parameters =: list_combine(param_names, param_types);
    param_ids := list_map(param_ty_opt, param_types, if param_ty_opt =: some(param_ty) then use_ty(param_ty) else empty_set);
    ids_args := union_list(arg_ids);
    ids_params := union_list(param_ids);
    ret_ids := if f.return_type =: some(ty) then use_ty(ty) else empty_set;
    body_ids := use_stmt(f.func_body);
    limit_ids := if f.recurse_limit =: some(e) then use_expr(e) else empty_set;
    ids := union(ids_args, ids_params, ret_ids, body_ids, limit_ids);
    --
    ids;
  }
;

typing function use_ty(t: ty) -> (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the type
  {t} depends on.",
  prose_application = "",
} =
  case simple {
    ast_label(t) in make_set(label_T_Bool, label_T_Enum, label_T_Real, label_T_String);
    --
    empty_set;
  }

  case t_named {
    t =: T_Named(s);
    --
    make_set(Other(s));
  }

  case int_no_constraints {
    t =: T_Int(c);
    ast_label(c) in make_set(label_Parameterized, label_PendingConstrained, label_Unconstrained);
    --
    empty_set;
  }

  case int_well_constrained {
    t =: T_Int(WellConstrained(cs));
    cs_ids := list_map(c, cs, use_constraint(c));
    --
    union_list(cs_ids);
  }

  case t_tuple {
    t =: T_Tuple(li);
    ids_sets := list_map(ty1, li, use_ty(ty1));
    --
    union_list(ids_sets);
  }

  case structured {
    ast_label(t) in make_set(label_T_Collection, label_T_Exception, label_T_Record);
    t =: make_structured(L, fields);
    fields =: list_combine(field_names, field_types);
    field_ids := list_map(field_ty, field_types, use_ty(field_ty));
    --
    union_list(field_ids);
  }

  case array_expr {
    t =: T_Array(ArrayLength_Expr(e), ty);
    ids := union(use_expr(e), use_ty(ty));
    --
    ids;
  }

  case array_enum {
    t =: T_Array(ArrayLength_Enum(enum, _), ty);
    ids := union(make_set(Other(enum)), use_ty(ty));
    --
    ids;
  }

  case t_bits {
    t =: T_Bits(e, bitfields);
    field_ids := list_map(bf, bitfields, use_bitfield(bf));
    ids := union(use_expr(e), union_list(field_ids));
    --
    ids;
  }
;

typing function use_subtypes(fields: option((x: Identifier, subfields: list0(field)))) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  \optionalterm{} pair consisting of identifier {x} (the type
  being subtyped) and fields {subfields} depends on.",
  prose_application = "",
} =
  case none {
    fields = None;
    --
    empty_set;
  }

  case some {
    fields =: some((x1, subfields1));
    subfields1 =: list_combine(sub_names, sub_types);
    sub_ids := list_map(sub_ty, sub_types, use_ty(sub_ty));
    ids := union(make_set(Other(x1)), union_list(sub_ids));
    --
    ids;
  }
;

typing function use_expr(e: expr) -> (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  expression {e} depends on.",
  prose_application = "",
  math_macro = \useexpr
} =
  case e_literal {
    e = E_Literal(_);
    --
    empty_set;
  }

  case e_atc {
    e =: E_ATC(e1, ty);
    --
    union(use_expr(e1), use_ty(ty));
  }

  case e_var {
    e =: E_Var(x);
    --
    make_set(Other(x));
  }

  case e_getarray {
    e =: E_GetArray(e1, e2);
    --
    union(use_expr(e1), use_expr(e2));
  }

  case e_getenumarray {
    e =: E_GetEnumArray(e1, e2);
    --
    union(use_expr(e1), use_expr(e2));
  }

  case e_binop {
    e =: E_Binop(_, e1, e2);
    --
    union(use_expr(e1), use_expr(e2));
  }

  case e_unop {
    e =: E_Unop(_, e1);
    --
    use_expr(e1);
  }

  case e_call {
    e =: E_Call(call);
    param_ids := list_map(p, call.params, use_expr(p));
    arg_ids := list_map(a, call.call_args, use_expr(a));
    ids := union(make_set(Subprogram(call.call_name)), union_list(param_ids), union_list(arg_ids));
    --
    ids;
  }

  case e_slice {
    e =: E_Slice(e1, slices);
    slice_ids := list_map(s, slices, use_slice(s));
    --
    union(use_expr(e1), union_list(slice_ids));
  }

  case e_cond {
    e =: E_Cond(e1, e2, e3);
    --
    union(use_expr(e1), use_expr(e2), use_expr(e3));
  }

  case e_getitem {
    e =: E_GetItem(e1, _);
    --
    use_expr(e1);
  }

  case e_getfield {
    e =: E_GetField(e1, _);
    --
    use_expr(e1);
  }

  case e_getfields {
    e =: E_GetFields(e1, _);
    --
    use_expr(e1);
  }

  case e_record {
    e =: E_Record(ty, fields);
    fields =: list_combine(field_names, field_exprs);
    field_ids := list_map(field_expr, field_exprs, use_expr(field_expr));
    --
    union(use_ty(ty), union_list(field_ids));
  }

  case e_tuple {
    e =: E_Tuple(es);
    ids_sets := list_map(e1, es, use_expr(e1));
    --
    union_list(ids_sets);
  }

case e_array {
  e =: E_Array[length: e1, array_value: e2];
  --
  union(use_expr(e1), use_expr(e2));
}

  case e_enumarray {
    e =: E_EnumArray[enum: enum_id, labels: labels, enum_array_value: v];
    label_ids := list_map(l, labels, make_set(Other(l)));
    ids := union(make_set(Other(enum_id)), union_list(label_ids), use_expr(v));
    --
    ids;
  }

  case e_arbitrary {
    e =: E_Arbitrary(ty);
    --
    use_ty(ty);
  }

  case e_pattern {
    e =: E_Pattern(e1, pat);
    --
    union(use_expr(e1), use_pattern(pat));
  }

  case e_getcollectionfields {
    e =: E_GetCollectionFields(collection_name, _);
    --
    make_set(Other(collection_name));
  }
;

typing function use_lexpr(le: lexpr) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  left-hand-side expression {le} depends on.",
  prose_application = "",
  math_macro = \uselexpr
} =
  case le_var {
    le =: LE_Var(x);
    --
    make_set(Other(x));
  }

  case le_destructuring {
    le =: LE_Destructuring(les);
    ids_sets := list_map(le1, les, use_lexpr(le1));
    --
    union_list(ids_sets);
  }

  case le_discard {
    le = LE_Discard;
    --
    empty_set;
  }

  case le_setarray {
    le =: LE_SetArray(le1, e1);
    ids := union(use_lexpr(le1), use_expr(e1));
    --
    ids;
  }

  case le_setenumarray {
    le =: LE_SetEnumArray(le1, e1);
    ids := union(use_lexpr(le1), use_expr(e1));
    --
    ids;
  }

  case le_setfield {
    le =: LE_SetField(le1, _);
    --
    use_lexpr(le1);
  }

  case le_setfields {
    le =: LE_SetFields(le1, _);
    --
    use_lexpr(le1);
  }

  case le_slice {
    le =: LE_Slice(le1, slices);
    slice_ids := list_map(s, slices, use_slice(s));
    ids := union(use_lexpr(le1), union_list(slice_ids));
    --
    ids;
  }
;

typing function use_pattern(p: pattern) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  pattern {p} depends on.",
  prose_application = "",
} =
  case mask_all {
    (p = Pattern_Mask(_)) || (p = Pattern_All);
    --
    empty_set;
  }

  case mask {
    p = Pattern_Mask(_);
    --
    empty_set;
  }

  case tuple {
    p =: Pattern_Tuple(patterns);
    ids_sets := list_map(p1, patterns, use_pattern(p1));
    --
    union_list(ids_sets);
  }

  case any {
    p =: Pattern_Any(patterns);
    ids_sets := list_map(p1, patterns, use_pattern(p1));
    --
    union_list(ids_sets);
  }

  case single {
    p =: Pattern_Single(e);
    --
    use_expr(e);
  }

  case geq {
    p =: Pattern_Geq(e);
    --
    use_expr(e);
  }

  case leq {
    p =: Pattern_Leq(e);
    --
    use_expr(e);
  }

  case not {
    p =: Pattern_Not(p1);
    --
    use_pattern(p1);
  }

  case range {
    p =: Pattern_Range(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }
;

typing function use_slice(s: slice) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the slice
  {s} depends on.",
  prose_application = "",
} =
  case single {
    s =: Slice_Single(e);
    --
    use_expr(e);
  }

  case star {
    s =: Slice_Star(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }

  case length {
    s =: Slice_Length(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }

  case range {
    s =: Slice_Range(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }

  case typed_length {
    s =: typed_Slice_Length(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }
;

typing function use_bitfield(bf: bitfield) -> (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  bitfield {bf} depends on.",
  prose_application = "",
} =
  case simple {
    bf =: BitField_Simple(_, slices);
    INDEX(i, slices: use_slice(slices[i]) -> ids_slices[i]);
    --
    union_list(ids_slices);
  }

  case nested {
    bf =: BitField_Nested(_, slices, bitfields);
    ids_slices := list_map(slice, slices, use_slice(slice));
    ids_bitfields := list_map(bitfield, bitfields, use_bitfield(bitfield));
    --
    union(union_list(ids_slices), union_list(ids_bitfields));
  }

  case type {
    bf =: BitField_Type(_, slices, ty);
    ids_slices := list_map(slice, slices, use_slice(slice));
    --
    union(union_list(ids_slices), use_ty(ty));
  }
;

typing function use_constraint(c: int_constraint) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the integer
  constraint {c} depends on.",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    --
    use_expr(e);
  }

  case range {
    c =: Constraint_Range(e1, e2);
    ids := union(use_expr(e1), use_expr(e2));
    --
    ids;
  }
;

typing function use_ldi(l: local_decl_item) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  \localdeclarationitem{} {l} depends on.",
  prose_application = "",
} =
  case ldi_var {
    l =: LDI_Var(x);
    --
    make_set(Other(x));
  }

  case ldi_tuple {
    l =: LDI_Tuple(li);
    --
    set_from_list(x, li, Other(x));
  }
;

typing function use_stmt(s: stmt) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  statement {s} depends on.",
  prose_application = "",
  math_macro = \usestmt
} =
  case pass_return_none {
    s = S_Pass || s = S_Return(None);
    --
    empty_set;
  }

  case s_seq {
    s =: S_Seq(s1, s2);
    ids := union(use_stmt(s1), use_stmt(s2));
    --
    ids;
  }

  case s_assert {
    s =: S_Assert(e);
    --
    use_expr(e);
  }

  case s_return_some {
    s =: S_Return(some(e));
    --
    use_expr(e);
  }

  case s_assign {
    s =: S_Assign(le, e);
    ids := union(use_lexpr(le), use_expr(e));
    --
    ids;
  }

  case s_call {
    s =: S_Call(call);
    param_ids := list_map(p, call.params, use_expr(p));
    arg_ids := list_map(a, call.call_args, use_expr(a));
    --
    union(make_set(Subprogram(call.call_name)), union(union_list(param_ids), union_list(arg_ids)));
  }

  case s_cond {
    s =: S_Cond(e, s1, s2);
    --
    union(use_expr(e), union(use_stmt(s1), use_stmt(s2)));
  }

  case s_for {
    s =: S_For[index_name: index_name, start_e: start_e, dir: _, end_e: end_e, body: body, limit: limit];
    limit_ids := if limit =: some(e) then use_expr(e) else empty_set;
    --
    (union(
      limit_ids,
      make_set(Other(index_name)),
      use_expr(start_e),
      use_expr(end_e),
      use_stmt(body)
    ))
    { (_, ([_])) };
  }

  case s_while {
    s =: S_While(e, limit, s1);
    limit_ids := if limit =: some(e1) then use_expr(e1) else empty_set;
    --
    union(limit_ids, use_expr(e), use_stmt(s1));
  }

  case s_repeat {
    s =: S_Repeat(s1, e, limit);
    limit_ids := if limit =: some(e1) then use_expr(e1) else empty_set;
    --
    union(limit_ids, use_expr(e), use_stmt(s1));
  }

  case s_decl {
    s =: S_Decl(_, ldi, ty, e);
    ty_ids := if ty =: some(t) then use_ty(t) else empty_set;
    expr_ids := if e =: some(e1) then use_expr(e1) else empty_set;
    --
    union(use_ldi(ldi), union(ty_ids, expr_ids));
  }

  case s_throw {
    s =: S_Throw(e);
    --
    use_expr(e);
  }

  case s_try {
    s =: S_Try(s1, catchers, s2_opt);
    catcher_ids := list_map(c, catchers, use_catcher(c));
    s2_ids := if (s2_opt =: some(s2)) then use_stmt(s2) else empty_set;
    --
    union(use_stmt(s1), union_list(catcher_ids), s2_ids);
  }

  case s_print {
    s =: S_Print(args, _);
    arg_ids := list_map(a, args, use_expr(a));
    --
    union_list(arg_ids);
  }

  case s_unreachable {
    s = S_Unreachable;
    --
    empty_set;
  }
;

typing function use_catcher(c: catcher) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the try
  statement catcher {c} depends on.",
  prose_application = "",
} =
  c =: (_, ty, s);
  ids := union(use_ty(ty), use_stmt(s));
  --
  ids;
;

semantics relation eval_spec(tenv: static_envs, spec: spec) ->
         (v: native_value, g: XGraphs) | TDynError | TDiverging
{
   prose_description = "evaluates the specification {spec} with the
                        \staticenvironmentterm{} {tenv}, yielding the native
                        integer value {v} and execution graph {g}. Otherwise,
                        the result is a \dynamicerrorterm{}.",
 prose_application = "",
} =
  build_genv(tenv, spec) -> (env, g1);
  subprogram_for_signature(tenv, main, empty_list, ST_Function) -> (name', func_sig, _)
  { [_] };
  de_check( func_sig.return_type =: some(unconstrained_integer), DE_NEP) -> True
  { [_] };
  case normal {
    eval_subprogram(env, name', empty_list, empty_list) -> ResultCall((v_res, _), g2) | DynErrorConfig(), DivergingConfig();
    v_res =: match_singleton_list((v, _));
    g := ordered_po(g1, g2);
    --
    (v, g);
  }

  case throwing {
    eval_subprogram(env, name', empty_list, empty_list) -> Throwing(_, _, _, _)| DynErrorConfig(), DivergingConfig();
    --
    DynamicError(DE_UE);
  }
;

semantics relation build_genv(tenv: static_envs, typed_spec: spec) -> (new_env: envs, new_g: XGraphs) | TDynError | TDiverging
{
   prose_description = "populates {tenv} and output execution
                        graph {new_g} with the global storage declarations in
                        {typed_spec}, starting from the
                        \staticenvironmentterm{} {tenv}. This works by
                        traversing the global storage declarations and
                        updating the environment accordingly.
                        \ProseOtherwiseDynamicErrorOrDiverging",
 prose_application = "",
} =
  env := (tenv, empty_denv);
  eval_globals(typed_spec, (env, empty_graph)) -> (new_env, new_g) | DynErrorConfig(), DivergingConfig();
  --
  (new_env, new_g);
;

//////////////////////////////////////////////////
// Relations for Statements

typing relation annotate_stmt(tenv: static_envs, s: stmt) ->
         (new_s: stmt, new_tenv: static_envs, ses: powerset(TSideEffect)) | type_error
{
  "annotates a statement {s} in an environment {tenv},
  resulting in {new_s} --- the \typedast{} for {s}, which
  is also known as the \emph{annotated statement}, a
  modified environment {new_tenv}, and
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
}
=
  case SPass {
    s = S_Pass;
    --
    (S_Pass, tenv, empty_set);
  }

  case SAssign {
    s =: S_Assign(le, re);
    annotate_expr(tenv, re) -> (t_re, re1, ses_re);
    annotate_lexpr(tenv, le, t_re) -> (le1, ses_le);
    ses := union(ses_re, ses_le);
    --
    (S_Assign(le1, re1), tenv, ses);
  }

  case SDecl {
    case Some {
      s =: S_Decl(ldk, ldi, ty_opt, some(e));
      annotate_expr(tenv, e) -> (t_e, e', ses_e);
      annotate_local_decl_type_annot(tenv, ty_opt, t_e, ldk, (e', ses_e), ldi) -> (tenv1, ty_opt', ses_ldi)
      { math_layout = [_,_] };
      ses := union(ses_e, ses_ldi);
      new_s := S_Decl(ldk, ldi, ty_opt', some(e'));
      --
      (new_s, tenv1, ses);
    }

    case None {
      s =: S_Decl(LDK_Var, ldi, ty_opt, None);
      te_check(ty_opt =: some(t), TE_BD) -> True;
      annotate_type(False, tenv, t) -> (t', ses_t');
      base_value(tenv, t') -> e_init;
      annotate_local_decl_item(tenv, t', LDK_Var, None, ldi) -> new_tenv;
      new_s := S_Decl(LDK_Var, ldi, some(t'), some(e_init));
      --
      (new_s, new_tenv, ses_t');
    }
  }

  case SSeq {
    s =: S_Seq(s1, s2);
    annotate_stmt(tenv, s1) -> (new_s1, tenv1, ses1);
    annotate_stmt(tenv1, s2) -> (new_s2, new_tenv, ses2);
    ses := union(ses1, ses2);
    --
    (S_Seq(new_s1, new_s2), new_tenv, ses);
  }

  case SCall {
    s =: S_Call(call);
    annotate_call(tenv, call) -> (call', None, ses);
    --
    (S_Call(call'), tenv, ses);
  }

  case SCond {
    s =: S_Cond(e, s1, s2);
    annotate_expr(tenv, e) -> (t_cond, e_cond, ses_cond);
    check_type_satisfies(tenv, t_cond, T_Bool) -> True;
    annotate_block(tenv, s1) -> (s1', ses1);
    annotate_block(tenv, s2) -> (s2', ses2);
    ses := union(ses_cond, ses1, ses2);
    --
    (S_Cond(e_cond, s1', s2'), tenv, ses);
  }

  case SAssert {
    s =: S_Assert(e);
    annotate_expr(tenv, e) -> (t_e', e', ses_e);
    te_check( ses_is_readonly(ses_e), TE_SEV ) -> True;
    check_type_satisfies(tenv, t_e', T_Bool) -> True;
    ses := ses_e;
    --
    (S_Assert(e'), tenv, ses);
  }

  case SWhile {
    s =: S_While(e1, limit1, s1);
    annotate_expr(tenv, e1) -> (t, e2, ses_e);
    annotate_limit_expr(tenv, limit1) -> (limit2, ses_limit);
    check_type_satisfies(tenv, t, T_Bool) -> True;
    annotate_block(tenv, s1) -> (s2, ses_block);
    ses := union(ses_block, ses_e, ses_limit);
    --
    (S_While(e2, limit2, s2), tenv, ses);
  }

  case SRepeat {
    s =: S_Repeat(s1, e1, limit1);
    annotate_block(tenv, s1) -> (s2, ses_block);
    annotate_limit_expr(tenv, limit1) -> (limit2, ses_limit);
    annotate_expr(tenv, e1) -> (t, e2, ses_e);
    check_type_satisfies(tenv, t, T_Bool) -> True;
    ses := union(ses_block, ses_e, ses_limit);
    --
    (S_Repeat(s2, e2, limit2), tenv, ses);
  }

  case SFor {
    s =: S_For[
      index_name : index_name,
      start_e    : start_e,
      dir        : dir,
      end_e      : end_e,
      body       : body,
      limit      : limit
    ];
    annotate_expr(tenv, start_e) -> (start_t, start_e', ses_start);
    annotate_expr(tenv, end_e) -> (end_t, end_e', ses_end);
    annotate_limit_expr(tenv, limit) -> (limit', ses_limit);
    te_check(ses_is_readonly(ses_start), TE_SEV) -> True;
    te_check(ses_is_readonly(ses_end), TE_SEV) -> True;
    ses_cond := union(ses_start, ses_end, ses_limit);
    make_anonymous(tenv, start_t) -> start_struct;
    make_anonymous(tenv, end_t) -> end_struct;
    get_for_constraints(tenv, start_struct, end_struct, start_e', end_e', dir) -> cs
    { ([_,_,_,_,_,_], _) };
    ty := T_Int(cs);
    check_var_not_in_env(tenv, index_name) -> True;
    add_local(tenv, index_name, ty, LDK_Let) -> tenv';
    annotate_block(tenv', body) -> (body', ses_block);
    ses := union(ses_block, ses_cond);
    --
    (
      S_For[
      index_name : index_name,
      start_e    : start_e',
      dir        : dir,
      end_e      : end_e',
      body       : body',
      limit      : limit'
      ],
      tenv,
      ses
    ) { math_layout = (_,[_,_,_]) };
  }

  case SThrow {
    s =: S_Throw(e);
    annotate_expr(tenv, e) -> (t_e, e', ses1);
    check_structure_label(tenv, t_e, label_T_Exception) -> True;
    t_e =: T_Named(exn_name);
    ses := union(ses1, make_set(GlobalEffect(SE_Impure), LocalEffect(SE_Impure)));
    --
    (typed_S_Throw(e', t_e), tenv, ses);
  }

  // The implementation includes code for fine-grained side-effect analysis,
  // which is not part of the reference.
  // Specifically, there's no need to call `SES.remove_thrown_exceptions`
  // and thus ses3 and ses2 are equal.
  case STry {
    s =: S_Try(s', catchers, otherwise);
    annotate_block(tenv, s') -> (s'', ses1);
    ( INDEX(i, catchers : annotate_catcher(tenv, ses1, catchers[i]) -> (_, (catchers'[i], xs[i]))) )
    { math_layout =  ([_,_]) };
    ses_catchers := union_list(xs);
    case No_Otherwise {
      otherwise = None;
      otherwise' := None;
      ses_otherwise := empty_set;
    }

    case Otherwise {
      otherwise =: some(block);
      annotate_block(tenv, block) -> (block', ses_block);
      otherwise' := some(block');
      ses_otherwise := ses_block;
    }
    ses := union(ses1, ses_catchers, ses_otherwise);
    new_s := S_Try(s'', catchers', otherwise');
    --
    (new_s, tenv, ses);
  }

  case SReturn {
    case Error {
      s =: S_Return(e_opt);
      b := (tenv.static_envs_L.local_return_type = None) <=> (e_opt = None);
      b = False;
      --
      TypeError(TE_BSPD);
    }

    case None {
      s = S_Return(None);
      tenv.static_envs_L.local_return_type = None;
      --
      (S_Return(None), tenv, empty_set);
    }

    case Some {
      s =: S_Return(some(e));
      tenv.static_envs_L.local_return_type =: some(t);
      annotate_expr(tenv, e) -> (t_e', e', ses);
      check_type_satisfies(tenv, t_e', t) -> True;
      --
      (S_Return(some(e')), tenv, ses);
    }
  }

  case SPrint {
    s =: S_Print(args, newline);
    INDEX(i, args : annotate_expr(tenv, args[i]) -> (tys[i], args'[i], sess[i]));
    INDEX(i, args : is_singular(tenv, tys[i]) -> are_singular_arg_types[i]);
    te_check(list_and(are_singular_arg_types), TE_UT) -> True;
    ses := union(make_set(GlobalEffect(SE_Impure), LocalEffect(SE_Impure)), union_list(sess));
    --
    (S_Print(args', newline), tenv, ses);
  }

  case SUnreachable {
    s = S_Unreachable;
    --
    (S_Unreachable, tenv, empty_set);
  }

  case SPragma {
    s =: S_Pragma(id, args);
    INDEX(i, args : annotate_expr(tenv, args[i]) -> (_, _, sess[i]));
    ses := union_list(sess);
    --
    (S_Pass, tenv, ses);
  }
;

render rule annotate_stmt_SPass = annotate_stmt(SPass);
render rule annotate_stmt_SAssign = annotate_stmt(SAssign);
render rule annotate_stmt_SDecl = annotate_stmt(SDecl);
render rule annotate_stmt_SSeq = annotate_stmt(SSeq);
render rule annotate_stmt_SCall = annotate_stmt(SCall);
render rule annotate_stmt_SCond = annotate_stmt(SCond);
render rule annotate_stmt_SAssert = annotate_stmt(SAssert);
render rule annotate_stmt_SWhile = annotate_stmt(SWhile);
render rule annotate_stmt_SRepeat = annotate_stmt(SRepeat);
render rule annotate_stmt_SFor = annotate_stmt(SFor);
render rule annotate_stmt_SThrow = annotate_stmt(SThrow);
render rule annotate_stmt_STry = annotate_stmt(STry);
render rule annotate_stmt_SReturn = annotate_stmt(SReturn);
render rule annotate_stmt_SPrint = annotate_stmt(SPrint);
render rule annotate_stmt_SUnreachable = annotate_stmt(SUnreachable);
render rule annotate_stmt_SPragma = annotate_stmt(SPragma);

typing relation annotate_local_decl_type_annot(
    tenv: static_envs,
    ty_opt: option(ty),
    t_e: ty,
    ldk: local_decl_keyword,
    typed_e: (expr, powerset(TSideEffect)),
    ldi: local_decl_item) ->
         (new_tenv: static_envs, ty_opt': option(ty), ses: powerset(TSideEffect)) | type_error
{
  "annotates the type annotation {ty_opt} in the
  \staticenvironmentterm{} {tenv} within the context of
  a local declaration with keyword {ldk}, item {ldi},
  an initializing expression with its associated side effects {typed_e}, and type {t_e}.
  It yields the modified \staticenvironmentterm{}
  {new_tenv}, the annotated type annotation {ty_opt'},
  and the inferred \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [input[_,_,_,_,_,_], _],
} =
  case none {
    ty_opt = None;
    check_no_precision_loss(t_e) -> True;
    annotate_local_decl_item(tenv, t_e, ldk, some(typed_e), ldi) -> new_tenv;
    --
    (new_tenv, None, empty_set)
    { [_] };
  }

  case some {
    ty_opt =: some(t);
    get_structure(tenv, t_e) -> t_e';
    inherit_integer_constraints(t, t_e') -> t';
    annotate_type(False, tenv, t') -> (t'', ses_t);
    check_can_be_initialized_with(tenv, t'', t_e) -> True;
    annotate_local_decl_item(tenv, t'', ldk, some(typed_e), ldi) -> new_tenv;
    --
    (new_tenv, some(t''), ses_t)
    { [_] };
  }
;

typing function inherit_integer_constraints(lhs: ty, rhs: ty) ->
         (lhs': ty) | type_error
{
  "propagates integer constraints from the right-hand
  side type {rhs} to the left-hand side type annotation
  {lhs}. In particular, each occurrence of
  \pendingconstrainedintegertypeterm{} on the left-hand
  side should inherit constraints from a corresponding
  \wellconstrainedintegertypeterm{} on the right-hand
  side. If the corresponding right-hand side type is not
  a \wellconstrainedintegertypeterm{} (including if it
  is an \unconstrainedintegertypeterm{}), the result is
  a \typingerrorterm{}.",
  prose_application = "",
} =
  case int {
    lhs = T_Int(PendingConstrained);
    check_no_precision_loss(rhs) -> True;
    te_check( is_well_constrained_integer(rhs), TE_UT ) -> True;
    --
    rhs;
  }

  case tuple {
    lhs =: T_Tuple(lhs_tys);
    rhs =: T_Tuple(rhs_tys);
    te_check(same_length(lhs_tys, rhs_tys), TE_UT) -> True;
    ( INDEX(i, lhs_tys: inherit_integer_constraints(lhs_tys[i], rhs_tys[i]) -> lhs_tys'[i]) )
    { ([_]) };
    --
    T_Tuple(lhs_tys');
  }

  case other {
    or(
      (lhs != T_Int(PendingConstrained)),
      (ast_label(lhs) != label_T_Tuple),
      (ast_label(rhs) != label_T_Tuple)
    ) { [_] };
    --
    lhs;
  }
;

typing function check_no_precision_loss(t: ty) ->
         CheckResult | type_error
{
  "checks whether the type {t} is the result of a
  precision loss in its constraint computation (see for
  example \TypingRuleRef{ApplyBinopTypes}).",
  prose_application = "",
} =
  case well_constrained_typed {
    t =: T_Int(typed_WellConstrained(_, p));
    te_check(p = Precision_Full, TE_PLD) -> True;
    --
    True;
  }

  case integer {
    t =: T_Int(c);
    ast_label(c) != label_WellConstrained;
    --
    True;
  }

  case other {
    ast_label(t) != label_T_Int;
    --
    True;
  }
;

typing function check_can_be_initialized_with(tenv: static_envs, s: ty, t: ty) ->
         CheckResult | type_error
{
  "checks whether an expression of type {s} can be used
  to initialize a storage element of type {t} in the
  \staticenvironmentterm{} {tenv}. If the answer if
  $\False$, the result is a \typingerrorterm.",
  prose_application = "",
} =
  case okay {
    type_satisfies(tenv, t, s) -> True;
    --
    True;
  }

  case error {
    type_satisfies(tenv, t, s) -> False;
    --
    TypeError(TE_TSF);
  }
;

typing relation annotate_limit_expr(tenv: static_envs, e: option(expr)) ->
         (option(e': expr), ses: powerset(TSideEffect)) | type_error
{
  "annotates an optional expression {e} serving as the
  limit of a loop or a recursive subprogram in {tenv},
  yielding a pair consisting of an expression {e'} and a
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case none {
    e = None;
    --
    (None, empty_set);
  }

  case some {
    e =: some(limit);
    annotate_symbolic_constrained_integer(tenv, limit) -> (limit', ses);
    --
    (some(limit'), ses);
  }
;

typing relation get_for_constraints(
    tenv: static_envs,
    start_struct: ty,
    end_struct: ty,
    start_e': expr,
    end_e': expr,
    dir: for_direction) ->
         (vis: constraint_kind) | type_error
{
  "infers the integer constraints for a \texttt{for} loop
  index variable from the following:
  \begin{itemize}
  \item the \wellconstrainedversionterm{} of the type of
  the start expression {start_struct}
  \item the \wellconstrainedversionterm{} of the type of the end
  expression {end_struct}
  \item the annotated start expression {start_e'}
  \item the annotated end expression {end_e'}
  \item the loop direction {dir}
  \end{itemize} The result is the integer constraint {vis}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \getforconstraints,
  math_layout = [_,_],
} =
  case not_integers {
    ast_label(start_struct) != label_T_Int || ast_label(end_struct) != label_T_Int;
    --
    TypeError(TE_UT)
    { [_] };
  }

  case unconstrained {
    start_struct =: T_Int(c1);
    end_struct =: T_Int(c2);
    c1 = Unconstrained || c2 = Unconstrained;
    --
    Unconstrained
    { [_] };
  }

  case well_constrained {
    ast_label(start_struct) = label_T_Int;
    ast_label(end_struct) = label_T_Int;
    start_struct != unconstrained_integer;
    end_struct != unconstrained_integer;
    normalize(tenv, start_e') -> start_n;
    normalize(tenv, end_e') -> end_n;
    (e_bot, e_top) := if dir = UP then (start_n, end_n) else (end_n, start_n)
    { (_, [_]) };
    vis := WellConstrained(make_singleton_list(Constraint_Range(e_bot, e_top)));
    --
    vis
    { [_] };
  }
;

semantics relation eval_stmt(env: envs, s: stmt) ->
    | Continuing(new_g: XGraphs, new_env: envs)
    | TReturning
    | TThrowing
    | TDynError
    | TDiverging
{
   prose_description = "evaluates a statement {s} in an environment {env},
                        resulting in one of four types of \semanticsconfigurationsterm{}",
 prose_application = "",
 math_layout = (_, [_,_,_,_,_]),
} =
  case SPass {
    s = S_Pass;
    --
    Continuing(empty_graph, env);
  }

  case SAssignCall {
    s =: S_Assign(LE_Destructuring(les), E_Call(call));
    list_forall(arg, les, lexpr_is_var(arg));
    eval_call(env, call.call_name, call.params, call.call_args) -> (ms, env1)
    { math_layout = [_] };
    eval_multi_assignment(env1, les, ms) -> ResultLexpr(new_g, new_env);
    --
    Continuing(new_g, new_env);
  }

  case SAssign {
    s =: S_Assign(le, re);
    (or(
      ast_label(le) != label_LE_Destructuring,
      ast_label(re) != label_E_Call,
      (le =: LE_Destructuring(les)) &&
      list_exists(lexpr, les, not_single(lexpr_is_var(lexpr)))
    ))
    { math_layout = ( [_] ) };
    eval_expr(env, re) -> ResultExpr(vm, env1);
    eval_lexpr(env1, le, vm) -> ResultLexpr(new_g, new_env);
    --
    Continuing(new_g, new_env);
  }

  case SDecl {
    s =: S_Decl(_, ldi, _, some(e));
    eval_expr(env, e) -> ResultExpr(vm, env1);
    eval_local_decl(env1, ldi, vm) -> ResultLDI(new_g, new_env);
    --
    Continuing(new_g, new_env);
  }

  case SSeq {
    s =: S_Seq(s1, s2);
    eval_stmt(env, s1) -> Continuing(g1, env1);
    case continuing {
      eval_stmt(env1, s2) -> Continuing(g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_po(g1, g2);
      --
      Continuing(new_g, new_env);
    }
    case returning {
      eval_stmt(env1, s2) -> Returning((vs, g2), new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_po(g1, g2);
      --
      Returning((vs, new_g), new_env);
    }
    case throwing {
      eval_stmt(env1, s2) -> Throwing(v, t, g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_po(g1, g2);
      --
      Throwing(v, t, new_g, new_env);
    }
  }

  case SCall {
    s =: S_Call(call);
    eval_call(env, call.call_name, call.params, call.call_args) -> (_, new_env)
    { math_layout = [_] };
    --
    Continuing(empty_graph, new_env);
  }

  case SCond {
    s =: S_Cond(e, s1, s2);
    eval_expr(env, e) -> ResultExpr((v, g1), env1);
    v =: nvbool(b);
    s' := if b then s1 else s2;
    case continuing {
      eval_block(env1, s') -> Continuing(g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_ctrl(g1, g2);
      --
      Continuing(new_g, new_env);
    }
    case returning {
      eval_block(env1, s') -> Returning((vs, g2), new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_ctrl(g1, g2);
      --
      Returning((vs, new_g), new_env);
    }
    case throwing {
      eval_block(env1, s') -> Throwing(v', t, g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_ctrl(g1, g2);
      --
      Throwing(v', t, new_g, new_env);
    }
  }

  case SAssert {
    s =: S_Assert(e);
    eval_expr(env, e) -> ResultExpr((v, new_g), new_env);
    case okay {
      v =: nvbool(True);
      --
      Continuing(new_g, new_env);
    }
    case error {
      v =: nvbool(False);
      --
      DynamicError(DE_DAF);
    }
  }

  case SWhile {
    s =: S_While(e, e_limit_opt, body);
    eval_limit(env, e_limit_opt) -> (limit_opt, g1);
    case continuing {
      eval_loop(env, True, limit_opt, e, body) -> Continuing(g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_data(g1, g2);
      --
      Continuing(new_g, new_env);
    }
    case returning {
      eval_loop(env, True, limit_opt, e, body) -> Returning((vs, g2), new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_data(g1, g2);
      --
      Returning((vs, new_g), new_env);
    }
    case throwing {
      eval_loop(env, True, limit_opt, e, body) -> Throwing(v, t, g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_data(g1, g2);
      --
      Throwing(v, t, new_g, new_env);
    }
  }

  case SRepeat {
    s =: S_Repeat(body, e, e_limit_opt);
    eval_limit(env, e_limit_opt) -> (limit_opt1, g1);
    tick_loop_limit(limit_opt1) -> limit_opt2;
    case returning {
      eval_block(env, body) -> Returning((vs, g2), new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_data(g1, g2);
      --
      Returning((vs, new_g), new_env);
    }
    case throwing {
      eval_block(env, body) -> Throwing(v, t, g2, new_env) | DynErrorConfig(), DivergingConfig();
      new_g := ordered_data(g1, g2);
      --
      Throwing(v, t, new_g, new_env);
    }
    case continuing {
      eval_block(env, body) -> Continuing(g2, env1);
      case continuing {
        eval_loop(env1, False, limit_opt2, e, body) -> Continuing(g3, new_env) | DynErrorConfig(), DivergingConfig()
        { math_layout = [_] };
        new_g := ordered_data(g1, ordered_po(g2, g3));
        --
        Continuing(new_g, new_env);
      }
      case returning {
        eval_loop(env1, False, limit_opt2, e, body) -> Returning((vs, g3), new_env) | DynErrorConfig(), DivergingConfig()
        { math_layout = [_] };
        new_g := ordered_data(g1, ordered_po(g2, g3));
        --
        Returning((vs, new_g), new_env);
      }
      case throwing {
        eval_loop(env1, False, limit_opt2, e, body) -> Throwing(v, t, g3, new_env) | DynErrorConfig(), DivergingConfig()
        { math_layout = [_] };
        new_g := ordered_data(g1, ordered_po(g2, g3));
        --
        Throwing(v, t, new_g, new_env);
      }
    }
  }

  case SFor {
    s =: S_For [
      index_name: index_name,
      start_e:    e_start,
      dir:        dir,
      end_e:      e_end,
      body:       body,
      limit:      e_limit_opt
    ];
    eval_expr_sef(env, e_start) -> ResultExprSEF(start_v, g1);
    eval_expr_sef(env, e_end) -> ResultExprSEF(end_v, g2);
    eval_limit(env, e_limit_opt) -> (limit_opt, g3);
    declare_local_identifier(env, index_name, start_v) -> (env1, g4);
    eval_for(env1, index_name, limit_opt, start_v, dir, end_v, body) -> Continuing(g5, env2)
    { math_layout = [_] };
    remove_local(env2, index_name) -> env3;
    new_g := ordered_data((parallel(g1, g2, g3)), ordered_po(g4, g5));
    new_env := env3;
    --
    Continuing(new_g, new_env);
  }

  case SThrow {
    s =: typed_S_Throw(e, t);
    eval_expr(env, e) -> ResultExpr((v, g1), new_env);
    name := fresh_identifier();
    g2 := write_identifier(name, v);
    new_g := ordered_data(g1, g2);
    ex := (v, name);
    --
    Throwing(ex, t, new_g, new_env);
  }

  case STry {
    s =: S_Try(s1, catchers, otherwise_opt);
    case continuing {
      eval_block(env, s1) -> Continuing(g, new_env) | DynErrorConfig(), DivergingConfig();
      eval_catchers(env, catchers, otherwise_opt, Continuing(g, new_env)) -> C | ;
    }
    case returning {
      eval_block(env, s1) -> Returning((vs, g), new_env) | DynErrorConfig(), DivergingConfig();
      eval_catchers(env, catchers, otherwise_opt, Returning((vs, g), new_env)) -> C | ;
    }
    case throwing {
      eval_block(env, s1) -> Throwing(v, t, g, new_env) | DynErrorConfig(), DivergingConfig();
      eval_catchers(env, catchers, otherwise_opt, Throwing(v, t, g, new_env)) -> C | ;
    }
    --
    C;
  }

  case SReturn {
    case none {
      s = S_Return(None);
      --
      Returning((empty_list, empty_graph), env);
    }
    case one {
      s =: S_Return(some(e));
      eval_expr(env, e) -> ResultExpr((v, g1), new_env);
      wid := numbered_identifier(return_var_prefix, zero);
      write_identifier(wid, v) -> g2;
      new_g := ordered_data(g1, g2);
      --
      Returning((make_singleton_list(v), new_g), new_env);
    }
    case tuple {
      s =: S_Return(some(E_Tuple(es)));
      eval_expr_list_m(env, es) -> ResultExprListM(ms, new_env);
      write_folder(ms) -> (vs, new_g);
      --
      Returning((vs, new_g), new_env);
    }
  }

 case SPrint {
   case print {
     s =: S_Print(e_list, False);
     eval_expr_list(env, e_list) -> ResultExprList((v_list, g), envs[one]);
     INDEX(i, v_list: output_to_console(envs[i], v_list[i]) -> envs[i + one]);
     n := list_len(v_list);
     new_env := envs[n + one];
     --
     Continuing(g, new_env);
   }
   case println {
     s =: S_Print(e_list, True);
     eval_stmt(env, S_Print(e_list, False)) -> Continuing(g, env1);
     output_to_console(env1, nvstring(new_line)) -> new_env;
     --
     Continuing(g, new_env);
   }
 }

 case SUnreachable {
   s = S_Unreachable;
   --
   DynamicError(DE_UNR);
 }
;

render rule eval_stmt_SPass = eval_stmt(SPass);
render rule eval_stmt_SAssignCall = eval_stmt(SAssignCall);
render rule eval_stmt_SAssign = eval_stmt(SAssign);
render rule eval_stmt_SDecl = eval_stmt(SDecl);
render rule eval_stmt_SSeq = eval_stmt(SSeq);
render rule eval_stmt_SCall = eval_stmt(SCall);
render rule eval_stmt_SCond = eval_stmt(SCond);
render rule eval_stmt_SAssert = eval_stmt(SAssert);
render rule eval_stmt_SWhile = eval_stmt(SWhile);
render rule eval_stmt_SRepeat = eval_stmt(SRepeat);
render rule eval_stmt_SFor = eval_stmt(SFor);
render rule eval_stmt_SThrow = eval_stmt(SThrow);
render rule eval_stmt_STry = eval_stmt(STry);
render rule eval_stmt_SReturn = eval_stmt(SReturn);
render rule eval_stmt_SPrint = eval_stmt(SPrint);
render rule eval_stmt_SUnreachable = eval_stmt(SUnreachable);

semantics function output_to_console(env: envs, v: native_value) -> (new_env: envs)
{
  "communicates {v} to a console, where one exists, possibly updating the environment {env}, yielding {new_env}.",
  prose_application = "communicating {v} to the console in the context of {env} yields {new_env}",
} =
  --
  bot; // This rule is expressed directly in LaTeX as it needs to re-define environments.
;

semantics function literal_to_string(l: literal) -> (s: Strings)
{
  "converts a literal {l} to a printable string {s}",
  prose_application = "converting {l} to a printable string yields {s}",
} =
  --
  bot; // This function is defined by a table in LaTeX.
;

semantics function lexpr_is_var(le: lexpr) -> (res: Bool)
 {
    "tests whether {le} is an assignable variable expression or a discarded \assignableexpression{}, yielding the result in {res}",
    prose_application = "testsing whether {le} is an assignable variable expression or a discarded \assignableexpression{} yields {res}",
 } =
  --
  ast_label(le) in make_set(label_LE_Discard, label_LE_Var);
;

semantics relation eval_for(
  env: envs,
  index_name: Identifier,
  limit_opt: option(N),
  v_start: native_value,
  dir: for_direction,
  v_end: native_value,
  body: stmt) -> TContinuing | TReturning | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates the \texttt{for} loop with the index
                        variable {index_name}, optional limit value\\
                        {limit_opt}, starting from the value {v_start} going
                        in the direction given by {dir} until the value given
                        by {v_end}, executing {body} on each iteration. % The
                        evaluation utilizes two helper relations:
                        $\evalforstep$ and $\evalforloop$.",
 prose_application = "",
  math_layout = [_,_],
} =
  comp := if dir = UP then LT else GT;
  v_start =: NV_Literal(L_Int(sv));
  read_identifier(index_name, v_start) -> g1;
  eval_binop(comp, v_end, v_start) -> v_cmp;
  case return {
    v_cmp =: nvbool(True);
    --
    Continuing(g1, env)
    { [_] };
  }

  case continue {
    v_cmp =: nvbool(False);
    tick_loop_limit(limit_opt) -> limit_opt1;
    eval_for_loop(env, index_name, limit_opt1, v_start, dir, v_end, body) ->
      Continuing(g2, new_env)
    { [_] };
    new_g := ordered_ctrl(g1, g2);
    --
    Continuing(new_g, new_env)
    { [_] };
  }
;

semantics relation eval_for_step(
    env: envs,
    index_name: Identifier,
    limit_opt: option(N),
    v_start: native_value,
    dir: for_direction) ->
         ((v_step: native_value, new_env: envs), new_g: XGraphs) |
         TReturning |
         TThrowing |
         TDynError |
         TDiverging
{
  "either increments or decrements the index variable,
  returning the new value of the index variable, the
  modified environment, and the resulting execution
  graph. \ProseOtherwiseReturningOrAbnormal",
  prose_application = "",
  math_layout = [_,_],
} =
  op := if dir = UP then ADD else SUB;
  read_identifier(index_name, v_start) -> g1;
  eval_binop(op, v_start, nvint(one)) -> v_step;
  write_identifier(index_name, v_step) -> g2;
  env =: (tenv, denv);
  updated_local := map_update(denv.dynamic_envs_L, index_name, v_step);
  new_denv := denv(dynamic_envs_L: updated_local);
  new_env := (tenv, new_denv);
  new_g := ordered_data(g1, g2);
  --
  ((v_step, new_env), new_g)
  { [_] };
;

semantics relation eval_for_loop(
    env: envs,
    index_name: Identifier,
    limit_opt: option(N),
    v_start: native_value,
    dir: for_direction,
    v_end: native_value,
    body: stmt) ->
         Continuing(new_g: XGraphs, new_env: envs) | TReturning | TThrowing | TDynError | TDiverging
{
  "executes one iteration of the loop body and then uses
  $\texttt{eval\_for}$ to execute the remaining
  iterations.",
  prose_application = "",
  math_layout = [_,_],
} =
  eval_block(env, body) -> Continuing(g1, env1);
  eval_for_step(env1, index_name, limit_opt, v_start, dir) -> ((v_step, env2), g2)
  { [_] };
  eval_for(env2, index_name, limit_opt, v_step, dir, v_end, body) ->
    Continuing(g3, new_env)
  { [_] };
  new_g := ordered_po(ordered_po(g1, g2), g3);
  --
  Continuing(new_g, new_env)
  { [_] };
;

semantics relation eval_loop(env: envs, is_while: Bool, limit_opt: option(N), e_cond: expr, body: stmt) ->
  | Continuing(new_g: XGraphs, new_env: envs)
  | TReturning
  | TThrowing
  | TDynError
  | TDiverging
{
   prose_description = "to evaluate both \texttt{while} statements and
                        \texttt{repeat} statements.",
 prose_application = "",
  math_layout = [_,_],
} =
  case exit {
    eval_expr(env, e_cond) -> ResultExpr((v_cond, g_cond), new_env);
    v_cond =: nvbool(b);
    b != is_while;
    --
    Continuing(g_cond, new_env);
  }

  case continue {
    eval_expr(env, e_cond) -> ResultExpr((v_cond, g1), env1);
    v_cond =: nvbool(b);
    b = is_while;
    tick_loop_limit(limit_opt) -> limit_opt';
    eval_block(env1, body) -> Continuing(g2, env2);
    eval_loop(env2, is_while, limit_opt', e_cond, body) -> Continuing(g3, new_env)
    { [_] };
    new_g := ordered_po(ordered_ctrl(g1, g2), g3);
    --
    Continuing(new_g, new_env);
  }
;

semantics relation eval_limit(env: envs, e_limit_opt: option(expr)) -> (v_opt: option(N), g: XGraphs) | TDynError | TDiverging
{
   prose_description = "evaluates the optional expression {e_limit_opt} in
                        the environment {env}, yielding the optional integer
                        value {v_opt} and execution graph {g}.
                        \ProseOtherwiseDynamicErrorOrDiverging",
 prose_application = ""
} =
  case none {
    e_limit_opt = None;
    v_opt := None;
    g := empty_graph;
    --
    (v_opt, g);
  }

  case some {
    e_limit_opt =: some(e_limit);
    eval_expr_sef(env, e_limit) -> ResultExprSEF(nvint(n), g);
    --
    (some(n), g);
  }
;

semantics relation tick_loop_limit(v_opt: option(N)) -> (v_opt': option(N)) | TDynError
{
   prose_description = "decrements the optional integer {v_opt}, yielding the
                        optional integer value {v_opt}. If the value is $0$,
                        the result is a \DynamicErrorConfigurationTerm{}.",
 prose_application = ""
} =
  case none {
    v_opt = None;
    --
    None;
  }

  case some_ok {
    v_opt =: some(v);
    v > zero;
    --
    some(v - one);
  }

  case some_error {
    v_opt =: some(zero);
    --
    DynamicError(DE_LE);
  }
;

semantics relation eval_expr_list_m(env: envs, es: list0(expr)) ->
  | ResultExprListM(vms: list0((native_value, XGraphs)), new_env: envs)
  | TThrowing
  | TDynError
  | TDiverging
{
  "evaluates a list of expressions {es} in left-to-right
  in the initial environment {env} and returns the list
  of values associated with graphs {vms} and the new
  environment {new_env}. If the evaluation of any
  expression terminates abnormally then the abnormal
  configuration is returned.",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    es = empty_list;
    --
    ResultExprListM(empty_list, env);
  }

  case non_empty {
    es =: match_cons(e, es1);
    eval_expr(env, e) -> ResultExpr(m, env1);
    eval_expr_list_m(env1, es1) -> ResultExprListM(vms1, new_env);
    --
    ResultExprListM(cons(m, vms1), new_env);
  }
;

semantics relation write_folder(vms: list0((native_value, XGraphs))) ->
         (vs: list0(native_value), new_g: XGraphs)
{
  "concatenates the input values in {vms} and generates
  an execution graph by composing the graphs in {vms}
  with Write Effects for the respective values.",
  prose_application = "",
} =
  case empty {
    vms = empty_list;
    --
    (empty_list, empty_graph);
  }

  case non_empty {
    vms =: match_cons(vm, vms1);
    vm =: (v, g);
    id := fresh_identifier();
    write_identifier(id, v) -> g1;
    write_folder(vms1) -> (vs1, g2);
    vs := match_cons(v, vs1);
    new_g := ordered_po(g, ordered_data(g1, g2));
    --
    (vs, new_g);
  }
;

//////////////////////////////////////////////////
// Relations for Static Evaluation

typing function static_eval(tenv: static_envs, e: expr) -> (v: literal) | type_error
{
  "evaluates an expression {e}
  in the \staticenvironmentterm{} {tenv}, returning a literal {v}.
  If the evaluation terminates by a thrown exception of a value that is not a literal
  (for example, a record value), the result is a \typingerrorterm{}.",
  prose_application = "statically evaluating {e} in {tenv} yields {v}\ProseOrTypeError",
} =
  case normal_literal {
    static_env_to_env(tenv) -> env;
    eval_expr(env, e) -> ResultExpr((NV_Literal(v), _), _) | ; // No short-circuiting expressions
    --
    v;
  }

  case normal_non_literal {
    static_env_to_env(tenv) -> env;
    eval_expr(env, e) -> ResultExpr((x, _), _) | ; // No short-circuiting expressions
    not(x = NV_Literal(_));
    --
    TypeError(TE_SEF);
  }

  case abnormal_throwing {
    static_env_to_env(tenv) -> env;
    eval_expr(env, e) -> Throwing(_, _, _, _) | ;
    --
    TypeError(TE_SEF);
  }

  case abnormal_dynamic_error {
    static_env_to_env(tenv) -> env;
    eval_expr(env, e) -> DynamicError(_) | ;
    --
    TypeError(TE_SEF);
  }

  // TODO: handling the diverging case requires either adding a Diverging
  // configuration as a short-circuit to most typechecking relations
  // or avoiding divergence in static evaluation.
;

semantics function static_env_to_env(tenv: static_envs) ->
         (env: envs)
{
  "transforms the constants defined in the
  \staticenvironmentterm{} {tenv} into an environment
  {env}.",
  prose_application = "",
};


//////////////////////////////////////////////////
// Relations for Subprogram Calls

typing relation annotate_call(tenv: static_envs, call: call) ->
         (call': call, ret_ty_opt: option(ty), ses: powerset(TSideEffect))
         | type_error
{
  "annotates the call {call} to a subprogram with call
    type $\calltype$, resulting in the following:
    \begin{itemize}
    \item the updated call {call'},
        with all arguments/parameters annotated and \\
        $\vcall.\callname$ updated to uniquely identify the
        call among the set of overloading subprograms declared
        with the same name;
    \item the \optionalterm{} annotated return type {ret_ty_opt};
    \item the \sideeffectsetterm{} inferred for {call}, {ses}.
    \end{itemize}
    \ProseOtherwiseTypeError",
  prose_application = "",
} =
  annotate_exprs(tenv, call.call_args) -> typed_args;
  annotate_exprs(tenv, call.params) -> typed_params;
  annotate_call_actuals_typed(tenv, call.call_name, typed_params, typed_args, call.call_type) ->
    (call', ret_ty_opt, ses_call)
  { ([_], [_]) };
  typed_args =: list_combine_three(arg_tys, arg_exprs, arg_ses);
  ses_args := union_list(arg_ses);
  ses := union(ses_args, ses_call);
  --
  (call', ret_ty_opt, ses);
;

typing relation annotate_call_actuals_typed(
  tenv: static_envs,
  name: Identifier,
  params: list0((ty, expr, powerset(TSideEffect))),
  typed_args: list0((ty, expr, powerset(TSideEffect))),
  call_type: subprogram_type) ->
  (call: call, ret_ty_opt: option(ty), ses: powerset(TSideEffect)) | type_error
{
  "is similar to $\annotatecall$, except that it accepts
  the annotated versions of the parameter and argument
  expressions as inputs, that is, tuples consisting of
  types, annotated expressions, and
  \sideeffectdescriptorsetsterm.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [input[_,_,_,_,_], _],
} =
  typed_args =: list_combine_three(arg_types, args, sess_args);
  ses_args := union_list(sess_args);
  subprogram_for_signature(tenv, name, arg_types, call_type) -> (name', func_sig, ses_call)
  { [_] };
  ses := union(ses_args, ses_call);
  insert_stdlib_param(tenv, func_sig, params, arg_types) -> params1;
  te_check(same_length(func_sig.parameters, params1), TE_BC) -> True;
  te_check(same_length(func_sig.args, args), TE_BC) -> True;
  check_params_typesat(tenv, func_sig.parameters, params1) -> True;
  func_sig.parameters =: list_combine(param_names, _);
  params1 =: list_combine_three(_, param_exprs, _);
  eqs := list_combine(param_names, param_exprs);
  check_args_typesat(tenv, func_sig.args, arg_types, eqs) -> True;
  annotate_ret_ty(tenv, call_type, func_sig.return_type, eqs) -> ret_ty_opt;
  call := [
    call_name: name',
    params: param_exprs,
    call_args: args,
    call_type: func_sig.func_subprogram_type
  ];
  --
  (call, ret_ty_opt, ses)
  { [_] };
;

typing function insert_stdlib_param(
  tenv: static_envs,
  func_sig: func,
  params: list0((ty, expr, powerset(TSideEffect))),
  arg_types: list0(ty)) ->
         (params1: list0((ty, expr, powerset(TSideEffect))))
{
  "inserts the (optionally) omitted input parameter of a
  standard library function call.",
  prose_application = "",
  math_layout = ([_,_,(_,_,_),_],[_])
} =
  case can_insert {
    can_insert_stdlib_param := ( and(
        can_omit_stdlib_param(func_sig),
        list_len(params) < list_len(func_sig.parameters),
        arg_types != empty_list
    ) ) { (_, ([_])) };
    can_insert_stdlib_param = True;
    arg_types =: match_cons(t, _);
    get_bitvector_width(tenv, t) -> width;
    param_type := T_Int(WellConstrained(make_singleton_list(AbbrevConstraintExact(width))));
    params1 := concat(params, make_singleton_list((param_type, width, empty_set)));
    --
    params1;
  }

  case cannot_insert {
    can_insert_stdlib_param := ( and(
        can_omit_stdlib_param(func_sig),
        list_len(params) < list_len(func_sig.parameters),
        arg_types != empty_list
    ) ) { (_, ([_])) };
    can_insert_stdlib_param = False;
    --
    params;
  }
;

typing function can_omit_stdlib_param(func_sig: func) ->
         (b: Bool)
{
  "tests whether the first parameter of the subprogram
  defined by {func_sig} can be omitted (and thus
  automatically inserted), yielding the result in {b}.",
  prose_application = "",
} =
  case builtin_one_param {
    func_sig.builtin;
    func_sig.parameters =: match_singleton_list((n_param, _));
    func_sig.args =: match_cons((_, T_Bits(E_Var(n_arg), _)), _);
    bool_transition(n_param = n_arg) -> True;
    --
    True;
  }

  case builtin_two_params {
    func_sig.builtin;
    func_sig.parameters =: match_cons(_, match_cons((n_param, _), empty_list));
    func_sig.args =: match_cons((_, T_Bits(E_Var(n_arg), _)), _);
    bool_transition(n_param = n_arg) -> True;
    --
    True;
  }

  case builtin_other {
    func_sig.builtin;
    --
    False;
  }

  case not_builtin {
    not_single(func_sig.builtin);
    --
    False;
  }
;

typing function check_params_typesat(tenv: static_envs, func_sig_params: list0((Identifier, option(ty))), params: list0((ty, expr, powerset(TSideEffect)))) ->
         CheckResult | type_error
{
  "checks that annotated parameters {params} are correct
  with respect to the declared parameters
  {func_sig_params}. \ProseOtherwiseTypeError{} It
  assumes that {func_sig_params} and {params} have the
  same length.",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    func_sig_params = empty_list;
    --
    True;
  }

  case parameterized {
    func_sig_params =: match_cons((name, ty_decl_opt), func_sig_params1);
    params =: match_cons((ty_actual, _, ses_actual), params1);
    check_symbolically_evaluable(ses_actual) -> True;
    check_constrained_integer(tenv, ty_actual) -> True;
    ty_decl_opt =: some(T_Int(Parameterized(name')));
    name = name';
    check_params_typesat(tenv, func_sig_params1, params1) -> True;
    --
    True;
  }

  case other {
    func_sig_params =: match_cons((name', ty_decl_opt), func_sig_params1);
    params =: match_cons((ty_actual, _, ses_actual), params1);
    check_symbolically_evaluable(ses_actual) -> True;
    check_constrained_integer(tenv, ty_actual) -> True;
    ty_decl_opt =: some(ty_decl);
    ty_decl != T_Int(Parameterized(name'));
    check_type_satisfies(tenv, ty_actual, ty_decl) -> True;
    check_params_typesat(tenv, func_sig_params1, params1) -> True;
    --
    True;
  }
;

typing relation rename_ty_eqs(tenv: static_envs, eqs: list0((Identifier, expr)), ty: ty) ->
         (new_ty: ty) | type_error
{
  "transforms the type {ty} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {eqs}, yielding the type {new_ty}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case t_bits {
    ty =: T_Bits(e, fields);
    subst_expr_normalize(tenv, eqs, e) -> new_e;
    --
    T_Bits(new_e, fields);
  }

  case t_int_wellconstrained {
    ty =: T_Int(WellConstrained(constraints));
    ( INDEX(i, constraints: subst_constraint(tenv, eqs, constraints[i]) -> new_constraints[i]) )
    { ([_]) };
    new_constraints1 := match_non_empty_list(new_constraints);
    --
    T_Int(WellConstrained(new_constraints1));
  }

  case t_int_parameterized {
    ty =: T_Int(Parameterized(name));
    subst_expr_normalize(tenv, eqs, E_Var(name)) -> e;
    --
    T_Int(WellConstrained(make_singleton_list(AbbrevConstraintExact(e))));
  }

  case t_tuple {
    ty =: T_Tuple(tys);
    INDEX(i, tys: rename_ty_eqs(tenv, eqs, tys[i]) -> new_tys[i]);
    --
    T_Tuple(new_tys);
  }

  case other {
    ast_label(ty) not_in make_set(label_T_Bits, label_T_Int, label_T_Tuple);
    --
    ty;
  }
;

typing function subst_expr_normalize(tenv: static_envs, eqs: list0((Identifier, expr)), e: expr) ->
         (new_e: expr)
{
  "transforms the expression {e} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {eqs}, and then attempting to symbolically simplify
  the result, yielding the expression {new_e}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  subst_expr(tenv, eqs, e) -> e1;
  normalize(tenv, e1) -> new_e;
  --
  new_e;
;

typing function subst_expr(tenv: static_envs, substs: list0((Identifier, expr)), e: expr) ->
         (new_e: expr)
{
  "transforms the expression {e} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {substs}, yielding the expression {new_e}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case e_var_in_substs {
    e =: E_Var(s);
    assoc_opt(substs, s) =: some(new_e);
    --
    new_e;
  }

  case e_var_not_in_substs {
    e =: E_Var(s);
    assoc_opt(substs, s) = None;
    --
    e;
  }

  case e_unop {
    e =: E_Unop(op, e1);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_Unop(op, e1');
  }

  case e_binop {
    e =: E_Binop(op, e1, e2);
    subst_expr(tenv, substs, e1) -> e1';
    subst_expr(tenv, substs, e2) -> e2';
    --
    E_Binop(op, e1', e2');
  }

  case e_cond {
    e =: E_Cond(e1, e2, e3);
    subst_expr(tenv, substs, e1) -> e1';
    subst_expr(tenv, substs, e2) -> e2';
    subst_expr(tenv, substs, e3) -> e3';
    --
    E_Cond(e1', e2', e3');
  }

  case e_call {
    e =: E_Call(call);
    args := call.call_args;
    INDEX(i, args: subst_expr(tenv, substs, args[i]) -> args'[i]);
    params := call.params;
    INDEX(i, params: subst_expr(tenv, substs, params[i]) -> params'[i]);
    call' := call(call_args: args', params: params');
    --
    E_Call(call');
  }

  case e_getarray {
    e =: E_GetArray(e1, e2);
    subst_expr(tenv, substs, e1) -> e1';
    subst_expr(tenv, substs, e2) -> e2';
    --
    E_GetArray(e1', e2');
  }

  case e_getenumarray {
    e =: E_GetEnumArray(e1, e2);
    subst_expr(tenv, substs, e1) -> e1';
    subst_expr(tenv, substs, e2) -> e2';
    --
    E_GetEnumArray(e1', e2');
  }

  case e_getfield {
    e =: E_GetField(e1, field);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_GetField(e1', field);
  }

  case e_getfields {
    e =: E_GetFields(e1, fields);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_GetFields(e1', fields);
  }

  case e_getitem {
    e =: E_GetItem(e1, i);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_GetItem(e1', i);
  }

  case e_pattern {
    e =: E_Pattern(e1, pat);
    subst_expr(tenv, substs, e1) -> e1';
    // TODO: shouldn't we also transform pat?
    --
    E_Pattern(e1', pat);
  }

  case e_record {
    e =: E_Record(ty, fields);
    fields =: list_combine(field_names, field_exprs);
    ( INDEX(i, field_exprs: subst_expr(tenv, substs, field_exprs[i]) -> field_exprs'[i]) )
    { ([_]) } ;
    fields' := list_combine(field_names, field_exprs');
    --
    E_Record(ty, fields');
  }

  case e_slice {
    e =: E_Slice(e1, slices);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_Slice(e1', slices);
  }

  case e_tuple {
    e =: E_Tuple(es);
    INDEX(i, es: subst_expr(tenv, substs, es[i]) -> es'[i]);
    --
    E_Tuple(match_non_empty_list(es'));
  }

  case e_array {
    e =: E_Array[length: length, array_value: value];
    subst_expr(tenv, substs, length) -> length';
    subst_expr(tenv, substs, value) -> value';
    --
    E_Array[length: length', array_value: value'];
  }

  case e_enumarray {
    e =: E_EnumArray[enum: enum_id, labels: labels, enum_array_value: value];
    subst_expr(tenv, substs, value) -> value';
    --
    E_EnumArray[enum: enum_id, labels: labels, enum_array_value: value'];
  }

  case e_atc {
    e =: E_ATC(e1, t);
    subst_expr(tenv, substs, e1) -> e1';
    --
    E_ATC(e1', t);
  }

  case e_literal_arbitrary {
    ast_label(e) in make_set (label_E_Literal, label_E_Arbitrary);
    --
    e;
  }
;

typing function subst_constraint(tenv: static_envs, eqs: list0((Identifier, expr)), c: int_constraint) ->
         (new_c: int_constraint)
{
  "transforms the integer constraint {c} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {eqs}, and then attempting to symbolically simplify
  the result, yielding the integer constraint {new_c}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    subst_expr_normalize(tenv, eqs, e) -> new_e;
    --
    Constraint_Exact(new_e);
  }

  case range {
    c =: Constraint_Range(e1, e2);
    subst_expr_normalize(tenv, eqs, e1) -> e1';
    subst_expr_normalize(tenv, eqs, e2) -> e2';
    --
    Constraint_Range(e1', e2');
  }
;

typing function check_args_typesat(tenv: static_envs, func_sig_args: list0((Identifier, ty)), arg_types: list0(ty), eqs: list0((Identifier, expr))) ->
         CheckResult | type_error
{
  "checks that the types {arg_types} \typesatisfyterm\
  the types of the corresponding formal arguments
  {func_sig_args} with the parameters substituted with
  their corresponding arguments as per {eqs} and results
  in a \typingerrorterm{} otherwise.",
  prose_application = "",
} =
  case empty {
    func_sig_args = empty_list;
    arg_types = empty_list;
    --
    True;
  }

  case non_empty {
    func_sig_args =: match_cons((_, ty_decl), func_sig_args1);
    arg_types =: match_cons(ty_actual, arg_types1);
    rename_ty_eqs(tenv, eqs, ty_decl) -> ty_decl1;
    check_type_satisfies(tenv, ty_actual, ty_decl1) -> True;
    check_args_typesat(tenv, func_sig_args1, arg_types1, eqs) -> True;
    --
    True;
  }
;

typing relation annotate_ret_ty(tenv: static_envs, call_type: subprogram_type, func_sig_ret_ty_opt: option(ty), eqs: list0((Identifier, expr))) ->
         (ret_ty_opt: option(ty)) | type_error
{
  "annotates the \optionalterm{} return type
  {func_sig_ret_ty_opt} given with the subprogram type
  {call_type} with respect to the parameter expressions
  {eqs}, yielding the \optionalterm{} annotated type
  {ret_ty_opt}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case function_or_getter {
    call_type in make_set(ST_Function, ST_Getter);
    func_sig_ret_ty_opt =: some(ty);
    rename_ty_eqs(tenv, eqs, ty) -> ty1;
    --
    some(ty1);
  }

  case procedure_or_setter {
    call_type in make_set(ST_Procedure, ST_Setter);
    func_sig_ret_ty_opt = None;
    --
    None;
  }

  case ret_type_mismatch {
    not((call_type in make_set(ST_Procedure, ST_Setter)) <=> (func_sig_ret_ty_opt = None))
    { [_] };
    --
    TypeError(TE_BC)
    { [_] };
  }
;

typing function subprogram_for_signature(tenv: static_envs, name: Strings, caller_arg_types: list0(ty), call_type: subprogram_type) ->
         (name': Strings, callee: func, ses: powerset(TSideEffect)) | type_error
{
  "looks up the \staticenvironmentterm{} {tenv} for a
  subprogram associated with {name}, the list of
  argument types {caller_arg_types}, and a subprogram
  type matching {call_type}.",
  prose_application = "",
  math_layout = [_,_],
} =
  case undefined {
    tenv.static_envs_G.overloaded_subprograms(name) = bot;
    --
    TypeError(TE_UI)
    { [_] };
  }

  case no_candidates {
    tenv.static_envs_G.overloaded_subprograms(name) =: candidates;
    filter_call_candidates(tenv, caller_arg_types, call_type, candidates) -> matches;
    matches = empty_list;
    --
    TypeError(TE_BC)
    { [_] };
  }

  case one_candidate {
    tenv.static_envs_G.overloaded_subprograms(name) =: candidates;
    filter_call_candidates(tenv, caller_arg_types, call_type, candidates) -> matches;
    matches =: match_singleton_list((name', callee));
    tenv.static_envs_G.subprogram(name') =: (_, ses);
    --
    (name', callee, ses)
    { [_] };
  }
;

typing function filter_call_candidates(tenv: static_envs, formal_types: list0(ty), call_type: subprogram_type, candidates: powerset(Strings)) ->
         (matches: list0((Strings, func)))
{
  "iterates over the set of subprogram names in
  {candidates} and checks whether their lists of
  arguments clash with the types in {formal_types} and
  their subprogram types match {call_type} in {tenv}.
  The result is the set of pairs consisting of the names
  and function definitions of the subprograms whose
  arguments clash and subprogram types match in
  {candidates}. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case no_candidates {
    candidates = empty_set;
    --
    empty_list;
  }

  case candidates_exist {
    candidates =: disjoint_union(match_set(name), remaining_candidates);
    tenv.static_envs_G.subprogram(name) =: (func_def, _);
    has_arg_clash(tenv, formal_types, func_def.args) -> b1;
    call_type_matches(func_def, call_type) -> b2;
    filter_call_candidates(tenv, formal_types, call_type, remaining_candidates) -> matches1
    { ([_], _) };
    matches := if_then_else(b1 && b2, concat(make_singleton_list((name, func_def)), matches1), matches1);
    --
    matches;
  }
;

typing function call_type_matches(func: func, call_type: subprogram_type) ->
         (b: Bool)
{
  "checks whether a function definition {func} is
  compatible with the subprogram type expected by a
  function call, {call_type}, yielding the result in
  {b}.",
  prose_application = "",
} =
  case getter_func {
    func.func_subprogram_type = ST_Getter;
    call_type = ST_Function;
    --
    True;
  }

  case equal {
    (func.func_subprogram_type != ST_Getter) || (call_type != ST_Function);
    --
    func.func_subprogram_type = call_type;
  }
;

typing function has_arg_clash(tenv: static_envs, f_tys: list0(ty), args: list0((Identifier, ty))) ->
         (b: Bool) | type_error
{
  "checks whether a list of types {f_tys} clashes with
  the list of types appearing in the list of arguments
  {args} in {tenv}, yielding the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  bool_transition(same_length(f_tys, args)) -> True | False;
  args =: list_combine(arg_names, arg_tys);
  INDEX(i, f_tys: type_clashes(tenv, f_tys[i], arg_tys[i]) -> clashes[i]);
  --
  list_or(clashes);
;

typing function type_clashes(tenv: static_envs, t: ty, s: ty) ->
         (b: Bool) | type_error
{
  "determines whether a type {t} \emph{\Prosetypeclashes}
  with a type {s} in environment {tenv}, returning the
  result {b}. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case subtype {
    is_subtype(tenv, s, t) || is_subtype(tenv, t, s);
    --
    True;
  }

  case no_subtype {
    is_subtype(tenv, s, t) -> False;
    is_subtype(tenv, t, s) -> False;
    get_structure(tenv, t) -> t_struct;
    get_structure(tenv, s) -> s_struct;
    case simple {
      ast_label(t_struct) = ast_label(s_struct);
      ast_label(t_struct) in make_set(label_T_Bits, label_T_Bool, label_T_Int, label_T_Real, label_T_String);
      --
      True;
    }

    case t_enum {
      t_struct =: T_Enum(labels_t);
      s_struct =: T_Enum(labels_s);
      --
      labels_t = labels_s;
    }

    case t_array {
      t_struct =: T_Array(_, ty_t);
      s_struct =: T_Array(_, ty_s);
      type_clashes(tenv, ty_t, ty_s) -> b;
      --
      b;
    }

    case t_tuple {
      t_struct =: T_Tuple(ts_t);
      s_struct =: T_Tuple(ts_s);
      bool_transition(same_length(ts_t, ts_s)) -> True | False;
      INDEX(i, ts_t: type_clashes(tenv, ts_t[i], ts_s[i]) -> clashes[i]);
      --
      list_and(clashes);
    }

    case otherwise_different_labels {
      ast_label(t_struct) != ast_label(s_struct);
      --
      False;
    }

    case otherwise_structured {
      ast_label(t_struct) = ast_label(s_struct);
      ast_label(t_struct) in make_set(label_T_Collection, label_T_Exception, label_T_Record);
      --
      False;
    }
  }
;

typing relation annotate_exprs(tenv: static_envs, exprs: list0(expr)) ->
         (typed_exprs: list0((ty, expr, powerset(TSideEffect)))) | type_error
{
  "annotates a list of expressions {exprs} from left to
  right, yielding a list of tuples \\ {typed_exprs},
  each consisting of a type, an annotated expression,
  and a \sideeffectsetterm. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case empty {
    exprs = empty_list;
    --
    empty_list;
  }

  case non_empty {
    exprs =: match_cons(e, exprs1);
    annotate_expr(tenv, e) -> typed_expr;
    annotate_exprs(tenv, exprs1) -> typed_exprs1;
    --
    cons(typed_expr, typed_exprs1);
  }
;

semantics relation eval_call(env: envs, name: Identifier, params: list0(expr), args: list0(expr)) ->
    ( list0((native_value, XGraphs)), new_env: envs)
    | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a call to the subprogram named {name} in
                        the environment {env}, with the parameter expressions
                        {params} and the argument expressions {args}. The
                        evaluation results in a list of pairs, each consisting
                        of a \nativevalueterm{} and an \executiongraphterm{},
                        and a new environment. \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
} =
  eval_expr_list_m(env, params) -> ResultExprListM(vparams, env1)
  { [_] };
  eval_expr_list_m(env1, args) -> ResultExprListM(vargs, env2);
  env2 =: (tenv, denv2);
  incr_pending_calls(denv2.dynamic_envs_G, name) -> genv;
  env2' := (tenv, [dynamic_envs_G: genv, dynamic_envs_L: empty_denv.dynamic_envs_L]);
  case normal {
    eval_subprogram(env2', name, vparams, vargs) -> ResultCall((values_read, global), g)
    { [_] };
    ( INDEX(i, values_read: read_value_from(values_read[i]) -> ms2[i]) )
    { ([_]) };
    decr_pending_calls(global, name) -> genv2;
    new_denv := denv2(dynamic_envs_G: genv2);
    new_env := (tenv, new_denv);
    --
    (ms2, new_env);
  }

  case throwing {
    eval_subprogram(env2', name, vparams, vargs) -> Throwing(v, t, g, env_throw)
    { [_] };
    env_throw =: (tenv_sub, denv_throw);
    vglobal := denv_throw.dynamic_envs_G;
    decr_pending_calls(vglobal, name) -> genv2;
    new_denv := denv2(dynamic_envs_G: genv2);
    new_env := (tenv, new_denv);
    --
    Throwing(v, t, g, new_env);
  }
;

semantics relation eval_subprogram(
  env: envs,
  name: Identifier,
  params: list0((native_value, XGraphs)),
  actual_args: list0((native_value, XGraphs))) ->
  | ResultCall((list0(value_read_from), new_genv: global_dynamic_envs), XGraphs)
  | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates the subprogram named {name} in the
                        environment {env}, with {actual_args} the list of
                        actual arguments, and {params} the list of arguments
                        deduced by type equality. The result is either a
                        \Prosenormalconfiguration{} or an abnormal
                        configuration. In the case of a
                        \Prosenormalconfiguration{}, it consists of a list of
                        pairs with a value and an identifier, and a new
                        environment {new_genv}. The values represent values
                        returned by the subprogram call and the identifiers
                        are used in generating execution graph constraints
                        for the returned values.",
 prose_application = "",
  math_layout = [_,_],
} =
  env =: (tenv, denv);
  tenv.static_envs_G.subprogram(name) =: (func_sig, _);
  func_sig =:
    [
      args: args,
      parameters: parameters,
      func_body: func_body,
      recurse_limit: recurse_limit
    ];
  env1 := (tenv, denv(dynamic_envs_L: empty_denv.dynamic_envs_L));
  check_recurse_limit(env1, name, recurse_limit) -> g1;
  args =: list_combine(arg_names, arg_types);
  assign_args((env1, empty_graph), arg_names, actual_args) -> (env2, g2);
  parameters =: list_combine(param_names, param_types);
  assign_args((env2, g2), param_names, params) -> (env3, g3);
  case returning {
    eval_stmt(env3, func_body) -> Returning((vs, body_g), body_env);
    match_func_res(Returning((vs, body_g), body_env)) -> ResultCall((res_vs, new_genv), res_g)
    { [_] };
    new_g := ordered_data(g1, ordered_po(g2, g3));
  }
  case continuing {
    eval_stmt(env3, func_body) -> Continuing(body_g, body_env);
    match_func_res(Continuing(body_g, body_env)) -> ResultCall((res_vs, new_genv), res_g)
    { [_] };
    new_g := ordered_data(g1, ordered_po(g2, g3));
  }
  --
  ResultCall((res_vs, new_genv), new_g)
  { [_] };
;

semantics relation assign_args((env: envs, g1: XGraphs), ids: list0(Identifier), actuals: list0((native_value, XGraphs))) -> (new_env: envs, new_g: XGraphs)
{
   prose_description = "updates the pair consisting of the environments {env}
                        and \executiongraphterm{} {g1} by assigning the values
                        given by {actuals} to the identifiers given by {ids},
                        yielding the updated pair ({new_env}, {new_g}).",
 prose_application = "",
} =
  case empty {
    ids = empty_list;
    actuals = empty_list;
    --
    (env, g1);
  }

  case non_empty {
    ids =: match_cons(x, ids1);
    actuals =: match_cons(vm, actuals1);
    declare_local_identifier_mm(env, x, vm) -> (env1, g2);
    assign_args((env1, ordered_po(g1, g2)), ids1, actuals1) -> (new_env, new_g);
    --
    (new_env, new_g);
  }
;

semantics relation match_func_res(C: TContinuingOrReturning) ->
    ResultCall(vms2: (list0(value_read_from), new_gdenv: global_dynamic_envs), XGraphs)
{
   prose_description = "converts continuing configurations and returning
                        configurations into corresponding normal
                        configurations that can be returned by a subprogram
                        evaluation.",
  prose_application = "",
  math_layout = [_,_],
} =
  case continuing {
    C =: Continuing(g, env);
    env =: (tenv, denv);
    --
    ResultCall((empty_list, denv.dynamic_envs_G), g);
  }

  case returning {
    C =: Returning((xs, _), ret_env);
    ret_env =: (tenv, ret_denv);
    ids := list_map(i, indices(xs), numbered_identifier(return_var_prefix, (i - one)));
    vs := list_combine(xs, ids);
    --
    ResultCall((vs, ret_denv.dynamic_envs_G), empty_graph);
  }
;

// REVIEW: signature changed to allow divergence.
// Previous: semantics relation check_recurse_limit(env: envs, name: Identifier, e_limit_opt: option(expr)) -> (g: XGraphs) | TDynError
semantics relation check_recurse_limit(env: envs, name: Identifier, e_limit_opt: option(expr)) ->
         (g: XGraphs) | TDynError | TDiverging
{
  "checks whether the value in the optional expression
  {e_limit_opt} has reached the limit associated with
  {name} in {env}, yielding the execution graph
  resulting from evaluating the optional expression in
  {g}. Otherwise, the result is a
  \DynamicErrorConfigurationTerm{} indicating that the
  recursion limit has been reached.",
  prose_application = "",
} =
  case none {
    eval_limit(env, e_limit_opt) -> (None, _);
    --
    empty_graph;
  }

  case some_ok {
    eval_limit(env, e_limit_opt) -> (some(limit), g);
    env =: (tenv, denv);
    get_pending_calls(denv, name) -> pending;
    pending < limit;
    --
    g;
  }

  case limit_exceeded {
    eval_limit(env, e_limit_opt) -> (some(limit), g);
    env =: (tenv, denv);
    get_pending_calls(denv, name) -> pending;
    pending >= limit;
    --
    DynamicError(DE_LE);
  }
;

semantics relation read_value_from(vrf: value_read_from) -> (native_value, XGraphs)
{
  "generates an execution graph for reading the given
  value to a variable given by the identifier, and pairs
  it with the given value.",
  prose_application = "",
} =
  vrf =: (v, id);
  read_identifier(id, v) -> g;
  --
  (v, g);
;

//////////////////////////////////////////////////
// Relations for Subprogram Declarations

typing relation annotate_and_declare_func(genv: global_static_envs, func_sig: func) ->
         (new_tenv: static_envs, new_func_sig: func, ses: powerset(TSideEffect)) | type_error
{
  "annotates a subprogram definition {func_sig} in the
    \globalstaticenvironmentterm{} {genv}, yielding a new
    subprogram definition {new_func_sig}, a modified
    \staticenvironmentterm{} {new_tenv}, and an inferred
    \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_func_sig(genv, func_sig) -> (tenv1, func_sig1, ses1);
  declare_one_func(tenv1, func_sig1, ses1) -> (new_tenv, new_func_sig);
  --
  (new_tenv, new_func_sig, ses1);
;

typing relation annotate_func_sig(genv: global_static_envs, func_sig: func) ->
         (new_tenv: static_envs, new_func_sig: func, ses: powerset(TSideEffect)) | type_error
{
  "annotates the signature of a function definition
  {func_sig} in the \globalstaticenvironmentterm{}
  {genv}, yielding a new function definition
  {new_func_sig}, a modified \staticenvironmentterm{}
  {new_tenv}, and an inferred \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  with_empty_local(genv) -> tenv;
  annotate_limit_expr(tenv, func_sig.recurse_limit) -> (rec_limit, ses_limit);
  annotate_params(tenv, func_sig.parameters, (tenv, empty_list)) ->
    (tenv_with_params, ses_with_params_raw, params)
  { [_] };
  ses_with_params := union(ses_limit, ses_with_params_raw);
  check_param_decls(tenv, func_sig) -> True;
  annotate_args(
    tenv_with_params,
    func_sig.args,
    (tenv_with_params, empty_list, ses_with_params)
  ) -> (tenv_with_args, args, ses_with_args)
  { [[_], _] };
  annotate_return_type(
    tenv_with_args,
    tenv_with_params,
    func_sig.return_type,
    ses_with_args
  ) -> (new_tenv, return_type, ses_with_return)
  { [[_], _]};
  ses_list := list_set(ses_with_return);
  ses_list_no_locals := list_filter(se, ses_list, not(se = LocalEffect(_)));
  ses' := list_to_set(ses_list_no_locals);
  check_subprogram_purity(func_sig.qualifier, ses') -> True;
  ses_for_subprogram(func_sig.qualifier) -> ses;
  new_func_sig := func_sig(
    parameters: params,
    args: args,
    return_type: return_type,
    recurse_limit: rec_limit
  );
  --
  (new_tenv, new_func_sig, ses);
;

typing relation annotate_params(
  tenv: static_envs,
  params: list0((Identifier, option(ty))),
  (new_tenv: static_envs, acc: list0((Identifier, option(ty))))) ->
         (tenv_with_params: static_envs,
         ses: powerset(TSideEffect),
         params1: list0((Identifier, option(ty)))) | type_error
{
  "annotates each parameter in {params} with respect to
  {tenv}, and declares it in environment {new_tenv}. It
  returns the updated environment {tenv_with_params},
  the inferred \sideeffectsetterm{}, and
  the annotated parameters {params1}, together with any
  annotated parameters already in the accumulator {acc}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    params = empty_list;
    --
    (new_tenv, empty_set, acc);
  }

  case non_empty {
    params =: match_cons((x, ty_opt), params');
    annotate_one_param(tenv, new_tenv, (x, ty_opt)) -> (new_tenv', ty, ses1)
    { [_] };
    acc' := concat(acc, match_singleton_list((x, some(ty))));
    annotate_params(tenv, params', (new_tenv', acc')) -> (tenv_with_params, ses2, params2)
    { [_] };
    ses := union(ses1, ses2);
    --
    (tenv_with_params, ses, params2)
    { [_] };
  }
;

typing relation annotate_one_param(
    tenv: static_envs,
    new_tenv: static_envs,
    (x: Identifier, ty_opt: option(ty))) ->
    (new_tenv': static_envs, ty: ty, ses: powerset(TSideEffect)) | type_error
{
   prose_description = "annotates the parameter given by {x} and the
                        \optionalterm{} type {ty_opt} with respect to {tenv} and
                        then declares the parameter {x} in environment
                        {new_tenv}. The updated environment {new_tenv'},
                        annotated parameter type {ty}, and its \sideeffectsetterm{} are returned.
                        \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case type_parameterized {
    ty_opt = None || (ty_opt =: some(unconstrained_integer));
    ty' := T_Int(Parameterized(x));
    annotate_type(False, tenv, ty') -> (ty, ses_ty);
    check_var_not_in_env(new_tenv, x) -> True;
    check_constrained_integer(new_tenv, ty) -> True;
    add_local(new_tenv, x, ty, LDK_Let) -> new_tenv';
    --
    (new_tenv', ty, ses_ty);
  }

  case type_annotated {
    ty_opt =: some(ty');
    ty' != unconstrained_integer;
    annotate_type(False, tenv, ty') -> (ty, ses_ty);
    check_var_not_in_env(new_tenv, x) -> True;
    check_constrained_integer(new_tenv, ty) -> True;
    add_local(new_tenv, x, ty, LDK_Let) -> new_tenv';
    --
    (new_tenv', ty, ses_ty);
  }
;

typing relation check_param_decls(tenv: static_envs, func_sig: func) ->
         (b: Bool) | type_error
{
  "checks the validity of the parameters declared in
  {func_sig}.",
  prose_application = "",
} =
  extract_parameters(tenv, func_sig) -> inferred_parameters;
  func_sig.parameters =: list_combine(declared_parameters, param_types);
  te_check(inferred_parameters = declared_parameters, TE_BSPD) -> True;
  --
  True;
;

typing function extract_parameters(tenv: static_envs, func_sig: func) ->
         (unique_parameters: list0(Identifier)) | type_error
{
  "returns the parameter names declared in {func_sig}
    into {unique_parameters}, while checking their validity
    in the \staticenvironmentterm{} {tenv}.
    \ProseOtherwiseTypeError",
  prose_application = "",
} =
  func_sig_types(func_sig) -> tys;
  case empty {
    tys = empty_list;
    --
    empty_list;
  }

  case non_empty {
    tys != empty_list;
    INDEX(i, tys: paramsofty(tenv, tys[i]) -> params_lists[i]);
    all_parameters := list_flatten(params_lists);
    --
    unique_list(all_parameters);
  }
;

typing function unique_list(ids: list0(Identifier)) ->
         (unique_ids: list0(Identifier))
{
  "removes duplicate identifiers from {ids}, preserving
  their first occurrence order.",
  prose_application = "",
} =
  unique_list_acc(ids, empty_set) -> unique_ids;
  --
  unique_ids;
;

typing function unique_list_acc(ids: list0(Identifier), seen: powerset(Identifier)) ->
         (unique_ids: list0(Identifier))
{
  "removes duplicate identifiers from {ids}, using
  {seen} to track previously encountered identifiers.",
  prose_application = "",
} =
  case empty {
    ids = empty_list;
    --
    empty_list;
  }

  case non_empty {
    ids =: match_cons(id, ids1);
    already_seen := member(id, seen);
    unique_list_acc(ids1, if_then_else(already_seen, seen, union(seen, make_set(id)))) -> rest;
    unique_ids := if_then_else(already_seen, rest, concat(match_singleton_list(id), rest));
    --
    unique_ids;
  }
;

typing function func_sig_types(func_sig: func) ->
         (tys: list0(ty))
{
  "returns the list of types {tys} in the subprogram
  signature {func_sig}. Their ordering is return type
  first (if any), followed by argument types
  left-to-right.",
  prose_application = "",
} =
  return_type_lst :=
    if func_sig.return_type =: some(ret_ty)
    then make_list(ret_ty)
    else empty_list
    { (_, [_]) };
  func_sig.args =: list_combine(arg_names, arg_types);
  --
  concat(return_type_lst, arg_types);
;

typing function paramsofty(tenv: static_envs, ty: ty) ->
         (ids: list0(Identifier)) | type_error
{
  "extracts the list of parameters appearing in the type
  {ty}, assuming that {ty} appears in a function
  signature. \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \paramsofty
} =
  case t_bits {
    ty =: T_Bits(e, _);
    params_of_expr(tenv, e) -> ids;
    --
    ids;
  }

  case t_tuple {
    ty =: T_Tuple(tys);
    INDEX(i, tys: paramsofty(tenv, tys[i]) -> ids_lists[i]);
    --
    list_flatten(ids_lists);
  }

  case t_int_wellconstrained {
    ty =: T_Int(WellConstrained(cs));
    INDEX(i, cs: params_of_constraint(tenv, cs[i]) -> ids_lists[i]);
    --
    list_flatten(ids_lists);
  }

  case other {
    or(
      ast_label(ty) in make_set(label_T_Array, label_T_Bool, label_T_Named, label_T_Real, label_T_String),
      is_unconstrained_integer(ty),
      is_parameterized_integer(ty)
    ) { [_] };
    --
    empty_list;
  }

  case error {
    ast_label(ty) = label_T_Enum || is_structured(ty);
    --
    TypeError(TE_BSPD);
  }
;

typing function params_of_expr(tenv: static_envs, e: expr) ->
         (ids: list0(Identifier)) | type_error
{
  "extracts the list of parameters appearing in the
  expression {e}. It assumes that {e} appears as
  $\TBits(\ve, \Ignore)$ or as part of a
  \wellconstrainedintegertypeterm{} in a function
  signature. \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \paramsofexpr
} =
  case e_var {
    e =: E_Var(x);
    ids := if_then_else(is_undefined(tenv, x), match_singleton_list(x), empty_list);
    --
    ids;
  }

  case e_unop {
    e =: E_Unop(_, e1);
    params_of_expr(tenv, e1) -> ids;
    --
    ids;
  }

  case e_binop {
    e =: E_Binop(_, e1, e2);
    params_of_expr(tenv, e1) -> ids1;
    params_of_expr(tenv, e2) -> ids2;
    --
    concat(ids1, ids2);
  }

  case e_tuple {
    e =: E_Tuple(es);
    es =: make_singleton_list(e1);
    params_of_expr(tenv, e1) -> ids;
    --
    ids;
  }

  case e_cond {
    e =: E_Cond(e0, e1, e2);
    params_of_expr(tenv, e0) -> ids0;
    params_of_expr(tenv, e1) -> ids1;
    params_of_expr(tenv, e2) -> ids2;
    --
    concat(ids0, concat(ids1, ids2));
  }

  case other {
    ast_label(e) not_in make_set(label_E_Binop, label_E_Tuple, label_E_Unop, label_E_Var);
    --
    TypeError(TE_BSPD);
  }
;

typing function params_of_constraint(tenv: static_envs, c: int_constraint) ->
         (ids: list0(Identifier))
{
  "finds the list of parameters in the constraint {c}. It
  assumes that {c} appears within a
  \wellconstrainedintegertypeterm{} in a function
  signature.",
  prose_application = "",
  math_macro = \paramsofconstraint
} =
  case exact {
    c =: Constraint_Exact(e);
    params_of_expr(tenv, e) -> ids;
    --
    ids;
  }

  case range {
    c =: Constraint_Range(e1, e2);
    params_of_expr(tenv, e1) -> ids1;
    params_of_expr(tenv, e2) -> ids2;
    --
    concat(ids1, ids2);
  }
;

typing relation annotate_args(tenv: static_envs, args: list0((Identifier, ty)), (new_tenv: static_envs, acc: list0((Identifier, ty)), ses_in: powerset(TSideEffect))) ->
         (tenv_with_args: static_envs, new_args: list0((Identifier, ty)), ses: powerset(TSideEffect)) | type_error
{
  "annotates each argument in {args} with respect to
  {tenv} and a \sideeffectsetterm{} {ses_in}, and
  declares it in environment {new_tenv}. It returns the
  updated environment {tenv_with_args}, the annotated
  arguments {new_args}, together with any annotated
  arguments already in the accumulator {acc}, and a
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case empty {
    args = empty_list;
    --
    (new_tenv, acc, ses_in)
    { [_] };
  }

  case non_empty {
    args =: match_cons((x, ty), args1);
    annotate_one_arg(tenv, new_tenv, (x, ty)) -> (new_tenv1, ty', ses_ty);
    acc' := concat(acc, match_singleton_list((x, ty')));
    annotate_args(tenv, args1, (new_tenv1, acc', ses_in)) -> (tenv_with_args, new_args, ses1)
    { [_] };
    ses := union(ses_ty, ses1);
    --
    (tenv_with_args, new_args, ses)
    { [_] };
  }
;

typing relation annotate_one_arg(tenv: static_envs, new_tenv: static_envs, (x: Identifier, ty: ty)) ->
         (new_tenv': static_envs, ty': ty, ses: powerset(TSideEffect)) | type_error
{
  "annotates the argument given by the identifier {x} and
  the type {ty} with respect to {tenv} and then declares
  the parameter {x} in environment {new_tenv}. The
  result is the updated environment {new_tenv'},
  annotated argument type {ty'}, and inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_type(False, tenv, ty) -> (ty', ses);
  check_var_not_in_env(new_tenv, x) -> True;
  add_local(new_tenv, x, ty', LDK_Let) -> new_tenv';
  --
  (new_tenv', ty', ses);
;

typing relation annotate_return_type(
  tenv_with_params: static_envs,
  tenv_with_args: static_envs,
  return_type: option(ty),
  ses_in: powerset(TSideEffect)) ->
         (new_tenv: static_envs,
         new_return_type: option(ty),
         ses: powerset(TSideEffect)) | type_error
{
  "annotates the \optionalterm{} return type {return_type} in
  the context of the \staticenvironmentterm{}
  {tenv_with_params}, where all parameters have been
  declared, and the \sideeffectsetterm{} {ses_in}. The
  result is {new_tenv}, which is the input
  {tenv_with_args} (where all parameters and arguments
  have been declared) with the \optionalterm{} annotated
  return type {new_return_type} added and the inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case no_return_type {
    return_type = None;
    --
    (tenv_with_args, None, ses_in)
    { ([_], [_]) };
  }

  case has_return_type {
    return_type =: some(ty);
    annotate_type(False, tenv_with_params, ty) -> (ty', ses_ty);
    new_return_type := some(ty');
    new_lenv := tenv_with_args.static_envs_L(local_return_type: new_return_type);
    new_tenv := tenv_with_args(static_envs_L: new_lenv);
    ses := union(ses_in, ses_ty);
    --
    (new_tenv, new_return_type, ses)
    { ([_], [_]) };
  }
;

typing function check_subprogram_purity(qualifier: option(func_qualifier), ses: powerset(TSideEffect)) ->
         (b: Bool) | type_error
{
  "checks that the \sideeffectsetterm{} {ses} is
  consistent with the subprogram qualifier {qualifier}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case none_or_noreturn {
    qualifier = None || qualifier = some(Noreturn);
    --
    True;
  }

  case some_pure {
    qualifier = some(Pure);
    te_check(ses_is_pure(ses), TE_SEV) -> True;
    --
    True;
  }

  case some_readonly {
    qualifier = some(Readonly);
    te_check(ses_is_readonly(ses), TE_SEV) -> True;
    --
    True;
  }
;

typing relation declare_one_func(tenv: static_envs, func_sig: func, ses_func_sig: powerset(TSideEffect)) ->
         (new_tenv: static_envs, new_func_sig: func) | type_error
{
  "checks that a subprogram defined by {func_sig} and
    associated with the \sideeffectsetterm{} {ses_func_sig}
    can be added to the \staticenvironmentterm{} {tenv},
    resulting in an annotated function definition
    {new_func_sig} and new \staticenvironmentterm{}
    {new_tenv}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  func_sig =: [name: name, qualifier: qualifier, args: args, func_subprogram_type: subprogram_type];
  add_new_func(tenv, name, qualifier, args, subprogram_type) -> (tenv1, name')
  { [_] };
  te_check(tenv1.static_envs_G.subprogram(name') = bot, TE_IAD) -> True;
  new_func_sig := func_sig(name: name');
  add_subprogram(tenv1, name', new_func_sig, ses_func_sig) -> new_tenv;
  --
  (new_tenv, new_func_sig)
  { [_] };
;

typing function subprogram_clash(
  tenv: static_envs,
  name: Strings,
  subpgm_type: subprogram_type,
  qualifier: option(func_qualifier),
  formal_types: list0(ty)) ->
    (b: Bool) | type_error
{
  "checks whether the unique subprogram associated with
  {name} clashes with another subprogram that has
  subprogram type {subpgm_type}, qualifier {qualifier},
  and list of formal types {formal_types}, yielding a
  Boolean value in {b}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  tenv.static_envs_G.subprogram(name) =: (other_func_sig, _);
  subprogram_types_clash(other_func_sig.func_subprogram_type, subpgm_type) -> subpgm_types_clash
  { [_] };
  qualifiers_differ := other_func_sig.qualifier != qualifier;
  has_arg_clash(tenv, formal_types, other_func_sig.args) -> arg_types_clash
  { [_] };
  --
  subpgm_types_clash && (qualifiers_differ || arg_types_clash)
  { [_] };
;

typing function subprogram_types_clash(s1: subprogram_type, s2: subprogram_type) ->
         (b: Bool)
{
  "defines whether the subprogram types {s1} and {s2}
  clash, yielding the result in {b}.",
  prose_application = "",
} =
  b := not((s1 = ST_Getter && s2 = ST_Setter) || (s1 = ST_Setter && s2 = ST_Getter));
  --
  b;
;

typing relation add_new_func(
  tenv: static_envs,
  name: Identifier,
  qualifier: option(func_qualifier),
  formals: list0(typed_identifier),
  subpgm_type: subprogram_type) ->
         (new_tenv: static_envs, new_name: Strings) | type_error
{
  "ensures that the subprogram given by the identifier
  {name}, qualifier {qualifier}, list of formals
  {formals}, and subprogram type {subpgm_type} has a
  unique name among all the potential subprograms that
  overload {name}. The result is the unique subprogram
  identifier {new_name}, which is used to distinguish it
  in the set of overloaded subprograms (that is, other
  subprograms that share the same name) and the
  environment {new_tenv}, which is updated with
  {new_name}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = ([_,_,_,_,_],_),
} =
  case first_name {
    tenv.static_envs_G.overloaded_subprograms(name) = bot;
    updated_map := map_update(tenv.static_envs_G.overloaded_subprograms, name, make_set(name));
    new_genv := tenv.static_envs_G(overloaded_subprograms : updated_map);
    new_tenv := tenv(static_envs_G : new_genv);
    --
    (new_tenv, name)
    { [_] };
  }

  case name_exists {
    tenv.static_envs_G.overloaded_subprograms(name) =: other_names;
    k := cardinality(other_names);
    new_name := numbered_identifier(name, k);
    formals =: list_combine(formal_names, formal_types);
    othernames_list := list_set(other_names);
    ( INDEX(i, othernames_list:
      subprogram_clash(tenv, othernames_list[i], subpgm_type, qualifier, formal_types) -> bs[i]
    ) )
    { ([_, ([_],_)]) };
    te_check(list_and(bs), TE_BSPD) -> True;
    ( updated_map := map_update(
      tenv.static_envs_G.overloaded_subprograms,
      name,
      union(other_names, make_set(new_name))
    ) )
    { ([_]) };
    new_genv := tenv.static_envs_G(overloaded_subprograms : updated_map);
    new_tenv := tenv(static_envs_G : new_genv);
    --
    (new_tenv, new_name)
    { [_] };
  }
;

typing relation annotate_subprogram(
  tenv: static_envs,
  f: func,
  ses_func_sig: powerset(TSideEffect)) ->
         (f': func, ses: powerset(TSideEffect)) | type_error
{
  "annotates a subprogram {f} in an environment {tenv}
  and \sideeffectsetterm{} {ses_func_sig}, resulting in
  an annotated subprogram {f'} and inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_block(tenv, f.func_body) -> (new_body, ses_body);
  check_control_flow(tenv, f, new_body) -> True;
  f' := f(func_body: new_body);
  ses_body_list := list_set(ses_body);
  ses_body_list_no_locals := list_filter(se, ses_body_list, not(se = LocalEffect(_)));
  ses_body_no_locals := list_to_set(ses_body_list_no_locals);
  ses' := union(ses_func_sig, ses_body_no_locals);
  check_subprogram_purity(f.qualifier, ses') -> True;
  ses_for_subprogram(f.qualifier) -> ses;
  --
  (f', ses);
;

typing function check_control_flow(tenv: static_envs, f: func, body: stmt) ->
         CheckResult | type_error
{
  "checks whether the annotated body statement {body} of the
  function definition {f} obeys the control-flow
  requirements in the \staticenvironmentterm{} {tenv}.",
  prose_application = "",
} =
  approx_stmt(tenv, body) -> abs_configs;
  te_check(subseteq(abs_configs, allowed_abs_configs(f)), TE_BSPD) -> True;
  --
  True;
;

typing function allowed_abs_configs(f: func) ->
         (abs_configs: powerset(abstract_configuration))
{
  "determines the set of \Proseabstractconfigurations{}
  allowed for the function definition {f}, yielding the
  result in {abs_configs}.",
  prose_application = "",
} =
  case noreturn {
    f.qualifier = some(Noreturn);
    --
    make_set(Abs_Abnormal);
  }

  case returning_proc {
    f.qualifier != some(Noreturn);
    f.return_type = None;
    --
    make_set(Abs_Abnormal, Abs_Continuing, Abs_Returning);
  }

  case func {
    f.qualifier != some(Noreturn);
    f.return_type != None;
    --
    make_set(Abs_Abnormal, Abs_Returning);
  }
;

typing function approx_stmt(tenv: static_envs, s: stmt) ->
         (abs_configs: powerset(abstract_configuration))
{
  "returns in {abs_configs} a superset of the set of
  \Proseabstractconfigurations{} (defined next), that an
  evaluation of {s} in any environment consisting of the
  \staticenvironmentterm{} {tenv} yields.",
  prose_application = "",
} =
  case s_pass {
    s = S_Pass;
    --
    make_set(Abs_Continuing);
  }

  case simple {
    ast_label(s) in make_set(
      label_S_Assign,
      label_S_Decl,
      label_S_Print,
      laebl_S_Assert);
    --
    make_set(Abs_Abnormal, Abs_Continuing);
  }

  case s_unreachable {
    s = S_Unreachable;
    --
    make_set(Abs_Abnormal);
  }

  case s_call {
    s =: S_Call(call);
    tenv.static_envs_G.subprogram(call.call_name) =: (f, _);
    abs_configs := if_then_else(
      f.qualifier = some(Noreturn),
      make_set(Abs_Abnormal),
      make_set(Abs_Abnormal, Abs_Continuing)
    ) { (_, [_]) };
    --
    abs_configs;
  }

  case s_return {
    s = S_Return(_);
    --
    make_set(Abs_Abnormal, Abs_Returning);
  }

  case s_throw {
    s = S_Throw(_);
    --
    make_set(Abs_Abnormal);
  }

  case s_seq {
    s =: S_Seq(s1, s2);
    approx_stmt(tenv, s1) -> configs1;
    approx_stmt(tenv, s2) -> configs2;
    configs1_list := list_set(configs1);
    config_sets :=
      list_map(c, configs1_list,
        if_then_else(c = Abs_Continuing, configs2, make_set(c))
      )
    { (_, (_, _, [_])) };
    --
    union_list(config_sets);
  }

  case s_repeat {
    s =: S_Repeat(body, _, _);
    approx_stmt(tenv, body) -> bodyconfigs;
    abs_configs := union(bodyconfigs, make_set(Abs_Abnormal));
    --
    abs_configs;
  }

  case s_for {
    s =: S_For[
      index_name: _,
      start_e: _,
      dir: _,
      end_e: _,
      body: body,
      limit: _
    ];
    approx_stmt(tenv, body) -> bodyconfigs;
    abs_configs := union(bodyconfigs, make_set(Abs_Abnormal, Abs_Continuing));
    --
    abs_configs;
  }

  // TODO: merge with previous case when aslspec allows redefining body.
  case s_while {
    s =: S_While(_, _, body);
    approx_stmt(tenv, body) -> bodyconfigs;
    abs_configs := union(bodyconfigs, make_set(Abs_Abnormal, Abs_Continuing));
    --
    abs_configs;
  }

  case s_cond {
    s =: S_Cond(_, s1, s2);
    approx_stmt(tenv, s1) -> configs1;
    approx_stmt(tenv, s2) -> configs2;
    // BUGFIX: LaTeX uses vsone/vstwo in the union; use configs1/configs2 from approx_stmt.
    abs_configs := union(make_set(Abs_Abnormal), configs1, configs2);
    --
    abs_configs;
  }

  case s_try_none {
    s =: S_Try(body, catchers, None);
    approx_stmt(tenv, body) -> bodyconfigs;
    catchers =: list_combine_three(catch_pats, catch_tys, catch_stmts);
    INDEX(i, catch_stmts: approx_stmt(tenv, catch_stmts[i]) -> catch_configs[i]);
    abs_configs := union(bodyconfigs, union_list(catch_configs));
    --
    abs_configs;
  }

  case s_try_some {
    s =: S_Try(body, catchers, some(otherwise_stmt));
    approx_stmt(tenv, body) -> bodyconfigs;
    approx_stmt(tenv, otherwise_stmt) -> otherwise_configs;
    catchers =: list_combine_three(catch_pats, catch_tys, catch_stmts);
    INDEX(i, catch_stmts: approx_stmt(tenv, catch_stmts[i]) -> catch_configs[i]);
    abs_configs := union(bodyconfigs, otherwise_configs, union_list(catch_configs));
    --
    abs_configs;
  }
;

//////////////////////////////////////////////////
// Relations for Symbolic Equivalence Testing

typing function normalize(tenv: static_envs, e: expr) ->
         (new_e: expr) | type_error
{
  "\hypertarget{def-symbolicallysimplifies}{symbolically
  simplifies} an expression {e} in the
  \staticenvironmentterm{} {tenv}, yielding an
  expression {new_e}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-normalize}{simplifying} expression {e} in {tenv} yields {new_e}",
} =

  case normalizable {
    to_ir(tenv, e) -> some(p);
    --
    polynomial_to_expr(p);
  }

  case not_normalizable {
    to_ir(tenv, e) -> None;
    --
    e;
  }
;

typing function reduce_constraint(tenv: static_envs, c: int_constraint) ->
         (new_c: int_constraint)
{
  "\symbolicallysimplifiesterm{} an integer constraint
  {c}, yielding the integer constraint {new_c}.",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    normalize(tenv, e) -> e';
    --
    Constraint_Exact(e');
  }

  case range {
    c =: Constraint_Range(e1, e2);
    normalize(tenv, e1) -> e1';
    normalize(tenv, e2) -> e2';
    --
    Constraint_Range(e1', e2');
  }
;

typing function reduce_constraints(tenv: static_envs, cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint))
{
  "\symbolicallysimplifiesterm{} a list of integer
  constraints {cs}, yielding a list of integer
  constraints {new_cs}.",
  prose_application = "",
} =
  INDEX(i, cs: reduce_constraint(tenv, cs[i]) -> new_cs[i]);
  --
  new_cs;
;

typing function to_ir(tenv: static_envs, e: expr) ->
         (p_opt: option(polynomial)) | type_error
{
  "transforms a subset of ASL expressions into
  \symbolicexpressionsterm{}. If an ASL expression
  cannot be represented by a symbolic expression
  (because, for example, it contains operations that are
  not available in \symbolicexpressionsterm{}), the
  result is $\None$.",
  prose_application = "",
} =
  case literal_int {
    e =: E_Literal(L_Int(i));
    polynomial_of_int(i) -> p;
    --
    some(p);
  }

  case literal_other {
    e =: E_Literal(v);
    ast_label(v) != label_L_Int;
    --
    None;
  }

  case evar_constant_int {
    e =: E_Var(s);
    lookup_constant(tenv, s) -> some(L_Int(i));
    polynomial_of_int(i) -> p;
    --
    some(p);
  }

  case evar_immutable_expr {
    e =: E_Var(s);
    lookup_constant(tenv, s) -> None;
    lookup_immutable_expr(tenv, s) -> some(e');
    to_ir(tenv, e') -> some(p) | None;
    --
    some(p);
  }

  case evar_exact_constraint {
    e =: E_Var(s);
    lookup_constant(tenv, s) -> None;
    lookup_immutable_expr(tenv, s) -> None;
    type_of(tenv, s) -> t;
    make_anonymous(tenv, t) -> t1;
    case int {
      case exact {
        ast_label(t1) = label_T_Int;
        t1 =: T_Int(WellConstrained(make_singleton_list(Constraint_Exact(e1))));
        to_ir(tenv, e1) -> p_opt;
        --
        p_opt;
      }
      case not_exact {
        ast_label(t1) = label_T_Int;
        not(t1 = T_Int(WellConstrained(make_singleton_list(Constraint_Exact(_)))));
        polynomial_of_var(s) -> p;
        --
        some(p);
      }
      case evar_non_int {
        ast_label(t1) != label_T_Int;
        --
        None;
      }
    }
  }

  case ebinop_plus {
    e =: E_Binop(ADD, e1, e2);
    to_ir(tenv, e1) -> some(p1) | None;
    to_ir(tenv, e2) -> some(p2) | None;
    add_polynomials(p1, p2) -> p;
    --
    some(p);
  }

  case ebinop_minus {
    e =: AbbrevEBinop(SUB, e1, e2);
    e' := AbbrevEBinop(ADD, e1, E_Unop(NEG, e2));
    to_ir(tenv, e') -> p_opt;
    --
    p_opt;
  }

  case ebinop_mul_div_left {
    e =: AbbrevEBinop(MUL, AbbrevEBinop(DIV, e1, e2), e3);
    to_ir(tenv, AbbrevEBinop(DIV, AbbrevEBinop(MUL, e1, e3), e2)) -> p_opt;
    --
    p_opt;
  }

  case ebinop_mul_div_right {
    e =: E_Binop(MUL, e1, E_Binop(DIV, e2, e3));
    not(is_div_binop(e1));
    to_ir(tenv, AbbrevEBinop(DIV, AbbrevEBinop(MUL, e1, e2), e3)) -> p_opt;
    --
    p_opt;
  }

  case ebinop_mul {
    e =: E_Binop(MUL, e1, e2);
    is_div_binop(e1) -> False;
    is_div_binop(e2) -> False;
    to_ir(tenv, e1) -> some(p1) | None;
    to_ir(tenv, e2) -> some(p2) | None;
    mul_polynomials(p1, p2) -> p;
    --
    some(p);
  }

  case ebinop_div_int_denominator {
    e =: E_Binop(DIV, e1, E_Literal(L_Int(i2)));
    to_ir(tenv, e1) -> some(p1) | None;
    f2 := fraction(one, i2);
    scale_polynomial(p1, f2) -> p;
    --
    some(p);
  }

  case ebinop_div_monomial_denominator {
    e =: E_Binop(DIV, e1, e2);
    to_ir(tenv, e1) -> some(p1) | None;
    to_ir(tenv, e2) -> some(p2) | None;
    bindings(p2) =: match_singleton_list((m, factor));
    polynomial_divide_by_term(p1, m, factor) -> p_opt;
    --
    p_opt;
  }

  case ebinop_div_non_monomial_denominator {
    e =: E_Binop(DIV, e1, e2);
    to_ir(tenv, e1) -> _;
    to_ir(tenv, e2) -> some(p2) | None;
    not(bindings(p2) = match_singleton_list(_));
    --
    None;
  }

  case ebinop_shl_non_lint_exponent {
    e =: E_Binop(SHL, _, e2);
    not(e2 = ELint(_));
    --
    None;
  }

  case ebinop_shl_neg_shift {
    e =: E_Binop(SHL, _, E_Literal(L_Int(i2)));
    i2 < zero;
    --
    None;
  }

  case ebinop_shl_okay {
    e =: E_Binop(SHL, e1, E_Literal(L_Int(i2)));
    to_ir(tenv, e1) -> some(p1) | None;
    i2 >= zero;
    f2 := as_rational(num_exponent(two, i2));
    scale_polynomial(p1, f2) -> p;
    --
    some(p);
  }

  case ebinop_other_non_literals {
    e =: E_Binop(op, e1, e2);
    op not_in make_set(ADD, DIV, MUL, SHL, SUB);
    not(e1 = E_Literal(_)) || not(e2 = E_Literal(_));
    --
    None;
  }

  case ebinop_other_literals_non_int_result {
    e =: E_Binop(op, E_Literal(v1), E_Literal(v2));
    op not_in make_set(ADD, DIV, MUL, SHL, SUB);
    binop_literals(op, v1, v2) -> v;
    ast_label(v) != label_L_Int;
    --
    None;
  }

  case ebinop_other_literals_int_result {
    e =: E_Binop(op, E_Literal(v1), E_Literal(v2));
    op not_in make_set(ADD, DIV, MUL, SHL, SUB);
    binop_literals(op, v1, v2) -> L_Int(k);
    polynomial_of_int(k) -> p;
    --
    some(p);
  }

  case eunop_neg {
    e =: E_Unop(NEG, e1);
    to_ir(tenv, AbbrevEBinop(MUL, ELint(num_negate(one)), e1)) -> p_opt;
    --
    p_opt;
  }

  case eunop_other {
    e =: E_Unop(op, _);
    op != NEG;
    --
    None;
  }

  case atc {
    e =: E_ATC(e', _);
    to_ir(tenv, e') -> p_opt;
    --
    p_opt;
  }

  case other {
    ast_label(e) not_in make_set(label_E_ATC, label_E_Binop, label_E_Literal, label_E_Unop, label_E_Var);
    --
    None;
  }
;

typing function polynomial_of_int(z: Z) -> (p: polynomial)
{
  "the polynomial for the integer {z}.",
  prose_application = "",
};

typing function polynomial_of_var(s: Identifier) -> (p: polynomial)
{
  "builds a polynomial for the variable {s}.",
  prose_application = "",
} =
  m := map_update(bot, s, n_to_n_pos(one));
  --
  map_update(bot, m, as_rational(one));
;

typing function scale_polynomial(p1: polynomial, f: Q) -> (p: polynomial)
{
  "scales the coefficients of {p1} by {f}, which is assumed to be non-zero.",
  prose_application = "",
} =
  bindings(p1) =: list_combine(ms, fs);
  fs_scaled := list_map(f1, fs, f1 * f);
  scaled_bindings := list_map(i, indices(ms), (ms[i], fs_scaled[i]));
  --
  bindings_to_map(scaled_bindings);
;

typing function is_div_binop(e: expr) -> (b: Bool)
{
  "tests whether {e} is a division binop expression.",
  prose_application = "",
} =
  --
  (e = E_Binop(DIV, _, _));
;

typing function expr_equal(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "conservatively checks whether {e1} and {e2} are
  \equivalentexprsterm{} in the \staticenvironmentterm{}
  {tenv}. The result is given in {b} or a
  \typingerrorterm{}, if one is detected.",
  prose_application = "",
} =
  case norm_true {
    expr_equal_norm(tenv, e1, e2) -> True;
    --
    True;
  }

  case norm_false {
    expr_equal_norm(tenv, e1, e2) -> False;
    expr_equal_case(tenv, e1, e2) -> b;
    --
    b;
  }
;

typing function expr_equal_norm(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "conservatively tests whether {e1} and {e2} are
  \equivalentexprsterm{} in the \staticenvironmentterm{}
  {tenv} by attempting to transform both expressions to
  their \symbolicexpressionterm{} form and, if
  successful, comparing the resulting
  \symbolicexpressionsterm{} for equality. The result is
  given in {b} or a \typingerrorterm{}, if one is
  detected.",
  prose_application = "",
} =
  to_ir(tenv, e1) -> ir1_opt;
  to_ir(tenv, e2) -> ir2_opt;
  case all_supported {
    and(
      ir1_opt =: some(ir1),
      ir1_opt =: some(ir2)
    );
    --
    ir1 = ir2;
  }

  case some_unsupported {
    ir1_opt = None || ir2_opt = None;
    --
    False;
  }
;

typing function expr_equal_case(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "specializes the equivalence test for expressions {e1}
  and {e2} in {tenv} for the different types of
  expressions. The result is given in {b} or a
  \typingerrorterm{}, if one is detected.",
  prose_application = "",
} =
  case different_labels {
    ast_label(e1) != ast_label(e2);
    --
    False;
  }

  case e_binop {
    e1 =: E_Binop(op1, e1_1, e1_2);
    e2 =: E_Binop(op2, e2_1, e2_2);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    expr_equal(tenv, e1_2, e2_2) -> b2;
    --
    (op1 = op2) && b1 && b2;
  }

  case e_call {
    e1 =: E_Call(call1);
    e2 =: E_Call(call2);
    bool_transition(call1.call_name = call2.call_name) -> True | False;
    call_args1 := call1.call_args;
    call_args2 := call2.call_args;
    bool_transition(same_length(call_args1, call_args2)) -> True | False;
    ( INDEX(i, call_args1: expr_equal(tenv, call_args1[i], call_args2[i]) -> args_equal[i]) )
    { ([_]) };
    call_params1 := call1.params;
    call_params2 := call2.params;
    bool_transition(same_length(call_params1, call_params2)) -> True | False;
    ( INDEX(i, call_params1: expr_equal(tenv, call_params1[i], call_params2[i]) -> params_equal[i]) )
    { ([_]) };
    --
    list_and(args_equal) && list_and(params_equal);
  }

  case e_cond {
    e1 =: E_Cond(e1_1, e1_2, e1_3);
    e2 =: E_Cond(e2_1, e2_2, e2_3);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    expr_equal(tenv, e1_2, e2_2) -> b2;
    expr_equal(tenv, e1_3, e2_3) -> b3;
    --
    b1 && b2 && b3;
  }

  case e_slice {
    e1 =: E_Slice(e1_1, slices1);
    e2 =: E_Slice(e2_1, slices2);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    slices_equal(tenv, slices1, slices2) -> b2;
    --
    b1 && b2;
  }

  case e_getarray {
    e1 =: E_GetArray(e1_1, e1_2);
    e2 =: E_GetArray(e2_1, e2_2);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    expr_equal(tenv, e1_2, e2_2) -> b2;
    --
    b1 && b2;
  }

  case e_getfield {
    e1 =: E_GetField(e1_1, f1);
    e2 =: E_GetField(e2_1, f2);
    b1 := f1 = f2;
    expr_equal(tenv, e1_1, e2_1) -> b2;
    --
    b1 && b2;
  }

  case e_getfields {
    e1 =: E_GetFields(e1_1, fs1);
    e2 =: E_GetFields(e2_1, fs2);
    b1 := fs1 = fs2;
    expr_equal(tenv, e1_1, e2_1) -> b2;
    --
    b1 && b2;
  }

  case e_getitem {
    e1 =: E_GetItem(e1_1, i1);
    e2 =: E_GetItem(e2_1, i2);
    b1 := i1 = i2;
    expr_equal(tenv, e1_1, e2_1) -> b2;
    --
    b1 && b2;
  }

  case e_literal {
    e1 =: E_Literal(v1);
    e2 =: E_Literal(v2);
    --
    v1 = v2;
  }

  case e_tuple {
    e1 =: E_Tuple(es1);
    e2 =: E_Tuple(es2);
    bool_transition(same_length(es1, es2)) -> True | False;
    INDEX(i, es1: expr_equal(tenv, es1[i], es2[i]) -> component_equal[i]);
    --
    list_and(component_equal);
  }

  case e_array {
    e1 =: E_Array[length: length1, array_value: value1];
    e2 =: E_Array[length: length2, array_value: value2];
    expr_equal(tenv, length1, length2) -> b1;
    expr_equal(tenv, value1, value2) -> b2;
    --
    b1 && b2;
  }

  case e_unop {
    e1 =: E_Unop(op1, e1_1);
    e2 =: E_Unop(op2, e2_1);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    --
    (op1 = op2) && b1;
  }

  case e_arbitrary {
    e1 = E_Arbitrary(_);
    e2 = E_Arbitrary(_);
    --
    False;
  }

  case e_atc {
    e1 =: E_ATC(e1_1, t1);
    e2 =: E_ATC(e2_1, t2);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    type_equal(tenv, t1, t2) -> b2;
    --
    b1 && b2;
  }

  case e_var {
    e1 =: E_Var(name1);
    e2 =: E_Var(name2);
    --
    name1 = name2;
  }

  case e_pattern {
    e1 =: E_Pattern(e1', p1);
    e2 =: E_Pattern(e2', p2);
    expr_equal(tenv, e1', e2') -> b1;
    pattern_equal(tenv, p1, p2) -> b2;
    --
    b1 && b2;
  }

  case e_record {
    e1 =: E_Record(s1, fields1);
    e2 =: E_Record(s2, fields2);
    type_equal(tenv, s1, s2) -> True | False;
    fields1 =: list_combine(field_names1, field_types1);
    fields2 =: list_combine(field_names2, field_types2);
    ( INDEX(i, fields1: expr_equal(tenv, field_types1[i], field_types2[i]) -> True | False, TypeErrorConfig()) )
    { ([_]) };
    --
    True;
  }
;

typing function pattern_equal(tenv: static_envs, p1: pattern, p2: pattern) ->
  (b: Bool) | type_error
{
  "tests whether {p1} is equivalent to {p2} in {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case all {
    p1 = Pattern_All;
    p2 = Pattern_All;
    --
    True;
  }

  case any_len {
    p1 =: Pattern_Any(ps1);
    p2 =: Pattern_Any(ps2);
    bool_transition(same_length(ps1, ps2)) -> True | False;
    INDEX(i, ps1: pattern_equal(tenv, ps1[i], ps2[i]) -> bs[i]);
    --
    list_and(bs);
  }

  case tuple_len {
    p1 =: Pattern_Tuple(ps1);
    p2 =: Pattern_Tuple(ps2);
    bool_transition(same_length(ps1, ps2)) -> True | False;
    INDEX(i, ps1: pattern_equal(tenv, ps1[i], ps2[i]) -> bs[i]);
    --
    list_and(bs);
  }

  case geq {
    p1 =: Pattern_Geq(e1);
    p2 =: Pattern_Geq(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case leq {
    p1 =: Pattern_Leq(e1);
    p2 =: Pattern_Leq(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case single {
    p1 =: Pattern_Single(e1);
    p2 =: Pattern_Single(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case mask{
    p1 =: Pattern_Mask(m1);
    p2 =: Pattern_Mask(m2);
    --
    m1 = m2;
  }

  case not {
    p1 =: Pattern_Not(p1');
    p2 =: Pattern_Not(p2');
    pattern_equal(tenv, p1', p2') -> b;
    --
    b;
  }

  case range {
    p1 =: Pattern_Range(e11, e12);
    p2 =: Pattern_Range(e21, e22);
    expr_equal(tenv, e11, e21) -> b1;
    expr_equal(tenv, e12, e22) -> b2;
    --
    b1 && b2;
  }

  case other {
    ast_label(p1) != ast_label(p2);
    --
    False;
  }
;

typing function type_equal(tenv: static_envs, t1: ty, t2: ty) ->
         (b: Bool) | type_error
{
  "conservatively tests whether {t1} and {t2} are
  \equivalenttypesterm{} in the \staticenvironmentterm{}
  {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case different_labels {
    ast_label(t1) != ast_label(t2);
    --
    False;
  }

  case tbool_treal_tstring {
    ast_label(t1) = ast_label(t2);
    ast_label(t1) in make_set(label_T_Bool, label_T_Real, label_T_String);
    --
    True;
  }

  case tint_unconstrained {
    t1 = unconstrained_integer;
    t2 = unconstrained_integer;
    --
    True;
  }

  case tint_parameterized {
    t1 =: T_Int(Parameterized(i1));
    t2 =: T_Int(Parameterized(i2));
    --
    i1 = i2;
  }

  case tint_wellconstrained {
    t1 =: T_Int(WellConstrained(cs1));
    t2 =: T_Int(WellConstrained(cs2));
    constraints_equal(tenv, cs1, cs2) -> b;
    --
    b;
  }

  case tbits {
    t1 =: T_Bits(w1, bf1);
    t2 =: T_Bits(w2, bf2);
    bitwidth_equal(tenv, w1, w2) -> b1;
    bitfields_equal(tenv, bf1, bf2) -> b2;
    --
    b1 && b2;
  }

  case tarray {
    t1 =: T_Array(l1, ty1);
    t2 =: T_Array(l2, ty2);
    array_length_equal(tenv, l1, l2) -> b1;
    type_equal(tenv, ty1, ty2) -> b2;
    --
    b1 && b2;
  }

  case tnamed {
    t1 =: T_Named(n1);
    t2 =: T_Named(n2);
    --
    n1 = n2;
  }

  case tenum {
    t1 =: T_Enum(li1);
    t2 =: T_Enum(li2);
    --
    li1 = li2;
  }

  case tstructured {
    t1 =: make_structured(L1, fields1);
    t2 =: make_structured(L2, fields2);
    L1 = L2;
    ast_label(L1) in make_set(label_T_Exception, label_T_Record);
    names1 := list_fst(fields1);
    names2 := list_fst(fields2);
    bool_transition(list_to_set(names1) = list_to_set(names2)) -> True | False;
    INDEX(i, names1: field_type(fields1, names1[i]) -> some(ts1[i]));
    INDEX(i, names1: field_type(fields2, names1[i]) -> some(ts2[i]));
    INDEX(i, ts1: type_equal(tenv, ts1[i], ts2[i]) -> types_equal[i]);
    --
    list_and(types_equal);
  }

  case ttuple {
    t1 =: T_Tuple(ts1);
    t2 =: T_Tuple(ts2);
    bool_transition(same_length(ts1, ts2)) -> True | False;
    INDEX(i, ts1: type_equal(tenv, ts1[i], ts2[i]) -> types_equal[i]);
    --
    list_and(types_equal);
  }
;

typing function bitwidth_equal(tenv: static_envs, w1: expr, w2: expr) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the bitwidth expression
  {w1} is equivalent to the bitwidth expression {w2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  expr_equal(tenv, w1, w2) -> b;
  --
  b;
;

typing function bitfields_equal(tenv: static_envs, bf1: list0(bitfield), bf2: list0(bitfield)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the list of bitfields
  {bf1} is equivalent to the list of bitfields {bf2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  bool_transition(same_length(bf1, bf2)) -> True | False;
  INDEX(i, bf1: bitfield_equal(tenv, bf1[i], bf2[i]) -> bf_equal[i]);
  --
  list_and(bf_equal);
;

typing function bitfield_equal(tenv: static_envs, bf1: bitfield, bf2: bitfield) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the bitfield {bf1} is
  equivalent to the bitfield {bf2} in environment {tenv}
  and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case different_labels {
    ast_label(bf1) != ast_label(bf2);
    --
    False;
  }

  case bitfield_simple {
    bf1 =: BitField_Simple(name1, slices1);
    bf2 =: BitField_Simple(name2, slices2);
    bool_transition(name1 = name2) -> True | False;
    slices_equal(tenv, slices1, slices2) -> b;
    --
    b;
  }

  case bitfield_nested {
    bf1 =: BitField_Nested(name1, slices1, bfs1);
    bf2 =: BitField_Nested(name2, slices2, bfs2);
    bool_transition(name1 = name2) -> True | False;
    slices_equal(tenv, slices1, slices2) -> slices_equal1;
    bitfields_equal(tenv, bfs1, bfs2) -> nested_equal;
    --
    slices_equal1 && nested_equal;
  }

  case bitfield_typed_name_match {
    bf1 =: BitField_Type(name1, slices1, ty1);
    bf2 =: BitField_Type(name2, slices2, ty2);
    bool_transition(name1 = name2) -> True | False;
    slices_equal(tenv, slices1, slices2) -> slices_equal;
    type_equal(tenv, ty1, ty2) -> types_equal;
    --
    slices_equal && types_equal;
  }
;

typing function constraints_equal(tenv: static_envs, cs1: list0(int_constraint), cs2: list0(int_constraint)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the constraint list {cs1}
  is equivalent to the constraint list {cs2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  bool_transition(same_length(cs1, cs2)) -> True | False;
  INDEX(i, cs1: constraint_equal(tenv, cs1[i], cs2[i]) -> cs_equal[i]);
  --
  list_and(cs_equal);
;

typing function constraint_equal(tenv: static_envs, c1: int_constraint, c2: int_constraint) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the constraint {c1} is
  equivalent to the constraint {c2} in environment
  {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case different_labels {
    ast_label(c1) != ast_label(c2);
    --
    False;
  }

  case constraint_exact {
    c1 =: Constraint_Exact(e1);
    c2 =: Constraint_Exact(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case constraint_range {
    c1 =: Constraint_Range(e1_1, e1_2);
    c2 =: Constraint_Range(e2_1, e2_2);
    expr_equal(tenv, e1_1, e2_1) -> b1;
    expr_equal(tenv, e1_2, e2_2) -> b2;
    --
    b1 && b2;
  }
;

typing function slices_equal(tenv: static_envs, slices1: list0(slice), slices2: list0(slice)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the list of slices
  {slices1} is equivalent to the list of slices
  {slices2} in environment {tenv} and yields the result
  in {b}.  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  bool_transition(same_length(slices1, slices2)) -> True | False;
  ( INDEX(i, slices1: slice_equal(tenv, slices1[i], slices2[i]) -> equal_at_index[i]) )
  { ([_]) };
  --
  list_and(equal_at_index);
;

typing function slice_equal(tenv: static_envs, slice1: slice, slice2: slice) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the slice {slice1} is
  equivalent to the slice {slice2} in environment {tenv}
  and yields the result in {b}. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case single_expr {
    slice1 =: Slice_Single(e1);
    slice2 =: Slice_Single(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case range_exprs {
    slice1 =: Slice_Range(e1, e2);
    slice2 =: Slice_Range(e3, e4);
    expr_equal(tenv, e1, e3) -> b1;
    expr_equal(tenv, e2, e4) -> b2;
    --
    b1 && b2;
  }

  case length_exprs {
    slice1 =: Slice_Length(e1, e2);
    slice2 =: Slice_Length(e3, e4);
    expr_equal(tenv, e1, e3) -> b1;
    expr_equal(tenv, e2, e4) -> b2;
    --
    b1 && b2;
  }

  case star_exprs {
    slice1 =: Slice_Star(e1, e2);
    slice2 =: Slice_Star(e3, e4);
    expr_equal(tenv, e1, e3) -> b1;
    expr_equal(tenv, e2, e4) -> b2;
    --
    b1 && b2;
  }

  case different_labels {
    ast_label(slice1) != ast_label(slice2);
    --
    False;
  }
;

typing function array_length_equal(tenv: static_envs, l1: array_index, l2: array_index) ->
         (b: Bool) | type_error
{
  "tests whether the array lengths {l1} and {l2} are
  equivalent and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case different_labels {
    ast_label(l1) != ast_label(l2);
    --
    False;
  }

  case expr_expr {
    l1 =: ArrayLength_Expr(e1);
    l2 =: ArrayLength_Expr(e2);
    expr_equal(tenv, e1, e2) -> b;
    --
    b;
  }

  case enum_enum {
    l1 =: ArrayLength_Enum(enum1, _);
    l2 =: ArrayLength_Enum(enum2, _);
    --
    enum1 = enum2;
  }
;

typing function mul_monomials(m1: unitary_monomial, m2: unitary_monomial) -> (m: unitary_monomial)
{
  "multiplies the unitary monomial {m1} with the unitary monomial {m2},
  yielding the unitary monomial {m}.",
  prose_application = "multiplying the unitary monomial {m1} with unitary monomial {m2}
                      yields the unitary monomial {m}",
} =
  --
  bot; // This function is expressed directly in LaTeX.
;

typing function add_polynomials(p1: polynomial, p2: polynomial) -> (p: polynomial)
{
  "adds the polynomial {p1} with the polynomial {p2},
  yielding the polynomial {p}",
  prose_application = "adding the polynomial {p1} with polynomial {p2}
                      yields the polynomial {p}",
} =
  --
  bot; // We express this function directly in lates, without inference rules.
;

typing function mul_polynomials(p1: polynomial, p2: polynomial) -> (p: polynomial)
{
  "multiplies the polynomial {p1} with the polynomial {p2}, yielding the polynomial {p}.",
  prose_application = "multiplying the polynomial {p1} with polynomial {p2}
                      yields the polynomial {p}"
} =
  --
  bot; // We express this function directly in lates, without inference rules.
;

typing function polynomial_divide_by_term(p1: polynomial, m: unitary_monomial, f: Q) ->
  (p_opt: option(polynomial))
{
  "returns the result of dividing the polynomial {p1} by the unitary monomial {m} multiplied by {f}.
  If the division can be performed, the result is a polynomial, and otherwise it is $\None$..",
  prose_application = "",
  math_layout = ([_,_,_], _),
} =
  --
  bot; // This function is expressed directly in LaTeX.
;

// TODO: define this operator in terms of sort and then remove it.
operator sort_monomial_bindings(monoms: list0((unitary_monomial, Q))) -> list0((unitary_monomial, Q))
{
  math_macro = \sortmonomialbindings,
};

typing function polynomial_to_expr(p: polynomial) ->
         (e: expr)
{
  "transforms a polynomial {p} into the corresponding expression {e}.",
  prose_application = "\hyperlink{relation-polynomialtoexpr}{converting} polynomial {p} to an expression yields {e}",
} =
  case empty {
    bindings(p) = empty_list;
    --
    ELint(zero);
  }

  case non_empty {
    ms := list_set(dom(p));
    monoms_and_factors := bindings(p);
    monoms := sort_monomial_bindings(monoms_and_factors);
    monomials_to_expr(monoms) -> (e1, s1);
    e := if_then_else(
      s1 = equal_sign,
      ELint(zero),
      if_then_else(s1 = positive_sign, e1, E_Unop(NEG, e1)))
    { (_, [_]) };
    --
    e;
  }
;

constant compare_identifier : fun Identifier -> Sign
{
  "identifier comparison",
};

typing function compare_monomial_bindings(
  (m1: unitary_monomial, q1: Q),
  (m2: unitary_monomial, q2: Q)) ->
         (s: Sign)
{
  "compares two uniary monomial bindings given by $(\vmone,
  \vqone)$ and $(\vmtwo, \vqtwo)$ and yields in {s} $-1$
  to mean that the first monomial binding should be
  ordered before the second, $0$ to mean that any
  ordering of the monomial bindings is acceptable, and
  $1$ to mean that the second monomial binding should be
  ordered before the first.",
  prose_application = "",
  math_layout = ([_,_],_)
} =
  case equal_monomials {
    m1 = m2;
    --
    sign(q2 - q1);
  }

  case different_monomials {
    m1 != m2;
    ids := sort(list_set(union(dom(m1), dom(m2))), compare_identifier);
    differences := list_filter(id, ids, map_apply(m1, id) = map_apply(m2, id));
    differences =: match_cons(k, _);
    s := cond(
      map_apply(m1, k) = bot && map_apply(m1, k) != bot : positive_sign,
      map_apply(m1, k) != bot && map_apply(m1, k) = bot : negative_sign,
      and((map_apply(m1, k) =: n1) && (n1 != bot) && (map_apply(m1, k) =: n2) && (n2 != bot)) : if n1 > n2 then positive_sign else negative_sign,
    ) { (_, [_,_,([_], [_])]) };
    --
    s;
  }
;

typing function monomials_to_expr(monoms: list0((unitary_monomial, Q))) ->
         (e: expr, s: Sign)
{
  "transforms a list consisting of pairs of unitary
  monomials and rational factors {monoms} (so, general
  monomials), into an expression {e}, which represents
  the absolute value of the sum of all the monomials,
  and a sign value {s}, which indicates the sign of the
  resulting sum.",
  prose_application = "\hyperlink{relation-monomialstoexpr}{converting} monomial list {monoms} to expression form
                        yields absolute value {e} and sign {s}",
} =
  case empty {
    monoms = empty_list;
    --
    (ELint(zero), equal_sign);
  }

  case non_empty {
    monoms =: match_cons((m, q), monoms1);
    unitary_monomials_to_expr(bindings(m)) -> e1';
    monomial_to_expr(e1', q) -> (e1, s1);
    monomials_to_expr(monoms1) -> (e2, s2);
    sym_add_expr(e1, s1, e2, s2) -> (e, s);
    --
    (e, s);
  }
;

typing function monomial_to_expr(e: expr, q: Q) ->
         (new_e: expr, s: Sign)
{
  "transforms an expression {e} and rational $q$ into the
  expression {new_e}, which represents the absolute
  value of {e} multiplied by $q$, and the sign of $q$ as
  {s}.",
  prose_application = "",
} =
  case q_zero {
    q = rational_zero;
    --
    (ELint(zero), equal_sign);
  }

  case q_natural {
    q > rational_zero && is_integer(q);
    q_nat := round_down(q);
    sym_mul_expr(ELint(q_nat), e) -> new_e;
    --
    (new_e, positive_sign);
  }

  case q_positive_fraction {
    q > rational_zero && is_not_integer(q);
    and(q =: fraction(d, n), is_not_integer(fraction(d, n)));
    sym_mul_expr(ELint(d), e) -> e2;
    new_e := E_Binop(DIV, e2, ELint(n));
    --
    (new_e, positive_sign);
  }

  case q_negative {
    q < rational_zero;
    monomial_to_expr(e, num_negate(q)) -> (new_e, _);
    --
    (new_e, negative_sign);
  }
;

typing function sym_mul_expr(e1: expr, e2: expr) -> (e: expr)
{
  "symbolically multiplies expressions {e1} and {e2},
  simplifying away the literal 1 operand when present.",
  prose_application = "\hyperlink{def-symmulexpr}{symbolically multiplying} expressions {e1} and {e2} yields {e}",
} =
  case one_operand {
    e1 = ELint(one) || e2 = ELint(one);
    --
    if e1 = ELint(one) then e1 else e1;
  }

  case no_one_operand {
    not(e1 = ELint(one) || e2 = ELint(one));
    --
    E_Binop(MUL, e1, e2);
  }
;

typing function sym_add_expr(e1: expr, s1: Sign, e2: expr, s2: Sign) ->
         (e: expr, s: Sign)
{
  "symbolically sums the expressions {e1} and {e2} with
  respective signs {s1} and {s2} yielding the expression
  {e} and sign {s}.",
  prose_application = "\hyperlink{relation-symaddexpr}{symbolically summing} expressions {e1} with sign {s1}
                        and {e2} with sign {s2} yields expression {e} with sign {s}",
} =
  case zero {
    (s1 = equal_sign || s2 = equal_sign);
    (e, s) := if (s1 = equal_sign) then (e2, s2) else (e1, s1);
    --
    (e, s);
  }

  case same_sign {
    and(s1 != equal_sign, s2 != equal_sign, s1 = s2);
    --
    (E_Binop(ADD, e1, e2), s1);
  }

  case different_signs {
    and(s1 != equal_sign, s2 != equal_sign, s1 != s2);
    --
    (E_Binop(SUB, e1, e2), s1);
  }
;

typing function unitary_monomials_to_expr(monoms: list0((Identifier, N))) ->
         (e: expr)
{
  "transforms a list of single-variable unitary monomials
  {monoms} into an expression {e}. Intuitively, {monoms}
  represent a multiplication of the single-variable
  unitary monomials.",
  prose_application = "",
} =
  case empty {
    monoms = empty_list;
    --
    ELint(one);
  }

  case exp_zero {
    monoms =: match_cons((v, n), monoms1);
    n = zero;
    unitary_monomials_to_expr(monoms1) -> e;
    --
    e;
  }

  case exp_one {
    monoms =: match_cons((v, n), monoms1);
    n = one;
    e1 := E_Var(v);
    unitary_monomials_to_expr(monoms1) -> e2;
    sym_mul_expr(e1, e2) -> e;
    --
    e;
  }

  case exp_two {
    monoms =: match_cons((v, n), monoms1);
    n = two;
    e1 := AbbrevEBinop(MUL, E_Var(v), E_Var(v));
    unitary_monomials_to_expr(monoms1) -> e2;
    sym_mul_expr(e1, e2) -> e;
    --
    e;
  }

  case exp_gt_two {
    monoms =: match_cons((v, n), monoms1);
    n > two;
    e1 := AbbrevEBinop(POW, E_Var(v), ELint(n));
    unitary_monomials_to_expr(monoms1) -> e2;
    sym_mul_expr(e1, e2) -> e;
    --
    e;
  }
;

typing function type_of(tenv: static_envs, s: Identifier) ->
         (ty: ty) | type_error
{
  "looks up the environment {tenv} for a type {ty}
  associated with an identifier {s}. The result is
  \typingerrorterm{} if {s} is not associated with any
  type.",
  prose_application = "",
} =
  case local {
    tenv.static_envs_L.local_storage_types(s) =: (ty, _);
    --
    ty;
  }

  case global {
    tenv.static_envs_L.local_storage_types(s) = bot;
    tenv.static_envs_G.global_storage_types(s) =: (ty, _);
    --
    ty;
  }

  case error {
    tenv.static_envs_L.local_storage_types(s) = bot;
    tenv.static_envs_G.global_storage_types(s) = bot;
    --
    TypeError(TE_UI);
  }
;

typing function normalize_opt(tenv: static_envs, e: expr) ->
         (new_e_opt: option(expr)) | type_error
{
  "is similar to $\normalize$, except that it returns
  $\None$ when {e} is not an expression that can be
  symbolically simplified. \ProseOtherwiseTypeError",
  prose_application = "",
} =
  to_ir(tenv, e) -> some(p) | None, TypeErrorConfig();
  // to_ir(tenv, e) -> CannotBeTransformed;
  normalize(tenv, e) -> new_e;
  --
  some(new_e);
;

//////////////////////////////////////////////////
// Relations for Symbolic Subsumption Testing

typing function symdom_subset_unions(tenv: static_envs, sd1: symdom_or_top, sd2: symdom_or_top) ->
         (b: Bool)
{
  "conservatively tests whether the set of integers
  represented by {sd1} is a subset of the set of
  integers represented by {sd2}, in the context of the
  \staticenvironmentterm{} {tenv}, yielding the result
  in {b}.",
  prose_application = "\hyperlink{relation-symdomsubsetunions}{testing} whether {sd1} is subsumed by {sd2} in {tenv} yields {b}",
} =
  case right_top {
    sd2 = Top;
    --
    True;
  }

  case left_top_right_not_top {
    sd1 = Top;
    sd2 != Top;
    --
    False;
  }

  case subdomains {
    sd1 =: Subdomains(symdoms1);
    sd2 =: Subdomains(symdoms2);
    symdom_normalize(symdoms1) -> symdoms1n;
    symdom_normalize(symdoms2) -> symdoms2n;
    b := list_forall(s1, symdoms1n, list_exists(s2, symdoms2n, symdom_subset(tenv, s1, s2)))
    { (_, [_]) };
    --
    b;
  }
;

typing function symdom_normalize(symdoms: list1(symdom)) ->
         (new_symdoms: list1(symdom))
{
  "transforms the list of symbolic domain {symdoms} into
  an equivalent list of symbolic domains {new_symdoms}
  (in the sense that they both represent the same set of
  integers) where all symbolic finite set integer
  domains are merged into a single symbolic finite set
  integer domain whose set of integers is the union of
  the sets of integers in the merged symbolic finite set
  integer domains.",
  prose_application = "\hyperlink{relation-symdomnormalize}{normalizing} {symdoms} by merging finite sets yields {new_symdoms}",
} =
  finite_sets := list_map(sd, symdoms, if sd =: Finite(s) then s else empty_set);
  finite_set := set_as_finite(union_list(finite_sets));
  others := list_filter(sd, symdoms, not(sd = Finite(_)));
  has_finite := list_exists(fs, finite_sets, fs != empty_set);
  new_list := if_then_else(
    has_finite,
    concat(match_singleton_list(Finite(finite_set)), others),
    others);
  new_symdoms := match_non_empty_list(new_list);
  --
  new_symdoms;
;

typing function symdom_of_type(tenv: static_envs, t: ty) ->
         (d: symdom_or_top)
{
  "transforms a type {t} in a \staticenvironmentterm{}
  {tenv} into a symbolic domain {d}. It assumes its
  input type has an \underlyingtypeterm{} which is an
  integer.",
  prose_application = "",
} =
  case int_unconstrained {
    t = unconstrained_integer;
    --
    Top;
  }

  case int_parameterized {
    t =: T_Int(Parameterized(id));
    d := Subdomains(make_singleton_list(ConstrainedDom(Constraint_Exact(E_Var(id)))));
    --
    d;
  }

  case int_well_constrained {
    t =: T_Int(WellConstrained(cs));
    INDEX(i, cs: symdom_of_constraint(tenv, cs[i]) -> ds[i]);
    --
    Subdomains(match_non_empty_list(ds));
  }

  case t_named {
    t = T_Named(_);
    make_anonymous(tenv, t) -> t1;
    symdom_of_type(tenv, t1) -> d;
    --
    d;
  }
;

typing function symdom_of_width_expr(e: expr) ->
         (d: symdom_or_top)
{
  "assigns a symbolic domain {d} to an \underline{integer
  typed} expression {e}, where {e} is assumed to be the
  expression conveying the width of a
  \bitvectortypeterm.",
  prose_application = "",
} =
  d := Subdomains(make_singleton_list(ConstrainedDom(Constraint_Exact(e))));
  --
  d;
;

typing function symdom_of_constraint(tenv: static_envs, c: int_constraint) ->
         (d: symdom)
{
  "transforms an integer constraint {c} into a symbolic
  domain {d} in the context of the
  \staticenvironmentterm{} {tenv}. It produces $\Top$
  when the expressions involved in the integer
  constraints cannot be simplified to integers.",
  prose_application = "",
} =
  case exact_top {
    c =: Constraint_Exact(e);
    symdom_eval(tenv, e) -> Top | ;
    --
    ConstrainedDom(c);
  }

  case exact_int {
    c =: Constraint_Exact(e);
    symdom_eval(tenv, e) -> v;
    --
    Finite(make_set(v));
  }

  case range_top_left {
    c =: Constraint_Range(e1, e2);
    symdom_eval(tenv, e1) -> Top;
    --
    ConstrainedDom(c);
  }

  case range_top_right {
    c =: Constraint_Range(e1, e2);
    symdom_eval(tenv, e1) -> v1;
    symdom_eval(tenv, e2) -> Top;
    --
    ConstrainedDom(c);
  }

  case range_ints {
    c =: Constraint_Range(e1, e2);
    symdom_eval(tenv, e1) -> v1;
    symdom_eval(tenv, e2) -> v2;
    --
    Finite(range_set(v1, v2));
  }
;

typing function symdom_eval(tenv: static_envs, e: expr) ->
         (n: Z) | constants_set(Top)
{
  "\symbolicallysimplifiesterm{} the
  \underline{integer-typed} expression {e} and returns
  the resulting integer or $\Top$ if the result of the
  simplification is not an integer.",
  prose_application = "",
} =
  case integer {
    normalize(tenv, e) -> e1;
    static_eval(tenv, e1) -> L_Int(n);
    --
    n;
  }

  case top {
    normalize(tenv, e) -> e1;
    static_eval(tenv, e1) -> v;
    ast_label(v) != label_L_Int;
    --
    Top;
  }
;

typing function symdom_subset(tenv: static_envs, cd1: symdom, cd2: symdom) ->
         (b: Bool)
{
  "conservatively tests whether the values represented by
  the \symbolicdomainterm{} {cd1} is a subset of the
  values represented by the \symbolicdomainterm{} {cd2}
  in any environment consisting of the
  \staticenvironmentterm{} {tenv}, yielding the result
  in {b}.",
  prose_application = "",
} =
  case finite_finite {
    cd1 =: Finite(s1);
    cd2 =: Finite(s2);
    --
    subseteq(s1, s2);
  }

  case constrained_constrained_equal {
    cd1 =: ConstrainedDom(c1);
    cd2 =: ConstrainedDom(c2);
    constraint_equal(tenv, c1, c2) -> True;
    --
    True;
  }

  case constrained_constrained_non_equal {
    cd1 =: ConstrainedDom(c1);
    cd2 =: ConstrainedDom(c2);
    constraint_equal(tenv, c1, c2) -> False;
    approx_constraint(tenv, Over, c1) -> s1_opt;
    approx_constraint(tenv, Under, c2) -> s2_opt;
    b := if and(s1_opt =: some(s1), s2_opt =: some(s2)) then subseteq(s1, s2) else False;
    --
    b;
  }

  case finite_constrained {
    cd1 =: Finite(s1);
    cd2 =: ConstrainedDom(c2);
    approx_constraints(tenv, Under, make_singleton_list(c2)) -> some(s2);
    --
    subseteq(s1, s2);
  }

  case constrained_finite_set {
    cd1 =: ConstrainedDom(c1);
    cd2 =: Finite(s2);
    approx_constraints(tenv, Over, make_singleton_list(c1)) -> some(s1);
    --
    subseteq(s1, s2);
  }

  case constrained_finite_top {
    cd1 =: ConstrainedDom(c1);
    cd2 = Finite(_);
    approx_constraints(tenv, Over, make_singleton_list(c1)) -> None;
    --
    False;
  }
;

typing function approx_constraints(tenv: static_envs, approx: constants_set(Over,Under), cs: list0(int_constraint)) ->
         (s: option(powerset_finite(Z)))
{
  "conservatively approximates the non-empty list of
  constraints {cs} by a set of integers. The
  approximation is over all environments consisting of
  the \staticenvironmentterm{} {tenv}. The approximation
  is either overapproximation or underapproximation,
  based on the \approximationdirectionterm{} {approx}.",
  prose_application = "",
} =
  case empty {
    cs = empty_list;
    --
    some(empty_set);
  }

  case non_empty_fail {
    cs != empty_list;
    s_opts := list_map(c, cs, approx_constraint(tenv, approx, c));
    list_exists(s_opt, s_opts, s_opt = None);
    --
    None;
  }

  case over {
    approx = Over;
    cs != empty_list;
    s_opts := list_map(c, cs, approx_constraint(tenv, approx, c));
    list_forall(s_opt, s_opts, s_opt != None);
    s_sets := list_map(i, indices(s_opts), if s_opts[i] =: some(s1) then s1 else empty_set);
    --
    some(union_list_finite(s_sets));
  }

  case under {
    approx = Under;
    cs != empty_list;
    s_opts := list_map(c, cs, approx_constraint(tenv, approx, c));
    list_forall(s_opt, s_opts, s_opt != None);
    s_sets := list_map(i, indices(s_opts), if s_opts[i] =: some(s1) then s1 else empty_set);
    --
    some(union_list_finite(s_sets));
  }
;

typing function approx_constraint(tenv: static_envs, approx: constants_set(Over,Under), c: int_constraint) ->
         (s: option(powerset(Z)))
{
  "conservatively approximates the constraint {c} by a
  set of integer. The approximation is over all
  environments that consist of the
  \staticenvironmentterm{} {tenv}. The approximation is
  either overapproximation or underapproximation, based
  on the \approximationdirectionterm{} {approx}.
  If {c} can be approximated, the result is the approximating set in an optional.
  Otherwise, the result is $\None$.",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    approx_expr(tenv, approx, e) -> s_opt;
    --
    s_opt;
  }

  case range_over_interval {
    approx = Over;
    c =: Constraint_Range(e1, e2);
    approx_expr_min(tenv, e1) -> some(z1) | None;
    approx_expr_max(tenv, e2) -> some(z2) | None;
    make_interval(Over, z1, z2) -> s_interval_opt;
    --
    s_interval_opt;
  }

  case range_under_interval {
    approx = Under;
    c =: Constraint_Range(e1, e2);
    approx_expr_max(tenv, e1) -> some(z1);
    approx_expr_min(tenv, e2) -> some(z2);
    make_interval(Under, z1, z2) -> s_interval_opt;
    s := if (s_interval_opt =: some(s_interval)) then some(s_interval) else None;
    --
    s;
  }

  case range_under_interval_fail {
    approx = Under;
    c =: Constraint_Range(e1, e2);
    approx_expr_max(tenv, e1) -> z1_opt;
    approx_expr_min(tenv, e2) -> z2_opt;
    z1_opt = None || z2_opt = None;
    --
    None;
  }
;

typing function make_interval(approx: constants_set(Over,Under), z1: Z, z2: Z) ->
         (s_opt: option(powerset(Z)))
{
  "returns the interval between the integers {z1} and
  {z2}, or an approximation based on {approx}.",
  prose_application = "",
} =
  case interval {
    z1 <= z2;
    --
    some(range_set(z1, z2));
  }

  case approx {
    z1 > z2;
    approx_bottom_top(approx) -> s_opt;
    --
    s_opt;
  }
;

typing function approx_expr_min(tenv: static_envs, e: expr) ->
         (z_opt: option(Z))
{
  "approximates the minimal integer represented by the
  expression {e} in any environment consisting of the
  \staticenvironmentterm{} {tenv}.
  The result, is an integer of $\None$ if approximation fails.",
  prose_application = "",
} =
  approx_expr(tenv, Over, e) -> some(s) | None;
  --
  some(set_min(s));
;

typing function approx_expr_max(tenv: static_envs, e: expr) ->
         (z_opt: option(Z))
{
  "approximates the maximal integer represented by the
  expression {e} in any environment consisting of the
  \staticenvironmentterm{} {tenv}.
  The result, is an integer of $\None$ if approximation fails.",
  prose_application = "",
} =
  approx_expr(tenv, Over, e) -> some(s) | None;
  --
  some(set_max(s));
;

typing function approx_bottom_top(approx: constants_set(Under,Over)) ->
         (s_opt: option(powerset_finite(Z)))
{
  "returns either the empty set in an optional, if {approx} is $\Under$,
  and $\None$ otherwise.",
  prose_application = "",
} =
  case under {
    --
    some(empty_set);
  }

  case over {
    approx = Over;
    --
    None;
  }
;

typing function intset_to_constraints(s: powerset_finite(Z)) ->
         (cs: list0(int_constraint))
{
  "converts a finite set of integers {s} into an
  equivalent list of constraints.",
  prose_application = "",
} =
  s =: union_list_finite(ranges);
  list_forall(s1, ranges, list_forall(s2, ranges, implies(subseteq(s1, s2), s1 = s2)));
  cs := list_map(range, ranges,
    if (range =: match_set(v))
    then AbbrevConstraintExact(ELint(v))
    else AbbrevConstraintRange(ELint(set_min(range)), ELint(set_max(range))))
  { (_, (_, _, [_]))};
  --
  cs;
;

typing function approx_expr(tenv: static_envs, approx: constants_set(Over,Under), e: expr) ->
         (s_opt: option(powerset_finite(Z)))
{
  "conservatively approximates the expression {e} by a
  set of integers in the \staticenvironmentterm{}
  {tenv}. The approximation is either overapproximation
  or underapproximation, based on the
  \approximationdirectionterm{} {approx}.",
  prose_application = "",
} =
  case literal_int {
    e =: E_Literal(L_Int(z));
    --
    some(make_set(z));
  }

  case literal_non_int {
    e =: E_Literal(l);
    ast_label(l) != label_L_Int;
    approx_bottom_top(approx) -> s_opt;
    --
    s_opt;
  }

  case var_over {
    approx = Over;
    e =: E_Var(x);
    // the following application of type_of is guaranteed not to result in a
    // type error, since symdom_subset is only applied to annotated types.
    type_of(tenv, x) -> t | ;
    approx_type(tenv, Over, t) -> s_opt;
    --
    s_opt;
  }

  case var_under {
    approx = Under;
    e =: E_Var(x);
    --
    some(empty_set);
  }

  case unop {
    e =: E_Unop(op, e1);
    approx_expr(tenv, approx, e1) -> some(s1) | None;
    zs := list_set(s1);
    INDEX(i, zs: unop_literals(op, L_Int(zs[i])) -> L_Int(zs'[i]));
    --
    some(list_to_set(zs'));
  }

  case binop_precise {
    e =: E_Binop(op, e1, e2);
    approx_expr(tenv, approx, e1) -> some(s1) | None;
    approx_expr(tenv, approx, e2) -> some(s2) | None;
    cs1 := intset_to_constraints(s1);
    cs2 := intset_to_constraints(s2);
    approx_constraint_binop(tenv, approx, op, cs1, cs2) -> some((cs, plf)) | None
    { [_] };
    plf = Precision_Full || (plf = Precision_Lost && approx = Under);
    approx_constraints(tenv, approx, cs) -> s_approx;
    --
    s_approx;
  }

  case binop_approx {
    e =: E_Binop(op, e1, e2);
    approx_expr(tenv, approx, e1) -> some(s1);
    approx_expr(tenv, approx, e2) -> some(s2);
    s1 =: s1_set;
    s2 =: s2_set;
    cs1 := intset_to_constraints(s1_set);
    cs2 := intset_to_constraints(s2_set);
    approx_constraint_binop(tenv, approx, op, cs1, cs2) -> some((cs_unused, plf)) | None
    { [_] };
    plf = Precision_Lost && approx = Over;
    --
    None;
  }

  case e_cond {
    e =: E_Cond(test, e2, e3);
    approx_expr(tenv, approx, e2) -> some(s2) | None;
    approx_expr(tenv, approx, e3) -> some(s3) | None;
    s := if approx = Over then union_finite(s2, s3) else intersect_finite(s2, s3);
    --
    some(s);
  }

  case other {
    ast_label(e) not_in make_set(label_E_Binop, label_E_Cond, label_E_Literal, label_E_Unop, label_E_Var);
    approx_bottom_top(approx) -> s_opt;
    --
    s_opt;
  }
;

typing function approx_constraint_binop(tenv: static_envs, approx: constants_set(Over,Under), op: binop, s1: list0(int_constraint), s2: list0(int_constraint)) ->
         option((s: list0(int_constraint), plf: precision_loss_indicator))
{
  "approximates the application of the binary operator
  {op} to lists of constraints {s1} and {s2} with the
  \approximationdirectionterm{} {approx} in the context
  of the static environment {tenv}, resulting in the
  list of constraints and \precisionlossindicatorterm{},
  or $\None$ if the result could not be overapproximated.",
  prose_application = "",
  math_layout = [[_,_,_,_,_],_],
} =
  case over {
    approx = Over;
    annotate_constraint_binop(Over, tenv, op, s1, s2) -> (s, plf);
    --
    some((s, plf));
  }

  case over_fail {
    approx = Over;
    annotate_constraint_binop(Over, tenv, op, s1, s2) -> CannotOverapproximate;
    --
    None;
  }

  case under_precise {
    approx = Under;
    annotate_constraint_binop(Under, tenv, op, s1, s2) -> (s, plf);
    --
    some((s, plf));
  }

  case under_approx {
    approx = Under;
    annotate_constraint_binop(Under, tenv, op, s1, s2) -> CannotUnderapproximate;
    --
    some((empty_list, Precision_Lost));
  }
;

typing function approx_type(tenv: static_envs, approx: constants_set(Over,Under), t: ty) ->
         (s: option(powerset_finite(Z)))
{
  "conservatively approximates the type {t} by a set of
  integers in the \staticenvironmentterm{} {tenv}.
  The approximation is either overapproximation or
  underapproximation, based on the
  \approximationdirectionterm{} {approx}.",
  prose_application = "",
} =
  case named {
    is_named(t) -> True;
    make_anonymous(tenv, t) -> t1;
    approx_type(tenv, approx, t1) -> s;
    --
    s;
  }

  case int_wellconstrained {
    t =: T_Int(WellConstrained(cs));
    approx_constraints(tenv, approx, cs) -> s_opt;
    --
    s_opt;
  }

  case other {
    not_single(is_named(t)) && not_single(is_well_constrained_integer(t));
    approx_bottom_top(approx) -> s_opt;
    --
    s_opt;
  }
;

typing function constraint_binop(op: binop, cs1: list0(int_constraint), cs2: list0(int_constraint)) ->
         (new_cs: list0(int_constraint))
{
  "symbolically applies the binary operation {op} to the
  lists of integer constraints {cs1} and {cs2}, yielding
  the integer constraints {new_cs}.",
  prose_application = "\hyperlink{relation-constraintbinop}{applying} operator {op}
                        to constraints {cs1} and {cs2} yields constraints {new_cs}",
} =
  case extremities {
    op in make_set(ADD, DIV, DIVRM, MUL, SHL, SHR, SUB);
    cs := list_cross(cs1, cs2);
    cs =: list_combine(cs_fst, cs_snd);
    extremities := list_map(i, indices(cs), apply_binop_extremities(op, cs_fst[i], cs_snd[i]))
    { [_] };
    --
    list_flatten(extremities);
  }

  case mod {
    op = MOD;
    new_cs := list_map(c, cs2, constraint_mod(c));
    --
    new_cs;
  }

  case pow {
    op = POW;
    cs := list_cross(cs1, cs2);
    cs =: list_combine(cs_fst, cs_snd);
    pow_cs := list_map(i, indices(cs), constraint_pow(cs_fst[i], cs_snd[i])) { [_] };
    --
    list_flatten(pow_cs);
  }
;

typing function apply_binop_extremities(op: binop, c1: int_constraint, c2: int_constraint) ->
         (new_cs: list0(int_constraint))
{
  "yields a list of constraints {new_cs} for the
  constraints {c1} and {c2}, which are needed to include
  range constraints for cases where the binary operation
  {op} yields a \dynamicerrorterm{}.",
  prose_application = "",
};

typing function possible_extremities_left(op: binop, a: expr, b: expr) ->
         (extpairs: list0((expr, expr)))
{
  "yields a list of pairs of expressions {extpairs} given
  the binary operation {op} and pair of expressions {a}
  and {b}, which are needed to form constraints for
  cases where applying {op} to {a} and {b} would lead to
  a \dynamicerrorterm{}.",
  prose_application = "",
} =
  case mul {
    op = MUL;
    --
    make_list((a, a), (a, b), (b, a), (b, b));
  }

  case other {
    op in make_set(ADD, DIV, DIVRM, SHL, SHR, SUB);
    --
    make_singleton_list((a, b));
  }
;

typing function possible_extremities_right(op: binop, c: expr, d: expr) ->
         (extpairs: list0((expr, expr)))
{
  "yields a list of pairs of expressions {extpairs} given
  the binary operation {op} and pair of expressions {c}
  and {d}, which are needed to form constraints for
  cases where applying {op} to {c} and {d} would lead to
  a \dynamicerrorterm{}.",
  prose_application = "",
} =
  case plus {
    op = ADD;
    --
    make_singleton_list((c, d));
  }

  case minus {
    op = SUB;
    --
    make_singleton_list((d, c));
  }

  case mul {
    op = MUL;
    --
    make_list((c, c), (c, d), (d, c), (d, d));
  }

  case shl_shr {
    op in make_set(SHL, SHR);
    --
    make_list((d, ELint(zero)), (ELint(zero), d));
  }

  case div_divrm {
    op in make_set(DIV, DIVRM);
    --
    make_list((d, ELint(one)), (ELint(one), d));
  }
;

typing function constraint_mod(c: int_constraint) ->
         (new_c: int_constraint)
{
  "yields a range constraint {new_c} from $0$ to the
  expression in {c} that is maximal. This is needed to
  apply the modulus operation to a pair of constraints.",
  prose_application = "",
} =
  case exact {
    c =: Constraint_Exact(e);
    --
    Constraint_Range(ELint(zero), AbbrevEBinop(SUB, e, ELint(one)));
  }

  case range {
    c =: Constraint_Range(_, e);
    --
    Constraint_Range(ELint(zero), AbbrevEBinop(SUB, e, ELint(one)));
  }
;

typing function constraint_pow(c1: int_constraint, c2: int_constraint) ->
         (new_cs: list1(int_constraint))
{
  "yields a list of range constraints {new_cs} that are
  needed to calculate the result of applying a $\POW$
  operation to the constraints {c1} and {c2}.",
  prose_application = "\hyperlink{relation-constraintpow}{symbolically applying} the $\POW$
                        operation to {c1} and {c2} yields constraint list {new_cs}",
} =
  case exact_exact {
    c1 =: Constraint_Exact(a);
    c2 =: Constraint_Exact(c);
    --
    make_singleton_list(AbbrevConstraintExact(AbbrevEBinop(POW, a, c)));
  }

  case range_exact {
    c1 =: Constraint_Range(a, b);
    c2 =: Constraint_Exact(c);
    mac := AbbrevEBinop(POW, E_Unop(NEG, a), c);
    --
    make_list(
      AbbrevConstraintRange(ELint(zero), AbbrevEBinop(POW, b, c)),
      AbbrevConstraintRange(E_Unop(NEG, mac), mac))
    { [_] };
  }

  case exact_range {
    c1 =: Constraint_Exact(a);
    c2 =: Constraint_Range(c, d);
    mad := AbbrevEBinop(POW, E_Unop(NEG, a), d);
    --
    make_list(
      AbbrevConstraintRange(ELint(zero), AbbrevEBinop(POW, a, d)),
      AbbrevConstraintRange(E_Unop(NEG, mad), mad),
      AbbrevConstraintExact(ELint(one)))
      { [_]} ;
  }

  case range_range {
    c1 =: Constraint_Range(a, b);
    c2 =: Constraint_Range(c, d);
    mad := AbbrevEBinop(POW, E_Unop(NEG, a), d);
    --
    make_list(
      AbbrevConstraintRange(ELint(zero), AbbrevEBinop(POW, b, d)),
      AbbrevConstraintRange(E_Unop(NEG, mad), mad),
      AbbrevConstraintExact(ELint(one)))
    { [_] };
  }
;

//////////////////////////////////////////////////
// Relations for Type Attributes

typing function is_unconstrained_integer(t: ty) -> Bool
{
  "Tests whether {t} is the unconstrained integer type.",
  prose_application = "",
} =
  --
  t = T_Int(Unconstrained);
;

typing function is_parameterized_integer(t: ty) -> Bool
{
  "Tests whether {t} is a parameterized integer type.",
  prose_application = "",
} =
  --
  t = T_Int(Parameterized(_));
;

typing function is_well_constrained_integer(t: ty) -> Bool
{
  "Tests whether {t} is a well-constrained integer type.",
  prose_application = "",
} =
  --
  t = T_Int(WellConstrained(_));
;

typing function is_builtin_singular(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \emph{builtin singular type}, yielding the result in {b}.",
    prose_application = "testing whether {ty} is a builtin singular type yields {b}",
} =
  b := (ast_label(ty) in make_set(label_T_Bits, label_T_Bool, label_T_Enum, label_T_Int, label_T_Real, label_T_String));
  --
  b;
;

typing function is_named(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \emph{named type}.",
    prose_application = "testing whether {ty} is a named type yields {b}",
} =
  --
  ast_label(ty) = label_T_Named;
;

typing function is_anonymous(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is an \emph{\anonymoustype}.",
    prose_application = "testing whether {ty} is an \anonymoustype{} yields {b}",
} =
  --
  not_equal(ast_label(ty), label_T_Named);
;

typing function is_singular(tenv: static_envs, ty: ty) -> (b: Bool) | type_error
{
    "tests whether the type {ty} is a \emph{\singulartypeterm} in the \staticenvironmentterm{} {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "tests whether {ty} is a \singulartypeterm{} in {tenv},
    yields {b}\ProseOrTypeError",
} =
  make_anonymous(tenv, ty) -> t1;
  is_builtin_singular(t1) -> b;
  --
  b;
;

typing function is_structured(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \structuredtypeterm{}.",
    prose_application = "testing whether {ty} is a \structuredtypeterm{} yields {b}",
} =
  --
  ast_label(ty) in make_set(label_T_Collection, label_T_Exception, label_T_Record);
;

typing function get_structure(tenv: static_envs, ty: ty) ->
         (t: ty) | type_error
{
  "assigns a type to its
  \hypertarget{def-tstruct}{\emph{\structureterm}},
  which is the type formed by recursively replacing
  named types by their type definition in the
  \staticenvironmentterm{} {tenv}. If a named type is
  not associated with a declared type in {tenv}, a
  \typingerrorterm{} is returned.",
  prose_application = "",
} =
  case named {
    ty =: T_Named(x);
    declared_type(tenv, x) -> t1;
    get_structure(tenv, t1) -> t;
    --
    t;
  }

  case builtin_singular {
    is_builtin_singular(ty) -> True;
    --
    ty;
  }

  case tuple {
    ty =: T_Tuple(tys);
    INDEX(i, tys: get_structure(tenv, tys[i]) -> tys'[i]);
    --
    T_Tuple(tys');
  }

  case array {
    ty =: T_Array(e, t);
    get_structure(tenv, t) -> t1;
    --
    T_Array(e, t1);
  }

  case structured {
    is_structured(ty);
    ty =: make_structured(L, fields);
    list_combine(names, types) := fields;
    INDEX(i, types: get_structure(tenv, types[i]) -> types'[i]);
    --
    make_structured(L, list_combine(names, types'));
  }
;

typing function make_anonymous(tenv: static_envs, ty: ty) ->
         (t: ty) | type_error
{
  "returns the \emph{\underlyingtypeterm} {t} of
  the type {ty} in the \staticenvironmentterm{} {tenv}
  or a \typingerrorterm{}. Intuitively, {ty} is the
  first non-named type that is used to define {ty}.
  Unlike $\tstruct$, $\makeanonymous$ replaces named
  types by their definition until the first non-named
  type is found but does not recurse further.",
  prose_application = "",
} =
  case named {
    ty =: T_Named(x);
    declared_type(tenv, x) -> t1;
    make_anonymous(tenv, t1) -> t;
    --
    t;
  }

  case non_named {
    not_equal(ast_label(ty), label_T_Named);
    --
    ty;
  }
;

typing function check_constrained_integer(tenv: static_envs, t: ty) ->
         CheckResult | type_error
{
  "checks whether the type {t} is a
  \constrainedintegerterm{} type. If so, the result is
  $\True$, otherwise the result is a \typingerrorterm.",
  prose_application = "",
} =
  case well_constrained {
    t = T_Int(WellConstrained(_));
    --
    True;
  }

  case parameterized {
    is_parameterized_integer(t);
    --
    True;
  }

  case unconstrained {
    t =: T_Int(c);
    ast_label(c) = label_Unconstrained || ast_label(c) = label_PendingConstrained;
    --
    TypeError(TE_UT);
  }

  case conflicting_type {
    not_equal(ast_label(t), label_T_Int);
    --
    TypeError(TE_UT);
  }
;

//////////////////////////////////////////////////
// Relations for Type Declarations

typing relation declare_type(
  genv: global_static_envs,
  name: Identifier,
  ty: ty,
  s: option((Identifier, list0(field)))) ->
         (new_genv: global_static_envs,
         t2: ty,
         s': option((Identifier, list0(field)))) | type_error
{
  "declares a type named {name} with type {ty} and
  \optionalterm{} additional fields over another type {s} in
  the \globalstaticenvironmentterm{} {genv}, resulting
  in the modified \globalstaticenvironmentterm{}
  {new_genv}, annotated type {t2}, and annotated
  \optionalterm{} additional fields {s'}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  check_var_not_in_genv(genv, name) -> True;
  with_empty_local(genv) -> tenv;
  annotate_type(True, tenv, ty) -> (t1, ses_t);
  annotate_extra_fields(tenv, name, t1, s) -> (tenv1, t2, s');
  ses_is_pure(ses_t) -> b;
  purity := if_then_else(b, SE_Pure, SE_Readonly);
  add_type(tenv1, name, t2, purity) -> tenv2;
  case enum {
    t2 =: T_Enum(ids);
    declare_enum_labels(tenv2, name, ids) -> tenv3;
    --
    (tenv3.static_envs_G, t2, s');
  }

  case not_enum {
    ast_label(t2) != label_T_Enum;
    --
    (tenv2.static_envs_G, t2, s');
  }
;

typing relation annotate_extra_fields(tenv: static_envs, name: Identifier, ty: ty, s: option((super: Identifier, extra_fields: list0(field)))) ->
         (new_tenv: static_envs, new_ty: ty, s': option((Identifier, list0(field)))) | type_error
{
  "annotates the type {ty} with the \optionalterm{} extra
  fields {s} in {tenv}, yielding the modified
  environment {new_tenv}, type {new_ty}, and \optionalterm{}
  extra fields {s'}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case none {
    s = None;
    --
    (tenv, ty, None);
  }

  case empty_fields {
    s =: some((super_name, extra_fields'));
    subtype_satisfies(tenv, ty, T_Named(super_name)) -> b;
    te_check(b, TE_UT) -> True;
    extra_fields' = empty_list;
    updated_subtypes := map_update(tenv.static_envs_G.subtypes, name, super_name);
    new_genv := tenv.static_envs_G(subtypes: updated_subtypes);
    new_tenv := tenv(static_envs_G: new_genv);
    --
    (new_tenv, ty, some((super_name, empty_list)))
    { [_] };
  }

  case no_super {
    s =: some((super_name, extra_fields'));
    subtype_satisfies(tenv, ty, T_Named(super_name)) -> b;
    te_check(b, TE_UT) -> True;
    extra_fields' != empty_list;
    tenv.static_envs_G.declared_types(super_name) = bot;
    --
    TypeError(TE_UI);
  }

  case structured {
    s =: some((super_name, extra_fields'));
    subtype_satisfies(tenv, ty, T_Named(super_name)) -> b;
    te_check(b, TE_UT) -> True;
    extra_fields' != empty_list;
    tenv.static_envs_G.declared_types(super_name) =: (t_super, _);
    te_check(ast_label(t_super) in make_set(label_T_Exception, label_T_Record), TE_UT) -> True
    { (((_, [_]), _), _) };
    t_super =: make_structured(L, fields);
    new_ty := make_structured(L, concat(fields, extra_fields'));
    updated_subtypes := map_update(tenv.static_envs_G.subtypes, name, super_name);
    new_genv := tenv.static_envs_G(subtypes: updated_subtypes);
    new_tenv := tenv(static_envs_G: new_genv);
    --
    (new_tenv, new_ty, some((super_name, empty_list)))
    { [_] };
  }
;

typing relation declared_type(tenv: static_envs, id: Identifier) ->
         (t: ty) | type_error
{
  "retrieves the type associated with the identifier {id}
  in the \staticenvironmentterm{} {tenv}. If the
  identifier is not associated with a declared type, the
  result is a \typingerrorterm.",
  prose_application = "",
} =
  case exists {
    tenv.static_envs_G.declared_types(id) =: (t, _);
    --
    t;
  }

  case type_not_declared {
    tenv.static_envs_G.declared_types(id) = bot;
    --
    TypeError(TE_UI);
  }
;

typing function declare_enum_labels(tenv: static_envs, name: Identifier, ids: list1(Identifier)) ->
         (new_tenv: static_envs) | type_error
{
  "updates the \staticenvironmentterm{} {tenv} with the
  identifiers {ids} listed by an \enumerationtypeterm{},
  yielding the modified environment {new_tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case empty {
    ids = empty_list;
    --
    tenv;
  }

  case non_empty {
    ids =: match_non_empty_cons(id, ids1);
    declare_const(tenv.static_envs_G, id, T_Named(name), L_Label(id)) -> genv1;
    tenv1 := tenv(static_envs_G: genv1);
    declare_enum_labels(tenv1, name, match_non_empty_list(ids1)) -> new_tenv;
    --
    new_tenv;
  }
;

typing function declare_const(genv: global_static_envs, name: Identifier, ty: ty, v: literal) ->
         (new_genv: global_static_envs) | type_error
{
  "adds a constant given by the identifier {name}, type
  {ty}, and literal {v} to the
  \globalstaticenvironmentterm{} {genv}, yielding the
  modified environment {new_genv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  add_global_storage(genv, name, GDK_Constant, ty) -> genv1;
  add_global_constant(genv1, name, v) -> new_genv;
  --
  new_genv;
;

//////////////////////////////////////////////////
// Relations for Types
//////////////////////////////////////////////////

typing relation annotate_type(decl: Bool, tenv: static_envs, ty: ty) ->
         (new_ty: ty, ses: powerset(TSideEffect)) | type_error
{
  "typechecks a type {ty} in a \staticenvironmentterm{}
  {tenv}, resulting in a \typedast{} {new_ty} and a
  \sideeffectsetterm{} {ses}. The flag {decl} indicates
  whether {ty} is a type currently being declared or
  not, and makes a difference only when {ty} is an
  \enumerationtypeterm{} or a \structuredtypeterm.
  \ProseOtherwiseTypeError",
  prose_application = "",
} =
  case t_string {
    ty = T_String;
    --
    (ty, empty_set);
  }

  case t_real {
    ty = T_Real;
    --
    (ty, empty_set);
  }

  case t_bool {
    ty = T_Bool;
    --
    (ty, empty_set);
  }

  case t_named {
    ty =: T_Named(x);
    te_check(tenv.static_envs_G.declared_types(x) != bot, TE_UI) -> True;
    tenv.static_envs_G.declared_types(x) =: (_, purity);
    ses := make_set(GlobalEffect(purity), Immutability(True));
    --
    (ty, ses);
  }

  case t_int {
    case pending_constrained {
      ty = T_Int(PendingConstrained);
      --
      TypeError(TE_UT);
    }

    // The case of an empty list of constraints is rejected by the parser.
    case well_constrained {
      ty =: T_Int(WellConstrained(constraints));
      ( INDEX(i, constraints: annotate_constraint(tenv, constraints[i]) -> (new_constraints[i], sess[i])) )
      { ([_]) };
      ses := union_list(sess);
      --
      (T_Int(typed_WellConstrained(match_non_empty_list(new_constraints), Precision_Full)), ses)
      { [_] };
    }

    case parameterized {
      is_parameterized_integer(ty);
      ses := make_set(Immutability(True), LocalEffect(SE_Pure));
      --
      (ty, ses);
    }

    case unconstrained {
      ty = unconstrained_integer;
      --
      (ty, empty_set);
    }
  }

  case t_bits {
    ty =: T_Bits(e_width, bitfields);
    annotate_expr(tenv, e_width) -> (t_width, e_width', ses_width);
    check_symbolically_evaluable(ses_width) -> True;
    check_constrained_integer(tenv, t_width) -> True;
    case with_bitfields {
      bitfields != empty_list;
      te_check(ses_is_pure(ses_width), TE_SEV) -> True;
      annotate_bitfields(tenv, e_width', bitfields) -> (bitfields', ses_bitfields)
      { [_] };
      static_eval(tenv, e_width') -> L_Int(width);
      check_common_bitfields_align(tenv, bitfields', z_to_n(width)) -> True;
    }

    case no_bitfields {
      bitfields = empty_list;
      bitfields' := empty_list;
      ses_bitfields := empty_set;
    }
    ses := union(ses_width, ses_bitfields);
    --
    (T_Bits(e_width', empty_list), ses_width);
  }

  case t_tuple {
    ty =: T_Tuple(tys);
    INDEX(i, tys: annotate_type(False, tenv, tys[i]) -> (tys'[i], sess[i]));
    ses := union_list(sess);
    --
    (T_Tuple(tys'), ses);
  }

  case t_array {
    ty =: T_Array(index, t);
    annotate_type(False, tenv, t) -> (t', ses_t);
    index =: ArrayLength_Expr(e);
    case expr_is_enum {
      get_variable_enum(tenv, e) -> some((s, labels));
      index' := ArrayLength_Enum(s, labels);
      ses_index := empty_set;
    }

    case expr_not_enum {
      get_variable_enum(tenv, e) -> None;
      annotate_symbolic_constrained_integer(tenv, e) -> (e', ses_index);
      index' := ArrayLength_Expr(e');
    }
    ses := union(ses_t, ses_index);
    --
    (T_Array(index', t'), ses)
    { [_] };
  }

  case tstructureddecl {
    ty =: make_structured(L, fields);
    fields =: list_combine(field_names, field_types);
    check_no_duplicates(field_names) -> True;
    ( INDEX(i, field_types: annotate_type(False, tenv, field_types[i]) -> (tys[i], sess[i])) )
    { ([_]) };
    ses := union_list(sess);
    case record_exception {
      L in make_set(label_T_Exception, label_T_Record);
      decl = True;
      fields' := list_combine(field_names, tys);
      --
      (make_structured(L, fields'), ses);
    }

    case collection {
      L = label_T_Collection;
      decl = False;
      INDEX(i, tys: check_structure_label(tenv, tys[i], label_T_Bits) -> True);
      fields' := list_map(i, indices(fields), (field_names[i], tys[i]));
      --
      (T_Collection(fields'), ses);
    }
  }

  case t_enum_decl {
    decl = True;
    ty =: T_Enum(li);
    check_no_duplicates(li) -> True;
    INDEX(i, li: check_var_not_in_genv(tenv.static_envs_G, li[i]) -> True);
    --
    (T_Enum(li), empty_set);
  }

  case t_non_decl {
    decl = False;
    ast_label(ty) in make_set(label_T_Enum, label_T_Exception, label_T_Record);
    --
    TypeError(TE_UT);
  }
;

render rule annotate_type_t_int = annotate_type(t_int);
render rule annotate_type_t_real = annotate_type(t_real);
render rule annotate_type_t_string = annotate_type(t_string);
render rule annotate_type_t_bool = annotate_type(t_bool);
render rule annotate_type_t_bits = annotate_type(t_bits);
render rule annotate_type_t_tuple = annotate_type(t_tuple);
render rule annotate_type_t_enum_decl = annotate_type(t_enum_decl);
render rule annotate_type_t_array = annotate_type(t_array);
render rule annotate_type_t_record_exception = annotate_type(t_record_exception);
render rule annotate_type_t_collection = annotate_type(t_collection);
render rule annotate_type_t_named = annotate_type(t_named);
render rule annotate_type_t_non_decl = annotate_type(t_non_decl);

typing relation annotate_constraint(tenv: static_envs, c: int_constraint) ->
         (new_c: int_constraint, ses: powerset(TSideEffect)) | type_error
{
  "annotates an integer constraint {c} in the
  \staticenvironmentterm{} {tenv} yielding the annotated
  integer constraint {new_c} and \sideeffectsetterm\
  {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  case exact {
    c =: Constraint_Exact(e);
    annotate_symbolic_constrained_integer(tenv, e) -> (e', ses);
    --
    (Constraint_Exact(e'), ses);
  }

  case range {
    c =: Constraint_Range(e1, e2);
    annotate_symbolic_constrained_integer(tenv, e1) -> (e1', ses1);
    annotate_symbolic_constrained_integer(tenv, e2) -> (e2', ses2);
    ses := union(ses1, ses2);
    --
    (Constraint_Range(e1', e2'), ses);
  }
;

typing function get_variable_enum(tenv: static_envs, e: expr) ->
         (option((x: Identifier, labels: list1(Identifier))))
{
  "tests whether the expression {e} represents a variable
  of an \enumerationtypeterm{}. If so, the result is {x}
  --- the name of the variable and the list of labels
  {labels}, declared for the \enumerationtypeterm{}.
  Otherwise, the result is $\None$.",
  prose_application = "",
} =
  case not_evar {
    ast_label(e) != label_E_Var;
    --
    None;
  }

  case undeclared {
    e =: E_Var(x);
    tenv.static_envs_G.declared_types(x) = bot;
    --
    None;
  }

  case declared_enum {
    e =: E_Var(x);
    tenv.static_envs_G.declared_types(x) =: (t, _);
    make_anonymous(tenv, t) -> t1;
    t1 =: T_Enum(labels);
    --
    some((x, labels));
  }

  case declared_not_enum {
    e =: E_Var(x);
    tenv.static_envs_G.declared_types(x) =: (t, _);
    make_anonymous(tenv, t) -> t1;
    ast_label(t1) != label_T_Enum;
    --
    None;
  }
;

typing relation annotate_symbolically_evaluable_expr(tenv: static_envs, e: expr) ->
         (t: ty, e': expr, ses: powerset(TSideEffect)) | type_error
{
  "annotates the expression {e} in the
  \staticenvironmentterm{} {tenv} and checks that it is
  \symbolicallyevaluableterm, yielding the type in {t},
  the annotated expression {e'} and the
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
} =
  annotate_expr(tenv, e) -> (t, e', ses);
  check_symbolically_evaluable(ses) -> True;
  --
  (t, e', ses);
;

typing function check_underlying_integer(tenv: static_envs, t: ty) -> CheckResult | type_error
{
  "returns $\True$ if {t} has the \underlyingtypeterm{} of an \integertypeterm{} in the \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "checking whether {t} has the \underlyingtypeterm{} of an \integertypeterm{} in {tenv} yields
  $\True$\ProseOrTypeError",
} =
  make_anonymous(tenv, t) -> t';
  te_check(ast_label(t') = label_T_Int, TE_UT) -> True;
  --
  True;
;

//////////////////////////////////////////////////
// Relations for Type System Utilities

typing function check_no_duplicates(ids: list0(Identifier)) ->
         CheckResult | type_error
{
  "checks whether the possibly-empty list of identifiers {ids}
  contains a duplicate identifier. If it does not, the
  result is $\True$ and otherwise the result is a
  \typingerrorterm{}.",
  prose_application = "",
} =
  case okay {
    unique_list(ids) -> ids1;
    same_length(ids1, ids);
    --
    True;
  }

  case error {
    unique_list(ids) -> ids1;
    list_len(ids1) != list_len(ids);
    --
    TypeError(TE_IAD);
  }
;

typing function find_bitfield_opt(name: Identifier, bitfields: list0(bitfield)) ->
         (r: option(bitfield))
{
  "returns the bitfield associated with the name {name}
  in the list of bitfields {bitfields}, if there is one.
  Otherwise, the result is $\None$.",
  prose_application = "",
} =
  case match {
    bitfields =: match_cons(first_bf, _);
    bitfield_get_name(first_bf) -> first_name;
    name = first_name;
    --
    some(first_bf);
  }

  case tail {
    bitfields =: match_cons(first_bf, bitfields');
    bitfield_get_name(first_bf) -> first_name;
    name != first_name;
    find_bitfield_opt(name, bitfields') -> r;
    --
    r;
  }

  case empty {
    bitfields = empty_list;
    --
    None;
  }
;

typing function type_of_array_length(size: array_index) ->
         (t: ty)
{
  "returns the type for the array length {size} in {t}.",
  prose_application = "",
} =
  case enum {
    size =: ArrayLength_Enum(name, _);
    --
    T_Named(name);
  }

  case expr {
    size = ArrayLength_Expr(_);
    --
    unconstrained_integer;
  }
;

typing function with_empty_local(genv: global_static_envs) ->
         (tenv: static_envs)
{
  "constructs a \staticenvironmentterm{} from the
  \globalstaticenvironmentterm{} {genv} and the empty
  \localstaticenvironmentterm.",
  prose_application = "",
} =
  --
  [static_envs_G: genv, static_envs_L: empty_tenv.static_envs_L];
;

typing function check_var_not_in_env(tenv: static_envs, id: Strings) ->
         CheckResult | type_error
{
  "checks whether {id} is already declared in {tenv}. If
  it is, the result is a \typingerrorterm{}, and
  otherwise the result is $\True$.",
  prose_application = "",
} =
  te_check(is_undefined(tenv, id), TE_IAD) -> True;
  --
  True;
;

typing function check_var_not_in_genv(genv: global_static_envs, id: Strings) ->
         CheckResult | type_error
{
  "checks whether {id} is already declared in the
  \globalstaticenvironmentterm{} {genv}. If it is, the
  result is a \typingerrorterm{}, and otherwise the
  result is $\True$.",
  prose_application = "",
} =
  te_check(is_global_undefined(genv, id), TE_IAD) -> True;
  --
  True;
;

typing function add_local(tenv: static_envs, id: Identifier, ty: ty, ldk: local_decl_keyword) ->
         (new_tenv: static_envs)
{
  "adds the identifier {id} as a local storage element
  with type {ty} and local declaration keyword {ldk} to
  the local environment of {tenv}, resulting in the
  \staticenvironmentterm{} {new_tenv}.",
  prose_application = "",
} =
  updated_map := map_update(tenv.static_envs_L.local_storage_types, id, (ty, ldk));
  new_lenv := tenv.static_envs_L(local_storage_types: updated_map);
  --
  tenv(static_envs_L: new_lenv);
;

typing function is_undefined(tenv: static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether the identifier {x} is defined as a
  storage element in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "",
} =
  --
  (is_global_undefined(tenv.static_envs_G, x) && is_local_undefined(tenv.static_envs_L, x))
  { [_] };
;

typing function is_global_undefined(genv: global_static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether the identifier {x} is defined in the
  \globalstaticenvironmentterm{} {genv} when subprogram
  definitions are ignored (see
  \RequirementRef{GlobalNamespace}), yielding the result
  in {b}.",
  prose_application = "",
} =
  b := (genv.global_storage_types(x) = bot) && (genv.declared_types(x) = bot);
  --
  b;
;

typing function is_local_undefined(lenv: local_static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether {x} is declared as a local storage
  element in the static local environment {lenv},
  yielding the result in {b}.",
  prose_application = "",
} =
  --
  lenv.local_storage_types(x) = bot;
;

typing function lookup_constant(tenv: static_envs, s: Identifier) ->
         (v_opt: option(literal))
{
  "looks up the environment {tenv} for a constant
  associated with an identifier {s}. The result is
  $\None$ if {s} is not associated with any constant.",
  prose_application = "",
} =
  tenv.static_envs_G.constant_values(s) =: c;
  case exists {
    c != bot;
    --
    some(c);
  }

  case does_not_exist {
    c = bot;
    --
    None;
  }
;

typing function add_global_constant(genv: global_static_envs, name: Identifier, v: literal) ->
         (new_genv: global_static_envs)
{
  "binds the identifier {name} to the literal {v} in the
  \globalstaticenvironmentterm{} {genv}, yielding the
  updated \globalstaticenvironmentterm{} {new_genv}.",
  prose_application = "",
} =
  updated_map := map_update(genv.constant_values, name, v);
  --
  genv(constant_values: updated_map);
;

typing function lookup_immutable_expr(tenv: static_envs, x: Identifier) ->
         (e: option(expr))
{
  "looks up the \staticenvironmentterm{} {tenv} for an
  immutable expression associated with the identifier
  {x}, returning $\bot$ if there is none.",
  prose_application = "",
} =
  case local {
    map_apply_opt(tenv.static_envs_L.local_expr_equiv, x) =: some(e);
    --
    some(e);
  }

  case global {
    map_apply_opt(tenv.static_envs_L.local_expr_equiv, x) =: some(e);
    --
    some(e);
  }

  case none {
    map_apply_opt(tenv.static_envs_L.local_expr_equiv, x) = None;
    map_apply_opt(tenv.static_envs_L.local_expr_equiv, x) = None;
    --
    None;
  }
;

typing function add_global_immutable_expr(tenv: static_envs, x: Identifier, e: expr) ->
         (new_tenv: static_envs)
{
  "binds the identifier {x}, which is assumed to name a
  global storage element, to the expression {e}, which
  is assumed to be \symbolicallyevaluableterm, in the
  \staticenvironmentterm{} {tenv}, resulting in the
  updated environment {new_tenv}.",
  prose_application = "",
} =
  updated_map := map_update(tenv.static_envs_G.global_expr_equiv, x, e);
  new_genv := tenv.static_envs_G(global_expr_equiv: updated_map);
  --
  tenv(static_envs_G: new_genv);
;

typing function add_local_immutable_expr(tenv: static_envs, x: Identifier, e: expr) ->
         (new_tenv: static_envs)
{
  "binds the identifier {x}, which is assumed to name a
  local storage element, to the expression {e}, which is
  assumed to be \symbolicallyevaluableterm, in the
  \staticenvironmentterm{} {tenv}, resulting in the
  updated environment {new_tenv}.",
  prose_application = "",
} =
  updated_map := map_update(tenv.static_envs_L.local_expr_equiv, x, e);
  new_lenv := tenv.static_envs_L(local_expr_equiv: updated_map);
  --
  tenv(static_envs_L: new_lenv);
;

typing function should_remember_immutable_expression(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether the \sideeffectsetterm{} {ses} allows an
  expression with those \sideeffectdescriptorsterm{} to
  be recorded as an immutable expression in the
  appropriate $\exprequiv$ map component of the
  \staticenvironmentterm{}, so that it can later be used
  to reason about type satisfaction, yielding the result
  in {b}.",
  prose_application = "",
} =
  --
  is_symbolically_evaluable(ses);
;

typing function add_immutable_expression(
    tenv: static_envs,
    ldk: local_decl_keyword,
    e_opt: option((e: expr, ses_e: powerset(TSideEffect))), x: Identifier) ->
         (new_tenv: static_envs) | type_error
{
  "conditionally updates the \staticenvironmentterm{}
  {tenv} for a \localdeclarationkeyword{} {ldk}, an
  optional pair {e_opt} consisting of an expression and
  its associated \sideeffectdescriptorsterm{}, and an
  identifier {x}, yielding the updated
  \staticenvironmentterm{} {new_tenv}. More precisely,
  $\addimmutableexpression(\tenv, \ldk, \veopt, \vx)$
  associates an expression with the identifier {x} in
  the \staticenvironmentterm{} {tenv}, if one exists in
  {e_opt} and it is \symbolicallyevaluableterm{} with
  respect to the \sideeffectsetterm{} {ses_e}, along with
  the local declaration keyword {ldk}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \addimmutableexpression,
  math_layout = [[_,_,_,_],_],
} =
  case remember {
    ldk = LDK_Let;
    e_opt =: some((e1, ses_e1));
    should_remember_immutable_expression(ses_e1) -> True;
    normalize_opt(tenv, e1) -> some(e');
    add_local_immutable_expr(tenv, x, e') -> new_tenv;
    --
    new_tenv;
  }

  case skip_not_let {
    ldk != LDK_Let;
    --
    tenv;
  }

  case skip_none {
    ldk = LDK_Let;
    e_opt = None;
    --
    tenv;
  }

  case normalize_failed {
    ldk = LDK_Let;
    e_opt =: some((e1, ses_e1));
    should_remember_immutable_expression(ses_e1) -> True;
    normalize_opt(tenv, e1) -> None;
    --
    tenv;
  }

  case skip_not_symbolic {
    ldk = LDK_Let;
    e_opt =: some((expr1, ses_e1));
    should_remember_immutable_expression(ses_e1) -> False;
    --
    tenv;
  }
;

typing function add_subprogram(tenv: static_envs, name: Strings, func_def: func, s: powerset(TSideEffect)) ->
         (new_tenv: static_envs)
{
  "updates the global environment of {tenv} by mapping
  the (unique) subprogram identifier {name} to the
  function definition {func_def} and
  \sideeffectdescriptorsterm{} {s} in {tenv}, resulting
  in a new \staticenvironmentterm{} {new_tenv}.",
  prose_application = "",
} =
  updated_map := map_update(tenv.static_envs_G.subprogram, name, (func_def, s));
  new_genv := tenv.static_envs_G(subprogram: updated_map);
  --
  tenv(static_envs_G: new_genv);
;

typing function add_type(tenv: static_envs, name: Identifier, ty: ty, f: TPurity) ->
         (new_tenv: static_envs)
{
  "binds the type {ty} and \purity{} {f} to the
  identifier {name} in the \staticenvironmentterm{}
  {tenv}, yielding the modified \staticenvironmentterm{}
  {new_tenv}.",
  prose_application = "",
} =
  updated_map := map_update(tenv.static_envs_G.declared_types, name, (ty, f));
  new_genv := tenv.static_envs_G(declared_types: updated_map);
  --
  tenv(static_envs_G: new_genv);
;
