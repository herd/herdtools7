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

typedef empty_set
{
    "the empty set",
    math_macro = \emptyset,
};

constant True { "true", math_macro = \True };
constant False { "false", math_macro = \False };

typedef Bool
{  "Boolean",
    math_macro = \Bool,
} = constants_set(True, False)
;

typedef Bit
{ "bit" };

typedef N
{  "natural number",
    math_macro = \N,
};

typedef N_pos
{  "positive natural number",
    math_macro = \Npos,
};

typedef Z
{ "integer",
    math_macro = \Z,
};

typedef Q
{ "rational",
   math_macro = \Q,
};

typedef Identifier
{ "identifier",
   math_macro = \Identifier,
};

typedef Strings
{ "string",
   math_macro = \Strings,
};

typedef ASTLabels
{ "AST label",
   math_macro = \ASTLabels,
};

constant bot { "bottom", math_macro = \bot };

typedef TAbsField { "\absolutebitfields", math_macro = \TAbsField } =
    (name: list0(Identifier), slice: list0(N))
    { "absolute field named {name} with slice {slice}" }
;

typedef def_use_name { "subprogram identifier kind" } =
    | Subprogram(id: Identifier)
    { "subprogram identifier {id}" }
    | Other(id: Identifier)
    { "non-subprogram identifier {id}" }
;

////////////////////////////////////////////////////////////////////////////////
// Types for Symbolic Equivalence Testing
constant negative_sign { "negative sign", math_macro = \negativesign };
constant positive_sign { "positive sign", math_macro = \positivesign };
constant equal_sign { "equal sign", math_macro = \equalsign };
typedef Sign { "sign" } =
    constants_set(negative_sign, positive_sign, equal_sign)
;
typedef Q_nonzero { "non-zero rational", math_macro = \Qnonzero };
typedef unitary_monomial { "unitary monomial" } = partial Identifier -> N_pos;
typedef polynomial { "polynomial" } = partial unitary_monomial -> Q_nonzero;
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
    | E_Binop(operator: binop, left: expr, right: expr)
    { "binary expression for the operator {operator}, left expression {left} and right expression {right}" }
    | E_Unop(operator: unop, subexpression: expr)
    { "unary expression for the unary operator {operator} and subexpression {subexpression}" }
    | E_Call(call_descriptor: call)
    { "call expression for the call descriptor {call_descriptor}" }
    | E_Slice(base: expr, slices: list0(slice))
    { "slice expression for the base expression {base} and slices {slices}" }
    | E_Cond(test: expr, true_branch: expr, false_branch: expr)
    { "condition expression for the test expression {test}
            true branch expression {true_branch} and
            false branch expression {false_branch}",
    }
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
    | E_Array[length: expr, value: expr]
    { "array construction for an array of length given by {length} with all cells initialized with {value}" }
    | E_EnumArray[labels: list1(Identifier), value: expr]
    { "array construction for an array associating each label in {labels} with the value given by {value}" }
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

render typed_expr = expr(E_GetItem, E_Array, E_EnumArray, E_GetEnumArray, E_GetCollectionFields);
render expr_array = expr(E_Array, E_EnumArray);

constant zero_bit
{ "\texttt{0}", math_macro = \zerobit };

constant one_bit
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
    | Pattern_Not(subexpression: expr)
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

render typed_slice = slice(typed_Slice_Length);

ast call { "call descriptor" } =
    [   name: Strings,
        params: list0(expr),
        args: list0(expr),
        call_type: subprogram_type,
    ]
    { "call of {call_type} subprogram {name}with parameters {params}, arguments {args}" }
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
    { "tuple type with components types {component_types}" }
    | T_Array(index: array_index, element_type: ty)
    { "integer type with {index} and element_type {element_type}" }
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
render typed_constraint_kind = constraint_kind(typed_WellConstrained), precision_loss_indicator(-);

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
    | LE_SetCollectionFields(collection_name: Identifier, field_names: list0(Identifier))
    { "assignable expression for the collection named {collection_name} and field names {field_names}" }
;

render untyped_lexpr = lexpr(
    LE_Discard,
    LE_Var,
    LE_Slice,
    LE_SetArray,
    LE_SetField,
    LE_SetFields,
    LE_Destructuring,
);
render typed_lexpr = lexpr(LE_SetEnumArray, LE_SetCollectionFields);

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
    index_name : Identifier,
    start_e    : expr,
    dir        : for_direction,
    end_e      : expr,
    body       : stmt,
    limit      : option(expr)
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
render typed_stmt = stmt(typed_S_Throw);

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

ast case_alt { "case alternative" } =
    [ pattern: pattern, where: option(expr), stmt: stmt ]
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

ast qualifier { "subprogram qualifier" } =
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
    name : Strings,
    parameters : list0((name: Identifier, type: option(ty))),
    args : list0(typed_identifier),
    body : stmt,
    return_type : option(ty),
    subprogram_type : subprogram_type,
    recurse_limit : option(expr),
    builtin : Bool,
    qualifier : option(qualifier),
    override : option(override_info),
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
        and override qualifier {override}
    " }
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
    keyword : global_decl_keyword,
    name : Identifier,
    ty : option(ty),
    initial_value : option(expr)
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

render decl_global_storage = decl(D_GlobalStorage), global_decl(-), global_decl_keyword(-);
render decl_func = decl(D_Func), func(-) ,subprogram_type(-), qualifier(-), override_info(-), typed_identifier(-);
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
 (G: global_static_envs, L: local_static_envs)
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
        global_storage_types: partial Identifier -> (element_type: ty, declared_keyword: global_decl_keyword),
        expr_equiv: partial Identifier -> (initializer: expr),
        subtypes: partial (sub_type: Identifier) -> (super_type: Identifier),
        subprogram: partial Identifier -> (func, side_effects: powerset(TSideEffect)),
        overloaded_subprogram: partial Identifier -> powerset(Strings)
    ]
    {  "global static environment with" }
;

typedef local_static_envs
    {
        "local static environment",
        math_macro = \localstaticenvs,
    } =
    [
        local_storage_types: partial Identifier -> (element_type: ty, declared_keyword: local_decl_keyword),
        expr_equiv: partial Identifier -> expr,
        return_type: option(ty)
    ]
    {  "local static environment" }
;

render static_envs_and_components = static_envs(-), global_static_envs(-), local_static_envs(-);

constant empty_tenv {
    "empty static environment",
    math_macro = \emptytenv,
};

typedef type_error
    {
        "\typingerrorterm{}",
    } =
    TypeError(error_code: Strings)
    {
        "\typingerrorterm{} with error code {error_code}",
    }
;

////////////////////////////////////////////////////////////////////////////////
// Side Effects Types

constant SE_Pure { "purity descriptor for the evaluation of a \pure{} construct" };
constant SE_Readonly { "purity descriptor for the evaluation of a \readonly{} construct" };
constant SE_Impure { "purity descriptor for the evaluation of a construct that is neither \pure{} nor \readonly{}" };

typedef TPurity { "purity descriptor" } =
    constants_set(SE_Pure, SE_Readonly, SE_Impure)
    { "\purity" }
;

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
    (G: global_dynamic_envs, L: local_dynamic_envs)
    {
        "dynamic environment with global dynamic environment {G} and local dynamic environment {L}",
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

constant empty_denv
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

constant Read
{ "read effect", math_macro = \Read };

constant Write
{ "write effect", math_macro = \Write };

constant asldata
{ "data dependency", math_macro = \asldata };

constant aslctrl
{ "control dependency", math_macro = \aslctrl };

constant aslpo
{ "program order dependency", math_macro = \aslpo };

typedef Labels
{
    "execution graph labels",
    math_macro = \Labels,
} =
    constants_set(asldata, aslctrl, aslpo)
    { "set of execution graph labels including data, control, and program order dependencies" }
;

typedef Nodes
{
    "execution graph nodes",
    math_macro = \Nodes,
} =
    (node_id: N, effect_type: constants_set(Read, Write), storage_element: Identifier)
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

render xgraphs_and_components = XGraphs(-), Nodes(-), Labels(-);

constant empty_graph { "empty execution graph", math_macro = \emptygraph };

ast symdom { "\symbolicdomain{}" } =
    | Finite(powerset_finite(Z))
    { "symbolic finite set integer domain" }
    | ConstrainedDom(int_constraint)
    { "symbolic constrained integer domain" }
;

ast symdom_or_top { "symbolic integer set" } =
    | Top
    { "symbolic unconstrained integer domain" }
    | Subdomains(list1(symdom))
    { "symbolic subdomains" }
;

render symbolic_domains = symdom(-), symdom_or_top(-);

constant Over { "overapproximation" };
constant Under { "underapproximation" };
typedef approximation_direction { "approximation direction" } =
    constants_set(Over, Under)
;

constant CannotOverapproximate {
    "cannot overapproximate",
    math_macro = \CannotOverapproximate
};

constant CannotUnderapproximate {
    "cannot underapproximate",
    math_macro = \CannotUnderapproximate
};

typedef ty_or_opt { "type or optional type" } =
    | (t:ty)
    { "the type {t}" }
    | option(ty)
    { "optional type" }
;

typedef expr_or_opt { "expression or optional expression" } =
    | (e:expr)
    { "the expression {e}" }
    | option(expr)
    { "optional expression" }
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
    | ResultCall(value_graph_pairs: list0((native_value, XGraphs)), environment: envs)
    { "call result with value-graph pairs {value_graph_pairs} and {environment}" }
;

typedef TThrowing
{
    "throwing execution result",
} =
    Throwing(exception_value: native_value, exception_type: ty, graph: XGraphs, environment: envs)
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
} =
    Returning(values_and_graph: (list0(native_value), XGraphs), environment: envs)
    { "returning result with values and graph {values_and_graph} and {environment}" }
;

typedef TDynError
{
    "dynamic error result",
} =
    DynamicError(error_code: Strings)
    { "dynamic error with error code {error_code}",
        math_macro = \DynamicError,
    }
;

constant Diverging
{ "diverging execution",  };

typedef TDiverging
{
    "diverging execution result",
} =
    constants_set(Diverging)
    { "diverging execution result" }
;

typedef value_read_from { "value-reading effect" } =
    (v: native_value, id: Identifier)
    { "{v} is read with {id}" }
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
// Literals Relations
////////////////////////////////////////////////////////////////////////////////

relation annotate_literal(tenv: static_envs, l: literal) -> (t: ty)
{
    "annotates a literal {l} in the \staticenvironmentterm{} {tenv}, resulting in a type {t}.",
    prose_application = "annotating {l} in {tenv} yields {t}",
};

////////////////////////////////////////////////////////////////////////////////
// Expression Relations
////////////////////////////////////////////////////////////////////////////////

relation annotate_expr(tenv: static_envs, e: expr) -> (t: ty, new_e: expr, ses: powerset(TSideEffect)) | type_error
{
    "annotates the expression {e} in the \staticenvironmentterm{} {tenv},
                        resulting in the following:
                        {t} is the type inferred for {e};
                        {new_e} is the \typedast{} for {e}, also known as the \emph{annotated expression};
                        and {ses} is the \sideeffectsetterm{} inferred for {e}. \ProseOtherwiseTypeError",
    prose_application = "annotating {e} in {tenv} yields
        {t}, the annotated expression {new_e} and {ses}\ProseOrTypeError",
};
