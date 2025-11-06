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

constant empty_set
{
    "the empty set",
    math_macro = \emptyset,
};

constant None { "the empty \optionalterm{}" };

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
    name : Strings,
    parameters : list0((name: Identifier, type: option(ty))),
    args : list0(typed_identifier),
    body : stmt,
    return_type : option(ty),
    subprogram_type : subprogram_type,
    recurse_limit : option(expr),
    builtin : Bool,
    qualifier : option(func_qualifier),
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
        subtypes: partial (sub_type: Identifier) ->
         (super_type: Identifier),
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
        short_circuit_macro = \TypeErrorConfig,
    } =
    TypeError(error_code: type_error_code)
    {
        "\typingerrorterm{} with error code {error_code}",
    }
;

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
    | option(t:ty)
    { "the optional type {t}" }
;

typedef expr_or_opt { "expression or optional expression" } =
    | (e:expr)
    { "the expression {e}" }
    | option(e:expr)
    { "the optional expression {e}" }
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
    | ResultCall((value_graph_pairs: (list0(value_read_from), XGraphs)), environment: envs)
    { "call result with value-graph pairs {value_graph_pairs} and {environment}" }
;

typedef TThrowing
{
    "throwing execution result",
    short_circuit_macro = \ThrowingConfig,
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

constant Diverging
{ "diverging execution",  };

typedef TDiverging
{
    "diverging execution result",
    short_circuit_macro = \DivergingConfig,
} =
    constants_set(Diverging)
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
    { "{v} is read with {id}" }
;

////////////////////////////////////////////////////////////////////////////////
// Generic Functions and Relations
////////////////////////////////////////////////////////////////////////////////

typing function te_check(cond: Bool, code: type_error_code) -> constants_set(True) | type_error
  {
    "returns $\True$ if {cond} holds and a type error with {code} otherwise.",
    prose_application = "checking whether {cond} holds returns $\True\terminateas\TypeError({code})$",
  }
;

semantics function de_check(cond: Bool, code: dynamic_error_code) -> constants_set(True) | TDynError
  {
    "returns $\True$ if {cond} holds and a dynamic error with {code} otherwise.",
    prose_application = "checking whether {cond} holds returns $\True\terminateas\DynamicError({code})$",
  }
;

function bool_transition(cond: Bool) -> (result: Bool)
{
    math_macro = \booltrans,
    "returns $\True$ if {cond} holds and $\False$ otherwise",
    prose_application = "testing whether {cond} holds returns {result}",
};

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
};

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
};

typing function find_bitfields_slices(name: Identifier, bitfields: list0(bitfield)) -> (slices: list0(slice)) | type_error
{
  "returns the slices associated with the bitfield named {name} among the list of bitfields {bitfields}
  in {slices}. \ProseOtherwiseTypeError",
  prose_application = "finding the slices associated with the bitfield named {name} among the list of bitfields {bitfields}
  yields {slices}\ProseOtherwiseTypeError",
};

typing relation annotate_field_init(tenv: static_envs, (name: Identifier, e': expr), field_types: list0(field)) ->
        (name: Identifier, e'': expr, ses: powerset(TSideEffect)) | type_error
{
  "annotates a field initializer $({name}, {e'})$ in a record expression
  with list of fields \\ {field_types} and returns the annotated initializing expression {e''}
  and its \sideeffectdescriptorterm\ {ses}. \ProseOtherwiseTypeError",
  prose_application = "annotating the field initializer $({name}, {e'})$ with respect to
  the list of fields {field_types}, yields {e''} and {ses}\ProseOrTypeError",
  math_layout = [_,_],
};

typing relation annotate_get_array(
        tenv: static_envs,
        (size: expr, t_elem: ty),
        (e_base: expr, ses_base: powerset(TSideEffect),
        e_index: expr)) ->
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
};

typing function get_bitfield_width(tenv: static_envs, name: Identifier, tfields: list0(field)) ->
         (e_width: expr) | type_error
{
  "returns the expression {e_width} that describes the
  width of the bitfield named {name} in the list of
  fields {tfields}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-getbitfieldwidth}{computing} the width of bitfield {name} in fields {tfields} yields expression {e_width}"
};

typing function width_plus(tenv: static_envs, exprs: list0(expr)) -> (e_width: expr) | type_error
{
  "generates the expression {e_width}, which represents the summation of all expressions in the list {exprs},
  normalized in the \staticenvironmentterm{} {tenv}. \ProseOtherwiseTypeError",
  prose_application = "generating the expression representing the summation of {exprs} in {tenv}, yields {e_width}",
};

typing function check_atc(tenv: static_envs, t1: ty, t2: ty) ->
         (constants_set(True)) | type_error
{
  "checks whether the types {t1} and {t2}, which are
  assumed to not be named types, are compatible for a
  type assertion in the \staticenvironmentterm{} {tenv},
  yielding $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkatc}{checking} type compatibility between {t1} and {t2} in {tenv} yields True"
};

semantics relation eval_expr(env: envs, e: expr) ->
         ResultExpr((v: native_value, g: XGraphs), new_env: envs)
                                        | TThrowing | TDynError | TDiverging
{
    "evaluates the expression {e} in an environment {env} and terminates normally with
    a \nativevalueterm{} {v}, an \executiongraphterm{} {g}, and a modified environment
    {new_env}. \ProseOtherwiseAbnormal",
    prose_application = "\hyperlink{relation-eval_expr}{evaluating} {e} in {env} yields
    {v}, {g}, and {new_env}",
    math_layout = (_, [_,_,_,_]),
};

semantics relation eval_expr_sef(env: envs, e: expr) -> ResultExprSEF(v: native_value, g: XGraphs) | TDynError | TDiverging
{
   prose_description = "specializes the expression evaluation relation for
                        side-effect-free expressions by omitting throwing
                        configurations as possible output configurations.",
 prose_application = "",
};

semantics relation is_val_of_type(env: envs, v: native_value, t: ty) ->
         (b: Bool, g: XGraphs) | TDynError | TDiverging
{
  "tests whether the value {v} can be stored in a
  variable of type {t} in the environment {env},
  resulting in a Boolean value {b} and execution graph
  {g}. \ProseOtherwiseDynamicErrorOrDiverging",
  prose_application = "\hyperlink{relation-isvaloftype}{testing} if value {v} matches type {t} in {env} yields result {b} and graph {g}"
};

semantics relation is_constraint_sat(env: envs, c: int_constraint, n: Z) ->
         (b: Bool, g: XGraphs)
{
  "tests whether the integer value $n$ \emph{satisfies
  the constraint} {c} (that is, whether $n$ is within
  the range of values defined by {c}) in the environment
  {env} and returns a Boolean answer {b} and the
  execution graph {g} resulting from evaluating the
  expressions appearing in {c}.",
  prose_application = "\hyperlink{relation-isconstraintsat}{verifying} integer {n} satisfies constraint {c} in {env} yields {b} and graph {g}"
};

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
};

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
};

semantics relation eval_lexpr(env: envs, le: lexpr, m: (v: native_value, g: XGraphs)) ->
        | ResultLexpr(new_g: XGraphs, new_env: envs)
        | TThrowing
        | TDynError
        | TDiverging
{
    "evaluates the assignment of the value {v}
        with \executiongraphterm{} {g}
        to the \assignableexpression{} {le} in the environment {env},
        resulting in the configuration $\ResultLexpr({new_g}, {new_env})$. \ProseOtherwiseAbnormal",
    prose_application = "evaluating the assignment of {v} with {g} to {le} in {env}
        yields $\ResultLexpr({new_g}, {new_env})$\ProseOrAbnormal",
    math_layout = (_, [_,_,_,_]),
};

semantics relation eval_multi_assignment(env: envs, lelist: list0(expr), vmlist: list0((native_value, XGraphs))) ->
        | ResultLexpr(new_g: XGraphs, new_env: envs)
        | TThrowing
        | TDynError
{
    "evaluates multi-assignments. That is, the simultaneous assignment of the list of value-\executiongraphterm{} pairs {vmlist} to the corresponding list of \assignableexpressions{} {lelist}, in the environment {env}. The result is either the \executiongraphterm{} {new_g} and new environment {new_env} or an abnormal configuration",
    prose_application = "evaluating multi-assignment of {vmlist} to {lelist} in {env} yields $\ResultLexpr({new_g}, {new_env})$ or abnormal configuration",
    math_macro = \evalmultiassignment,
    math_layout = (_, [_,_,_]),
};

typing relation annotate_set_array(tenv: static_envs, size_elem: (array_index, ty), rhs_ty: ty, base_ses_index: (e_base: expr, ses_base: powerset(TSideEffect), e_index: expr)) ->
    (new_le: lexpr, ses: powerset(TSideEffect)) | type_error
{
    "annotates an array update in the \staticenvironmentterm{} {tenv}
    where {size_elem} contains the array index and type of array elements,
    {rhs_ty} is the type of the \rhsexpression{},
    and {base_ses_index} contains the annotated expression {e_base} for the array base,
    the \sideeffectsetterm{} {ses_base} inferred for the base,
    and the index expression {e_index}.
    The result is the annotated \assignableexpression{} {new_le} and \sideeffectsetterm{} for the annotated expression {ses}. \ProseOtherwiseTypeError",
    prose_application = "annotating array update in {tenv} with {size_elem}, {rhs_ty}, and {base_ses_index} yields {new_le} and {ses}\ProseOrTypeError",
    math_layout = [_,_],
};

typing function check_disjoint_slices(tenv: static_envs, slices: list0(slice)) ->
         constants_set(True) | type_error
{
    "checks whether the list of slices {slices} do not overlap in {tenv}, yielding $\True$. \ProseOtherwiseTypeError",
    prose_application = "checking whether {slices} are disjoint in {tenv} yields $\True$\ProseOrTypeError",
};

semantics function check_non_overlapping_slices(value_ranges: list0((tint, tint))) ->
         constants_set(True) | TDynError
{
    "checks whether the sets of integers represented by the list of ranges {value_ranges} overlap, yielding $\True$. \ProseOtherwiseDynamicErrorOrDiverging",
    prose_application = "checking whether {value_ranges} are non-overlapping yields $\True$\ProseOrDynamicErrorOrDiverging",
};

semantics function check_two_ranges_non_overlapping(range1: (s1: tint, l1: tint), range2: (s2: tint, l2: tint)) ->
         constants_set(True) | TDynError
{
    "checks whether two sets of integers represented by the ranges $({s1},{l1})$ and $({s2},{l2})$ do not intersect, yielding $\True$. \ProseOtherwiseDynamicError",
    prose_application = "checking whether $({s1},{l1})$ and $({s2},{l2})$ do not intersect yields $\True$\ProseOrError",
};

typing function fold_bitvector_fields(tenv: static_envs, base_fields: list0(field), le_fields: list0(bitfield)) ->
         (length: N, slices: list0((N, N)))
{
    "accepts a \staticenvironmentterm{} {tenv}, the list of all fields {base_fields} for a record type, and a list of fields {le_fields} that are the subset of {base_fields} about to be assigned to, and yields the total width across {le_fields} and the ranges corresponding to {le_fields} in terms of pairs where the first component is the start position and the second component is the width of the field.",
    prose_application = "folding bitvector fields {le_fields} from {base_fields} in {tenv} yields length {length} and slices {slices}",
};

semantics function assign_bitvector_fields(bitvector: tbitvector, record: trecord, fields: list0(Identifier), slices: list0((N, N))) ->
         (result: trecord)
{
    "updates the list of fields {fields} of {record} with the slices given by {slices} from \\
    {bitvector}, yielding the \nativevalueterm{} {result}",
    prose_application = "assigning {bitvector} slices {slices} to fields {fields} of {record} yields {result}",
};

//////////////////////////////////////////////////
// Relations for Base Values

typing function base_value(tenv: static_envs, t: ty) ->
         (e_init: expr) | type_error
{
  "returns the expression {e_init} which can be used to
  initialize a storage element of type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-basevalue}{computing} initial value for type {t} in {tenv} yields expression {e_init}"
};

typing function constraint_abs_min(tenv: static_envs, c: int_constraint) ->
         (zs: list0(Z)) | type_error
{
  "returns a single element list containing the integer
  closest to $0$ that satisfies the constraint {c} in
  {tenv}, if one exists, and an empty list if the
  constraint represents an empty set. Otherwise, the
  result is $\TypeErrorVal{\NoBaseValue}$.",
  prose_application = "\hyperlink{relation-constraintabsmin}{finding} minimal absolute value satisfying constraint {c} in {tenv} yields {zs}"
};

typing function list_min_abs(l: list0(Z)) ->
         (z: Z)
{
  "returns {z} --- the integer closest to $0$ among the
  list of integers in the list {l}. The result is biased
  towards positive integers. That is, if two integers
  $x$ and $y$ have the same absolute value and $x$ is
  positive and $y$ is negative then $x$ is considered
  closer to $0$.",
  prose_application = "\hyperlink{relation-listminabs}{finding} integer closest to zero in list {l} yields {z}"
};

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
  prose_application = "\hyperlink{relation-annotatebitfields}{annotating} bitfields {fields} with width {e_width} in {tenv} yields {new_fields} and side effects {ses}"
};

typing function bitfield_get_name(bf: bitfield) ->
         (name: Identifier)
{
  "given a bitfield {bf}, returns {name}, the name of the bitfield {bf}.",
  prose_application = "\hyperlink{relation-bitfieldgetname}{extracting} name from bitfield {bf} yields {name}"
};

typing function bitfield_get_slices(bf: bitfield) ->
         (slices: list0(slice))
{
  "returns the list of slices {slices} associated with
  the bitfield {bf}.",
  prose_application = "\hyperlink{relation-bitfieldgetslices}{extracting} slices from bitfield {bf} yields {slices}"
};

typing function bitfield_get_nested(bf: bitfield) ->
         (nested: list0(bitfield))
{
  "returns the list of bitfields {nested} nested within
  the bitfield {bf}, if there are any, and an empty list
  if there are none.",
  prose_application = "\hyperlink{relation-bitfieldgetnested}{extracting} nested bitfields from {bf} yields {nested}"
};

typing relation annotate_bitfield(tenv: static_envs, width: Z, field: bitfield) ->
         (new_field: bitfield, ses: powerset(TSideEffect)) | type_error
{
  "annotates a bitfield {field} with an integer
  {width} indicating the number of bits in the
  bitvector type that contains {field}, in an
  environment {tenv}, resulting in an annotated bitfield
  {new_field} or a \typingerrorterm{}, if one is
  detected.",
  prose_application = "\hyperlink{relation-annotatebitfield}{annotating} bitfield {field} with width {width} in {tenv} yields {new_field} and {ses}"
};

typing function check_slices_in_width(tenv: static_envs, width: Z, slices: list0(slice)) ->
         (constants_set(True)) | type_error
{
  "checks whether the slices in {slices} fit within the
  bitvector width given by {width} in {tenv}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkslicesinwidth}{verifying} slices {slices} fit within width {width} in {tenv} yields True"
};

typing function check_positions_in_width(width: Z, positions: powerset(Z)) ->
         (constants_set(True)) | type_error
{
  "checks whether the set of positions in {positions} fit
  within the bitvector width given by {width}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkpositionsinwidth}{verifying} positions {positions} fit within width {width} yields True"
};

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
  prose_application = "\hyperlink{relation-disjointslicestopositions}{converting} disjoint slices {slices} in {tenv} with static flag {is_static} yields positions {positions}"
};

typing function bitfield_slice_to_positions(tenv: static_envs, is_static: Bool, slice: slice) ->
         (positions: option(powerset_finite(Z))) | type_error
{
  "returns the set of integers defined by the bitfield
  slice {slice} in {positions}, if it can be determined
  via static evaluation or normalization, depending on
  {is_static}, and $\None$ if it cannot be determined.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-bitfieldslicetopositions}{converting} slice {slice} in {tenv} with static flag {is_static} yields optional positions {positions}"
};

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
  prose_application = "\hyperlink{relation-evalsliceexpr}{evaluating} slice expression {e} in {tenv} with static flag {is_static} yields optional integer {z_opt}"
};

typing function check_common_bitfields_align(tenv: static_envs, bitfields: list0(bitfield), width: N) ->
         (constants_set(True)) | type_error
{
  "checks \RequirementRef{BitfieldAlignment} for every
  pair of bitfields in {bitfields}, contained in a
  bitvector type of width {width} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkcommonbitfieldsalign}{checking} alignment of bitfields {bitfields} of width {width} in {tenv} yields True"
};

typing function bitfields_to_absolute(tenv: static_envs, bitfields: list1(bitfield), absolute_parent: TAbsField) ->
         (abs_bitfields: powerset(TAbsField))
{
  "returns the set of \absolutebitfields{} {abs_bitfields}
  that correspond to the list of bitfields {bitfields},
  whose \bitfieldscope{} and \absoluteslice{} is given by
  {absolute_parent}, in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "\hyperlink{relation-bitfieldstoabsolute}{converting} bitfields {bitfields} with parent {absolute_parent} in {tenv} yields absolute bitfields {abs_bitfields}"
};

typing function bitfield_to_absolute(tenv: static_envs, bf: bitfield, absolute_parent: TAbsField) ->
         (abs_bitfields: powerset(TAbsField))
{
  "returns the set of \absolutebitfields{} {abs_bitfields}
  that correspond to the bitfields nested in {bf},
  including itself, where the \bitfieldscope{} and
  \absoluteslice{} of the bitfield containing {bf} are
  {absolute_parent}, in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "\hyperlink{relation-bitfieldtoabsolute}{converting} bitfield {bf} with parent {absolute_parent} in {tenv} yields absolute bitfields {abs_bitfields}"
};

typing function select_indices_by_slices(indices: list1(N), slice_indices: list1(N)) ->
         (absolute_slice: list0(N))
{
  "considers the list {indices} as a list of indices into
  a bitvector type (essentially, a slice of it), and the
  list {slice_indices} as a list of indices into
  {indices} (a slice of a slice), and returns the
  sub-list of {indices} indicated by the indices in
  {slice_indices}.",
  prose_application = "\hyperlink{relation-selectindicesbyslices}{selecting} indices from {indices} using slice indices {slice_indices} yields absolute slice {absolute_slice}"
};

typing function absolute_bitfields_align(f: TAbsField, g: TAbsField) ->
         (b: Bool)
{
  "tests whether the \absolutebitfields{} {f} and {g}
  share the same name and exist in the same scope. If
  they do, {b} indicates whether their \absoluteslices\
  are equal. Otherwise, the result is $\True$.",
  prose_application = "\hyperlink{relation-absolutebitfieldsalign}{checking} alignment between absolute bitfields {f} and {g} yields {b}"
};

typing function slice_to_indices(tenv: static_envs, s: slice) ->
         (indices: list0(N))
{
  "returns the list of indices {indices} represented by
  the bitvector slice {s} in the
  \staticenvironmentterm{} {tenv}.",
  prose_application = "\hyperlink{relation-slicetoindices}{converting} slice {s} in {tenv} yields indices {indices}"
};

//////////////////////////////////////////////////
// Relations for Block Statements

typing relation annotate_block(tenv: static_envs, s: stmt) ->
         (new_stmt: stmt, ses: powerset(TSideEffect)) | type_error
{
  "annotates a block statement {s} in
  \staticenvironmentterm{} {tenv} and returns the
  annotated statement {new_stmt} and inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-annotateblock}{annotating} block statement {s} in {tenv} yields statement {new_stmt} and side effects {ses}"
};

semantics relation eval_block(env: envs, stm: stmt) -> Continuing(new_g: XGraphs, new_env: envs) | TReturning | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a statement {stm} as a \emph{block}. That
                        is, {stm} is evaluated in a fresh local environment,
                        which drops back to the original local environment of
                        {env} when the evaluation terminates.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
 };

semantics function pop_local_scope(outer_denv: dynamic_envs, inner_denv: dynamic_envs) -> (new_denv: dynamic_envs)
{
  "discards from {inner_denv} the bindings to local storage elements that are not in\\ {outer_denv}, yielding {new_denv}.",
  prose_application = "dropping from {inner_denv} the bindings to local storage elements that are not in {outer_denv}
  yields {new_denv}",
};

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
};

semantics relation eval_catchers(env: envs, catchers: list0(catcher), otherwise_opt: option(stmt), s_m: TOutConfig) ->
  TReturning | TContinuing | TThrowing | TDynError
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
 };

semantics function find_catcher(tenv: static_envs, v_ty: ty, catchers: list0(catcher)) ->
  (catcher_opt: option(catcher))
{
  "returns the first catcher clause in {catchers} that matches the type {v_ty} in {catcher_opt}, if one exists.
  Otherwise, it returns $\None$",
  prose_application = "finding the first catcher clause in {catchers} that matches the type {v_ty} in the context of {tenv} yields {catcher_opt}",
};

//////////////////////////////////////////////////
// Relations for Global Pragmas

typing function check_global_pragma(genv: global_static_envs, d: decl) ->
         (constants_set(True)) | type_error
{
  "typechecks a global pragma declaration {d} in the
  \globalstaticenvironmentterm{} {genv}, yielding
  $\True$. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-checkglobalpragma}{checking} global pragma declaration {d} in {genv} yields True"
};

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
  prose_application = "\hyperlink{relation-declareglobalstorage}{declaring} global storage {gsd} in {genv} yields environment {new_genv} and declaration {new_gsd}"
};

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
};

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
};

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
};

semantics relation eval_globals(decls: list0(decl), envm: (env: envs, g1: XGraphs)) -> (C: (envs, XGraphs)) | TThrowing | TDynError | TDiverging
{
   prose_description = "updates the input environment and execution graph by
                        initializing the global storage declarations.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
};

semantics function declare_global(name: Identifier, v: native_value, env: envs) -> (new_env: envs)
{
   prose_description = "updates the environment {env} by mapping {name} to
                        {v} in the $\storage$ map of the global dynamic
                        environment $G^\denv$.",
 prose_application = "",
};

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
  {ty} and optional initializing expression and \sideeffectsetterm{} {e_opt}, yields {new_tenv}\OrTypeError",
  math_layout = [[_,_,_,_,_], _],
};

semantics relation eval_local_decl(env: envs, ldi: local_decl_item, m: (v: native_value, g1: XGraphs)) ->
  ResultLDI(new_g: XGraphs, new_env: envs)
{
  "evaluates a \localdeclarationitem{} {ldi} in an environment {env} with an initialization value {m},
  yielding the \executiongraphterm{} {new_g} and new environment {new_env}.",
  prose_application = "evaluating the \localdeclarationitem{} {ldi} in {env} with the initializing
  value {m} yields {new_g} and {new_env}.",
};

semantics relation declare_ldi_tuple(env: envs, ids: list0(Identifier), liv: list0((native_value, XGraphs))) ->
  ResultLDI(g: XGraphs, new_env: envs)
{
  "declares in {env} the local storage elements whose names are given by {ids} and initialization values and \executiongraphterm{} by {liv}.
  The lists {ids} and {liv} are assumed to have equal lengths.
  The result is the updated environment {new_env} and resulting \executiongraphterm{} {g}.",
  prose_application = "declaring the local storage elements whose identifiers are given by {ids} and initializers are
  given by {liv} yields the updated environment {new_env} and \executiongraphterm{} {g}.",
};

typing function check_is_not_collection(tenv: static_envs, t: ty) ->
         (constants_set(True)) | type_error
{
  "checks whether the type {t} has the structure of a
  \collectiontypeterm{}, and if so, raises a
  \typingerrorterm{}. Otherwise, the result is $\True$.",
  prose_application = "\hyperlink{relation-checkisnotcollection}{verifying} type {t} in {tenv} is not a collection type yields True"
};

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
};

semantics relation eval_pattern(env: envs, v: native_value, p: pattern) -> ResultPattern(b: tbool, new_g: XGraphs) | TDynError | TDiverging
{
   prose_description = "determines whether a value {v} matches the pattern
                        {p} in an environment {env} resulting in either
                        $\ResultPattern(\vb, \newg)$ or an abnormal
                        configuration.",
 prose_application = "",
};

semantics function mask_match(mv: constants_set(zero_bit, one_bit, x_bit), b: Bit) -> (res: Bool)
{
  "tests whether the bit {b} matches the mask value {mv}, yielding the result in {res}",
  prose_application = "testing whether the bit {b} matches the mask value {mv}, yields {res}",
};

//////////////////////////////////////////////////
// Relations for Primitive Operations

typing function unop_literals(op: unop, l: literal) ->
         (r: literal) | type_error
{
  "statically evaluates a unary operator {op} (a terminal
  derived from the AST non-terminal for unary operators)
  over a literal {l} and returns the resulting literal
  {r}. \ProseOtherwiseTypeError",
  prose_application = "",
};

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
};

typing function binary_to_unsigned(bits: list0(Bit)) -> (num: N)
{
  "converts the bit sequence {bits} into the natural number {num} or $0$ if {bits} is empty.",
  prose_application = "converting the bit sequence {bits} into a natural number yields {num}",
};

typing function int_to_bits(val: Z, width: Z) -> (bits: list0(Bit))
{
  "converts the integer {val} to its two's complement little endian representation
  of {width} bits, yielding the result in {bits}.",
  prose_application = "converting the integer {val} to its two's complement little endian representation
  of {width} bits, yields {bits}",
};

semantics function eval_unop(op: unop, v: native_value) ->
         (w: native_value) | TDynError
{
  "evaluates a unary operator {op} over a
  \nativevalueterm{} {v} and returns the
  \nativevalueterm{} {w} or an error.",
  prose_application = "",
};

semantics function eval_binop(op: binop, v1: native_value, v2: native_value) ->
         (w: native_value) | TDynError
{
  "evaluates a binary operator {op} over a pair of
  \nativevaluesterm{}  {v1} and {v2} and returns
  the \nativevalueterm{}  {w} or an error.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Relations On Types

typing function is_subtype(tenv: static_envs, t1: ty, t2: ty) -> (b: Bool)
{
    "defines whether the type {t1} \subtypesterm{} the type {t2} in the \staticenvironmentterm{} {tenv},
    yielding the result in {b}.",
    prose_application = "testing whether {t1} \subtypesterm{} {t2} in {tenv} yields {b}",
};

typing function subtype_satisfies(tenv: static_envs, t: ty, s: ty) -> (b: Bool) | type_error
{
    "determines whether a type {t} \emph{\subtypesatisfiesterm} a type {s} in the static environment {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {t} \subtypesatisfiesterm{} {s} in {tenv} yields {b}\ProseOrTypeError",
};

typing function field_names(fields: list0((name: Identifier, type: ty))) -> (names: powerset(Identifier))
{
  "returns the set of field names appearing in {fields} in {names}",
  prose_application = "the set of field names appearing in {fields} is {names}",
};

typing function field_type(fields: list0((name: Identifier, type: ty)), id: Identifier) -> (ty_opt: option(ty))
{
  "returns the type associated with {id} in {fields}, if there exists a unique one, and $\None$, otherwise.",
  prose_application = "finding the unique type associated with {id} in {fields} yields {ty_opt}",
};

typing function type_satisfies(tenv: static_envs, t: ty, s: ty) -> (b: Bool) | type_error
{
    "determines whether a type {t} \emph{\typesatisfiesterm} a type {s} in the static environment {tenv},
    yielding the result {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {t} \typesatisfiesterm{} {s} in {tenv} yields {b}\ProseOrTypeError",
};

typing function checked_typesat(tenv: static_envs, t: ty, s: ty) -> constants_set(True) | type_error
{
  "returns $\True$ if {t} \typesatisfiesterm{} a type {s} in the static environment {tenv}. \ProseOtherwiseTypeError",
  prose_application = "checking whether {t} \typesatisfiesterm{} {s} in {tenv} yields $\True$\ProseOrTypeError",
  math_macro = \checktypesat,
};

typing relation lowest_common_ancestor(tenv: static_envs, t: ty, s: ty) -> (ty: ty) | type_error
{
  "returns the \Proselca{} of types {t} and {s} in the \staticenvironmentterm{} {tenv}, yielding {ty}.
  If a \Proselca{} does not exist or a \typingerrorterm{} is detected, the result is a \typingerrorterm{}.",
  prose_application = "the \Proselca{} of {t} and {s} in {tenv} is {ty}\ProseOrTypeError",
  math_macro = \lca,
};

typing relation apply_unop_type(tenv: static_envs, op: unop, t: ty) ->
         (s: ty) | type_error
{
  "determines the result type of applying a unary
  operator when the type of its operand is known.
  Similarly, we determine the negation of integer
  constraints. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function negate_constraint(c: int_constraint) ->
         (new_c: int_constraint)
{
  "takes an integer constraint {c} and returns the
  constraint {new_c}, which corresponds to the negation
  of all the values that {c} represents.",
  prose_application = "",
};

typing relation apply_binop_types(tenv: static_envs, op: binop, t1: ty, t2: ty) ->
         (t: ty) | type_error
{
  "determines the result type {t} of applying the binary
  operator {op} to operands of type {t1} and {t2} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function named_lowest_common_ancestor(tenv: static_envs, t: ty, s: ty) ->
         (ty: ty) | type_error
{
  "returns the lowest common named super type
   of types {t} and {s} in {tenv} in {ty}.",
  prose_application = "",
};

typing function supers(tenv: static_envs, t: ty) ->
         (powerset(ty))
{
  "returns the set of \emph{named supertypes} of a type
  {t} in the $\subtypes$ function of a
  \globalstaticenvironmentterm{} {tenv}.",
  prose_application = "",
};

typing relation annotate_constraint_binop(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    op: binop,
    cs1: list0(int_constraint),
    cs2: list0(int_constraint)) ->
         (annotated_cs: list0(int_constraint), p: precision_loss_indicator) | type_error
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
  math_layout = [_,_],
};

typing relation binop_filter_rhs(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    op: binop,
    cs: list0(int_constraint)) ->
         (new_cs: list1(int_constraint)) | constants_set(CannotUnderapproximate,CannotOverapproximate)
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
};

typing relation refine_constraints(
    approx: constants_set(Over,Under),
    tenv: static_envs,
    f: fun int_constraint -> option(int_constraint),
    cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint)) | constants_set(CannotUnderapproximate,CannotOverapproximate)
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
         (res: list0(int_constraint)) | constants_set(CannotUnderapproximate,CannotOverapproximate)
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
};

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
};

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
};

typing function explode_intervals(tenv: static_envs, cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint), p: precision_loss_indicator)
{
  "applies $\explodeintervals$ to each constraint of {cs}
    in {tenv}, and returns a pair consisting of the list
    of exploded constraints in {new_cs} and a
    \precisionlossindicatorterm{} {p}.",
  prose_application = "",
};

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
};

typing function interval_too_large(z1: Z, z2: Z) ->
         (b: Bool)
{
  "determines whether the set of numbers between {z1} and
  {z2}, inclusive, contains more than
  $\maxexplodedintervalsize$ integers, yielding the
  result in {b}.",
  prose_application = "",
};

typing function binop_is_exploding(op: binop) ->
         (b: Bool)
{
  "determines whether the binary operation {op} should
  lead to applying $\explodeintervals$ when the {op} is
  applied to a pair of constraint lists. It is assumed
  that {op} is one of $\MUL$, $\SHL$, $\POW$, $\ADD$,
  $\DIV$, $\SUB$, $\MOD$, $\SHR$, and $\DIVRM$.",
  prose_application = "",
};

typing function bitfields_included(tenv: static_envs, bfs1: list0(bitfield), bfs2: list0(bitfield)) -> (b: Bool) | type_error
{
    "tests whether the set of bit fields in {bfs1} is included in the set of bit fields in {bfs2}
    in the static environment {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "testing whether {bfs1} is included in {bfs2} in {tenv} yields {b}\ProseOrTypeError",
};

typing function mem_bfs(tenv: static_envs, bfs2: list1(bitfield), bf1: bitfield) ->
         (b: Bool)
{
  "checks whether the bitfield {bf1} exists in {bfs2} in
  the context of {tenv}, returning the result in {b}.",
  prose_application = "",
};

typing function check_structure_label(tenv: static_envs, t: ty, l: ASTLabels) -> constants_set(True) | type_error
{
  "returns $\True$ if {t} has the \structureterm{} of a type corresponding to the AST label {l}. \ProseOtherwiseTypeError",
  prose_application = "checking whether the \structureterm{} of {t} has the AST label {l} yields $\True$\ProseOrTypeError",
  math_macro = \checkstructurelabel,
};

typing function to_well_constrained(t: ty) ->
         (t': ty)
{
  "returns {t'}, the \wellconstrainedversionterm{} of a type {t}, which converts
  \parameterizedintegertypesterm{} to
  \wellconstrainedintegertypesterm{}, and leaves all
  other types as are.",
  prose_application = "",
};

typing function get_well_constrained_structure(tenv: static_envs, t: ty) ->
         (t': ty) | type_error
{
  "returns the \wellconstrainedstructureterm{} of a type
  {t} in the \staticenvironmentterm{} {tenv} --- {t'},
  which is defined as follows. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function get_bitvector_width(tenv: static_envs, t: ty) ->
         (e: expr) | type_error
{
  "returns the expression {e}, which represents the width
  of the bitvector type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function get_bitvector_const_width(tenv: static_envs, t: ty) ->
         (w: N) | type_error
{
  "returns the natural number {w}, which represents the
  width of the bitvector type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function check_bits_equal_width(tenv: static_envs, t1: ty, t2: ty) ->
         (constants_set(True)) | type_error
{
  "tests whether the types {t1} and {t2} are bitvector
  types of the same width. If the answer is positive,
  the result is $\True$. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function precision_join(p1: precision_loss_indicator, p2: precision_loss_indicator) ->
         (p: precision_loss_indicator)
{
  "returns the \precisionlossindicatorterm{} {p},
  denoting whether {p1} or {p2} denote a precision loss.",
  prose_application = "",
  math_layout = [_,_],
};

//////////////////////////////////////////////////
// Relations for Semantics Utilities

semantics function get_pending_calls(denv: dynamic_envs, name: Identifier) ->
         (s: N)
{
  "retrieves the value associated with {name} in
  $\denv.\pendingcalls$ or $0$ if no value is associated
  with it.",
  prose_application = "\hyperlink{relation-getpendingcalls}{retrieving} pending calls count for {name} in {denv} yields {s}"
};

semantics function set_pending_calls(genv: global_dynamic_envs, name: Identifier, v: N) ->
         (new_genv: dynamic_envs)
{
  "updates the value bound to {name} in $\genv.\storage$
  to {v}, yielding the new global dynamic environment
  {new_genv}.",
  prose_application = "",
};

semantics function incr_pending_calls(genv: global_dynamic_envs, name: Identifier) ->
         (new_genv: global_dynamic_envs)
{
  "increments the value associated with {name} in
  $ {genv}.\pendingcalls $, yielding the updated global
  dynamic environment {new_genv}.",
  prose_application = "incrementing the number of pending calls for {name} in {genv} yields {new_genv}",
};

semantics function decr_pending_calls(genv: global_dynamic_envs, name: Identifier) ->
         (new_genv: global_dynamic_envs)
{
  "decrements the value associated with {name} in
  $\genv.\pendingcalls$, yielding the updated global
  dynamic environment {new_genv}. It is assumed that\\
  $\getpendingcalls((\genv, \emptyfunc), \name)$ yields
  a positive value.",
  prose_application = "",
};

semantics function remove_local(env: envs, name: Identifier) -> (new_env: envs)
{
   prose_description = "removes the binding of the identifier {name} from the
                        local storage of the environment {env}, yielding the
                        environment {new_env}.",
 prose_application = ""
 };

semantics relation read_identifier(name: Identifier, v: native_value) -> (XGraphs)
{
   prose_description = "creates an \executiongraphterm{} that represents the
                        reading of the value {v} into a storage element given
                        by the identifier {name}. The result is an execution
                        graph containing a single Read Effect, which denotes
                        reading from {name}. % The value {v} is ignored, as
                        execution graphs do not contain values.",
 prose_application = ""
 };

semantics relation write_identifier(name: Identifier, v: native_value) -> (XGraphs)
{
   prose_description = "creates an \executiongraphterm{} that represents the
                        writing of the value {v} into the storage element
                        given by an identifier {name}. The result is an
                        execution graph containing a single Write Effect,
                        which denotes writing into {name}. % The value {v} is
                        ignored, as execution graphs do not contain values.",
 prose_application = ""
 };

semantics function concat_bitvectors(vs: list0(tbitvector)) ->
         (new_vs: tbitvector)
{
  "transforms a (possibly empty) list of bitvector
  \nativevaluesterm{} {vs} into a single bitvector
  {new_vs}.",
  prose_application = "",
};

semantics function slices_to_positions(slices: list1((s: tint, l: tint))) ->
         (positions: list0(N)) | TDynError
{
  "returns the list of positions (indices) specified by
  a list of slices expressed as pairs. Each pair specifies the start
  and length of a slice.
  If all slices consist of only
  non-negative integers. \ProseOtherwiseDynamicError",
  prose_application = "",
};

semantics function max_pos_of_slice(s: tint, l: tint) ->
         (max_pos: tint)
{
  "returns the maximum position specified by the slice
  starting at {s} of length {l}, assuming that all slices consist only of
  non-negative integers.",
  prose_application = "",
};

semantics function read_from_bitvector(v: native_value, slices: list0((tint, tint))) ->
         (new_v: tbitvector) | TDynError
{
  "reads from a bitvector {v}, or an integer seen as a
  bitvector, the indices specified by the list of slices
  {slices}, thereby concatenating their values, yielding the new bitvector {new_v}.",
  prose_application = "",
};

semantics function write_to_bitvector(slices: list0((tint, tint)), src: tbitvector, dst: tbitvector) ->
         (v: tbitvector) | TDynError
{
  "overwrites the bits of {dst} at the positions given by
  {slices} with the bits of {src}.",
  prose_application = "\hyperlink{relation-writetobitvector}{writing} bits from {src} to {dst} at positions {slices} yields bitvector {v}"
};

semantics function get_index(i: N, vec: tvector) -> (r: tvector) | TDynError
{
   prose_description = "reads the value {r} from the vector of values {vec}
                        at the index {i}. \ProseOtherwiseDynamicError",
 prose_application = ""
 };

semantics function set_index(i: N, v: native_value, vec: tvector) -> (res: tvector) | TDynError
{
   prose_description = "overwrites the value at the given index {i} in a
                        vector of values {vec} with the new value {v}.
                        \ProseOtherwiseDynamicError",
 prose_application = "",
};

semantics function get_field(name: Identifier, record: trecord) -> (native_value)
{
   prose_description = "retrieves the value corresponding to the field name
                        {name} from the record value {record}.",
 prose_application = ""
 };

semantics function set_field(name: Identifier, v: native_value, record: trecord) -> (trecord)
{
   prose_description = "overwrites the value corresponding to the field name
                        {name} in the record value {record} with the value
                        {v}.",
 prose_application = "",
};

semantics relation declare_local_identifier(env: envs, name: Identifier, v: native_value) -> (new_env: envs, g: XGraphs)
{
   prose_description = "associates {v} to {name} as a local storage element
                        in the environment {env} and returns the updated
                        environment {new_env} with the execution graph
                        consisting of a Write Effect to {name}.",
 prose_application = "",
};

semantics relation declare_local_identifier_m(env: envs, x: Identifier, m: (v: native_value, g: XGraphs)) ->
         (new_env: envs, new_g: XGraphs)
{
  "declares the local identifier {x} in the environment
  {env}, in the context of the value-graph pair $(\vv,
  \vg)$, yielding a pair consisting of the environment
  {new_env} and \executiongraphterm{} {new_g}.",
  prose_application = "\hyperlink{relation-declarelocalidentifierm}{declaring} local identifier {x} in {env} with value-graph pair {m} yields environment {new_env} and graph {new_g}"
};

semantics relation declare_local_identifier_mm(env: envs, x: Identifier, m: (v: native_value, g: XGraphs)) ->
         (new_env: envs, new_g: XGraphs)
{
  "declares the local identifier {x} in the environment
  {env}, in the context of the value-graph pair $(\vv,
  \vg)$, yielding a pair consisting of an environment
  {new_env} and an \executiongraphterm{} {new_g}.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Side Effects

typing function side_effect_is_pure(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\pureterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \pureterm{} yields {b}",
};

typing function side_effect_is_readonly(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\readonlyterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \readonlyterm{} yields {b}",
};

typing function side_effect_is_symbolically_evaluable(s: TSideEffect) -> (b: Bool)
{
  "returns $\True$ if the \sideeffectdescriptorterm{} {s} is \emph{\symbolicallyevaluableterm},
    yielding the result in {b}.",
  prose_application = "testing whether {s} is \symbolicallyevaluableterm{} yields {b}",
};

typing function ses_ldk(ldk: local_decl_keyword) ->
         (s: powerset(TSideEffect))
{
  "constructs a \sideeffectsetterm{} {s} corresponding to
  a read of a storage element declared with a local
  declaration keyword {ldk}.",
  prose_application = "",
};

typing function ses_gdk(gdk: global_decl_keyword) ->
         (s: powerset(TSideEffect))
{
  "constructs a \sideeffectsetterm{} {s} corresponding to
  a read of a storage element declared with a global
  declaration keyword {gdk}.",
  prose_application = "",
};

typing function is_symbolically_evaluable(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether a set of \sideeffectdescriptorsterm\
  {ses} are all \symbolicallyevaluableterm, yielding the
  result in {b}.",
  prose_application = "",
};

typing function check_symbolically_evaluable(ses: powerset(TSideEffect)) -> constants_set(True) | type_error
{
  "returns $\True$ if the set of \sideeffectdescriptorsterm{} {ses} is \symbolicallyevaluableterm.
  \ProseOtherwiseTypeError",
  prose_application = "checking whether {ses} is \symbolicallyevaluableterm{} yields $\True$\OrTypeError",
};

typing function ses_is_readonly(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether all side effects in the set {ses} are
  \readonlyterm{}, yielding the result in {b}.",
  prose_application = "",
};

typing function ses_is_pure(ses: powerset(TSideEffect)) ->
         (b: Bool)
{
  "tests whether all side effects in the set {ses} are
  \pureterm{}, yielding the result in {b}.",
  prose_application = "",
};

typing function ses_for_subprogram(qualifier: option(func_qualifier)) ->
         (s: powerset(TSideEffect))
{
  "produces a \sideeffectsetterm{} given a subprogram
  qualifier {qualifier}.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Slicing

typing relation annotate_slice(tenv: static_envs, s: slice) -> (s': slice) | type_error
{
  "annotates a single slice {s} in the \staticenvironmentterm{} {tenv},
  resulting in an annotated slice {s'}.\ProseOtherwiseTypeError",
  prose_application = "annotating the slice {s} in {tenv} yields {s'}\OrTypeError"
};

typing relation slices_width(tenv: static_envs, slices: list0(slice)) ->
         (width: expr) | type_error
{
  "returns an expression {slices} that represents the
  width of all slices given by {slices} in the
  \staticenvironmentterm{} {tenv}.",
  prose_application = "",
};

typing function slice_width(slice: slice) ->
         (width: expr)
{
  "returns an expression {width} that represents the
  width of the slices given by {slice}.",
  prose_application = "",
};

typing relation annotate_symbolic_constrained_integer(tenv: static_envs, e: expr) ->
         (e'': expr, ses: powerset(TSideEffect)) | type_error
{
  "annotates a \symbolicallyevaluableterm{} integer
  expression {e} of a constrained integer type in the
  \staticenvironmentterm{} {tenv} and returns the
  annotated expression {e''} and \sideeffectsetterm\
  {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing relation annotate_slices(tenv: static_envs, slices: list0(slice)) ->
         (slices': list0(slice), ses: powerset(TSideEffect))
{
  "annotates a list of slices {slices} in the
  \staticenvironmentterm{} {tenv}, yielding a list of
  annotated slices (that is, slices in the \typedast)
  and \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

semantics relation eval_slice(env: envs, s: slice) ->
  | (((v_start: tint, v_length: tint), new_g: XGraphs), new_env: envs)
  | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates an individual slice {s} in an environment
                        {env} is, resulting either in \\
                        $((({v_start}, {v_length}), {new_g}), {new_env})$.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
};

relation eval_slices(env: envs, slices: list0(slice)) ->
  | ResultSlices((ranges: list0((native_value, native_value)), new_g: XGraphs), new_env: envs)
  | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a list of slices {slices} in an environment
                        {env}, resulting in either \\
                        $\ResultSlices((\ranges, \newg), \newenv)$.
                        \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
};

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
};

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
};

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
    env_and_fs: list0((local_static_envs, func))) ->
         (new_genv: global_static_envs,
         new_env_and_fs: list0((local_static_envs,
         func,
         powerset(TSideEffect)))) |
         type_error
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
};

typing function add_subprogram_decls(tenv: static_envs, funcs: list0((func, powerset(TSideEffect)))) ->
         (new_tenv: static_envs)
{
  "adds each subprogram definition given by a $\func$ AST
  node in {funcs} to the $\subprograms$ map of
  $G^\tenv$, yielding {new_tenv}.",
  prose_application = "",
};

typing relation override_subprograms(decls: list0(decl)) ->
         (decls': list0(decl)) | type_error
{
  "overrides subprograms in a list of declarations
  {decls}, yielding the new list of declarations
  {decls'}. \ProseOtherwiseTypeError{} See
  \ExampleRef{Overriding Subprograms} for an example of
  valid overriding.",
  prose_application = "",
};

typing function check_implementations_unique(impls: list0(func)) ->
         (constants_set(True)) | type_error
{
  "checks that the \Proseimplementationsubprograms{}
  {impls} have unique signatures.
  \ProseOtherwiseTypeError{} See \ExampleRef{Invalid
  Overriding} for an example of non-unique
  \Proseimplementationsubprograms{}.",
  prose_application = "",
};

typing function signatures_match(func1: func, func2: func) ->
         (Bool)
{
  "checks whether the signatures of subprograms {func1}
  and {func2} match for overriding purposes.
  \ProseOtherwiseTypeError{} See \ExampleRef{Overriding
  Subprograms} for an example of matching signatures.",
  prose_application = "",
};

typing function process_overrides(impdefs: list0(func), impls: list0(func)) ->
         (impdefs': list0(func), discarded: list0(func)) | type_error
{
  "overrides the \Proseimpdefsubprograms{} {impdefs} with
  the \Proseimplementationsubprograms{} {impls},
  yielding the new \Proseimpdefsubprograms{} {impdefs}
  and the discarded subprograms {discarded}.
  \ProseOtherwiseTypeError{} See \ExampleRef{Overriding
  Subprograms} for an example of valid replacement of an
  \Proseimplementationsubprogram{}.",
  prose_application = "",
};

typing relation rename_subprograms(discarded: list0(func)) ->
         (renamed_discarded: list0(func))
{
  "renames the subprograms {discarded} to give them fresh
  names.",
  prose_application = "",
};

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
};

typing function decl_dependencies(d: decl) ->
         (depends: list0((def_use_name, def_use_name)))
{
  "returns the set of dependent pairs of identifiers
  {depends} induced by the declaration {d}.",
  prose_application = "",
};

typing function def_decl(d: decl) ->
         (name: def_use_name)
{
  "returns the identifier {name} being defined by the
  declaration {d}.",
  prose_application = "",
};

typing function def_enum_labels(d: decl) ->
         (labels: powerset(def_use_name))
{
  "takes a declaration {d} and returns the set of
  enumeration labels it defines in {labels}, if it
  defines any.",
  prose_application = "",
};

typing function use_decl(d: decl) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  declaration {d} depends on.",
  prose_application = "",
};

typing function use_ty(t: ty_or_opt) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the type or
  \optionalterm{} type {t} depends on.",
  prose_application = "",
};

typing function use_subtypes(fields: option((x: Identifier, subfields: list0(field)))) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  \optionalterm{} pair consisting of identifier {x} (the type
  being subtyped) and fields {subfields} depends on.",
  prose_application = "",
};

typing function use_expr(e: expr_or_opt) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  expression or \optionalterm{} expression {e} depends on.",
  prose_application = "",
  math_macro = \useexpr
};

typing function use_lexpr(le: lexpr) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  left-hand-side expression {le} depends on.",
  prose_application = "",
  math_macro = \uselexpr
};

typing function use_pattern(p: pattern) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  pattern {p} depends on.",
  prose_application = "",
};

typing function use_slice(s: slice) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the slice
  {s} depends on.",
  prose_application = "",
};

typing function use_bitfield(bf: decl) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  bitfield {bf} depends on.",
  prose_application = "",
};

typing function use_constraint(c: int_constraint) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the integer
  constraint {c} depends on.",
  prose_application = "",
};

typing function use_ldi(l: local_decl_item) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  \localdeclarationitem{} {l} depends on.",
  prose_application = "",
};

typing function use_stmt(s: stmt) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the
  statement {s} depends on.",
  prose_application = "",
  math_macro = \usestmt
};

typing function use_catcher(c: catcher) ->
         (ids: powerset(def_use_name))
{
  "returns the set of identifiers {ids} which the try
  statement catcher {c} depends on.",
  prose_application = "",
};

semantics relation eval_spec(tenv: static_envs, spec: spec) -> (v: tint, g: XGraphs) | TDynError
{
   prose_description = "evaluates the specification {spec} with the
                        \staticenvironmentterm{} {tenv}, yielding the native
                        integer value {v} and execution graph {g}. Otherwise,
                        the result is a \dynamicerrorterm{}.",
 prose_application = "",
};

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
};

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
};

typing relation annotate_local_decl_type_annot(
    tenv: static_envs,
    ty_opt: option(ty),
    t_e: ty,
    ldk: local_decl_keyword,
    e': expr,
    ldi: local_decl_item) ->
         (new_tenv: static_envs, ty_opt': option(ty), ses: powerset(TSideEffect)) | type_error
{
  "annotates the type annotation {ty_opt} in the
  \staticenvironmentterm{} {tenv} within the context of
  a local declaration with keyword {ldk}, item {ldi},
  and initializing expression {e'} with type {t_e}. It
  yields the modified \staticenvironmentterm{}
  {new_tenv}, the annotated type annotation {ty_opt'},
  and the inferred \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [input[_,_,_,_,_,_], _],
};

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
};

typing function check_no_precision_loss(t: ty) ->
         (constants_set(True)) | type_error
{
  "checks whether the type {t} is the result of a
  precision loss in its constraint computation (see for
  example \TypingRuleRef{ApplyBinopTypes}).",
  prose_application = "",
};

typing function check_can_be_initialized_with(tenv: static_envs, s: ty, t: ty) ->
         (constants_set(True)) | type_error
{
  "checks whether an expression of type {s} can be used
  to initialize a storage element of type {t} in the
  \staticenvironmentterm{} {tenv}. If the answer if
  $\False$, the result is a \typingerrorterm.",
  prose_application = "",
};

typing relation annotate_limit_expr(tenv: static_envs, e: option(expr)) ->
         (option(e': expr), ses: powerset(TSideEffect)) | type_error
{
  "annotates an optional expression {e} serving as the
  limit of a loop or a recursive subprogram in {tenv},
  yielding a pair consisting of an expression {e'} and a
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing relation get_for_constraints(
    tenv: static_envs,
    struct1: ty,
    struct2: ty,
    e1': expr,
    e2': expr,
    dir: for_direction) ->
         (vis: constraint_kind) | type_error
{
  "infers the integer constraints for a \texttt{for} loop
  index variable from the following:
  \begin{itemize}
  \item the \wellconstrainedversionterm{} of the type of
  the start expression {struct1}
  \item the \wellconstrainedversionterm{} of the type of the end
  expression {struct2}
  \item the annotated start expression {e1'}
  \item the annotated end expression {e2'}
  \item the loop direction {dir}
  \end{itemize} The result is {vis}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \getforconstraints,
  math_layout = [_,_],
};

semantics relation eval_stmt(env: envs, s: stmt) -> Returning((vs: list0(native_value), new_g: XGraphs), new_env: envs)
    | Continuing(new_g: XGraphs, new_env: envs) | TThrowing | TDynError | TDiverging
{
   prose_description = "evaluates a statement {s} in an environment {env},
                        resulting in one of four types of configurations (see
                        more details in
                        \secref{KindsOfSemanticConfigurations}):
                        \begin{itemize}
                        \item returning configurations with values {vs}, execution graph {new_g}, and a modified environment {new_env};
                        \item continuing configurations with an execution graph {new_g} and modified environment {new_env};
                        \item throwing configurations;
                        \item error configurations;
                        \item diverging configurations.
                        \end{itemize}",
 prose_application = "",
 math_layout = (_, [_,_,_,_,_]),
};

semantics function output_to_console(env: envs, v: native_value) -> (new_env: envs)
{
  "communicates {v} to a console, where one exists, possibly updating the environment {env}, yielding {new_env}.",
  prose_application = "communicating {v} to the console in the context of {env} yields {new_env}",
};

semantics function literal_to_string(l: literal) -> (s: Strings)
{
  "converts a literal {l} to a printable string {s}",
  prose_application = "converting {l} to a printable string yields {s}",
};

semantics function lexpr_is_var(le: lexpr) -> (res: Bool)
 {
    "tests whether {le} is an assignable variable expression or a discarded \assignableexpression{}, yielding the result in {res}",
    prose_application = "testsing whether {le} is an assignable variable expression or a discarded \assignableexpression{} yields {res}",
 };

semantics relation eval_for(
  env: envs,
  index_name: Identifier,
  limit_opt: option(tint),
  v_start: tint,
  dir: constants_set(UP,DOWN),
  v_end: tint,
  body: stmt) -> TReturning | TContinuing | TThrowing | TDynError | TDiverging
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
 };

semantics relation eval_for_step(
    env: envs,
    index_name: Identifier,
    limit_opt: option(tint),
    v_start: tint,
    dir: constants_set(UP,DOWN)) ->
         ((v_step: tint, new_env: envs), new_g: XGraphs) |
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
};

semantics relation eval_for_loop(
    env: envs,
    index_name: Identifier,
    limit_opt: option(tint),
    v_start: tint,
    dir: constants_set(UP,DOWN),
    v_end: tint,
    body: stmt) ->
         Continuing(new_g: XGraphs, new_env: envs) | TReturning | TThrowing | TDynError | TDiverging
{
  "executes one iteration of the loop body and then uses
  $\texttt{eval\_for}$ to execute the remaining
  iterations.",
  prose_application = "",
  math_layout = [_,_],
};

semantics relation eval_loop(env: envs, is_while: Bool, limit_opt: option(N), e_cond: expr, body: stmt) ->
  | Continuing(new_g: XGraphs, new_env: envs)
  | TReturning
  | TThrowing
  | TDynError
{
   prose_description = "to evaluate both \texttt{while} statements and
                        \texttt{repeat} statements.",
 prose_application = "",
  math_layout = [_,_],
 };

semantics relation eval_limit(env: envs, e_limit_opt: option(expr)) -> (v_opt: option(N), g: XGraphs) | TDynError | TDiverging
{
   prose_description = "evaluates the optional expression {e_limit_opt} in
                        the environment {env}, yielding the optional integer
                        value {v_opt} and execution graph {g}.
                        \ProseOtherwiseDynamicErrorOrDiverging",
 prose_application = ""
 };

semantics relation tick_loop_limit(v_opt: option(N)) -> (v_opt': option(N)) | TDynError
{
   prose_description = "decrements the optional integer {v_opt}, yielding the
                        optional integer value {v_opt}. If the value is $0$,
                        the result is a \DynamicErrorConfigurationTerm{}.",
 prose_application = ""
 };

semantics relation eval_expr_list_m(env: envs, es: list0(expr)) ->
         ResultExprListM(vms: list0((native_value, XGraphs)), new_env: envs) | TThrowing | TDynError | TDiverging
{
  "evaluates a list of expressions {es} in left-to-right
  in the initial environment {env} and returns the list
  of values associated with graphs {vms} and the new
  environment {new_env}. If the evaluation of any
  expression terminates abnormally then the abnormal
  configuration is returned.",
  prose_application = "",
  math_layout = [_,_],
};

semantics relation write_folder(vms: list0((native_value, XGraphs))) ->
         (vs: list0(native_value), new_g: XGraphs)
{
  "concatenates the input values in {vms} and generates
  an execution graph by composing the graphs in {vms}
  with Write Effects for the respective values.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Static Evaluation

typing function static_eval(tenv: static_envs, e: expr) -> (v: literal) | type_error
{
  "evaluates an expression {e}
  in the \staticenvironmentterm{} {tenv}, returning a literal {v}.
  If the evaluation terminates by a thrown exception of a value that is not a literal
  (for example, a record value), the result is a \typingerrorterm{}.",
  prose_application = "statically evaluating {e} in {tenv} yields {v}\ProseOrTypeError",
};

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
};

typing relation annotate_call_actuals_typed(
  tenv: static_envs,
  name: Identifier,
  params: list0((ty, expr, powerset(TSideEffect))),
  typed_args: list0((ty, expr, powerset(TSideEffect))),
  call_type: subprogram_type) ->
  (call: call, ret_ty_opt: option(ty)) | type_error
{
  "is similar to $\annotatecall$, except that it accepts
  the annotated versions of the parameter and argument
  expressions as inputs, that is, tuples consisting of
  types, annotated expressions, and
  \sideeffectdescriptorsetsterm.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [input[_,_,_,_,_], _],
};

typing function insert_stdlib_param(func_sig: func, params: list0((ty, expr)), arg_types: list0(ty)) ->
         (params1: list0((ty, expr, powerset(TSideEffect))))
{
  "inserts the (optionally) omitted input parameter of a
  standard library function call.",
  prose_application = "",
};

typing function can_omit_stdlib_param(func_sig: func) ->
         (b: Bool)
{
  "tests whether the first parameter of the subprogram
  defined by {func_sig} can be omitted (and thus
  automatically inserted), yielding the result in {b}.",
  prose_application = "",
};

typing function check_params_typesat(tenv: static_envs, func_sig_params: list0((Identifier, option(ty))), params: list0((ty, expr, powerset(TSideEffect)))) ->
         (constants_set(True)) | type_error
{
  "checks that annotated parameters {params} are correct
  with respect to the declared parameters
  {func_sig_params}. \ProseOtherwiseTypeError{} It
  assumes that {func_sig_params} and {params} have the
  same length.",
  prose_application = "",
  math_layout = [_,_],
};

typing relation rename_ty_eqs(tenv: static_envs, eqs: list0((Identifier, expr)), ty: ty) ->
         (new_ty: ty) | type_error
{
  "transforms the type {ty} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {eqs}, yielding the type {new_ty}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

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
};

typing function subst_expr(tenv: static_envs, substs: list0((Identifier, expr)), e: expr) ->
         (new_e: expr)
{
  "transforms the expression {e} in the
  \staticenvironmentterm{} {tenv}, by substituting
  parameter names with their corresponding expressions
  in {substs}, yielding the expression {new_e}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

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
};

typing function check_args_typesat(tenv: static_envs, func_sig_args: list0((Identifier, ty)), arg_types: list0(ty), eqs: list0((Identifier, expr))) ->
         (constants_set(True)) | type_error
{
  "checks that the types {arg_types} \typesatisfyterm\
  the types of the corresponding formal arguments
  {func_sig_args} with the parameters substituted with
  their corresponding arguments as per {eqs} and results
  in a \typingerrorterm{} otherwise.",
  prose_application = "",
};

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
};

typing function subprogram_for_signature(tenv: static_envs, name: Strings, caller_arg_types: list0(ty), call_type: subprogram_type) ->
         (name': Strings, callee: func, ses: powerset(TSideEffect)) | type_error
{
  "looks up the \staticenvironmentterm{} {tenv} for a
  subprogram associated with {name}, the list of
  argument types {caller_arg_types}, and a subprogram
  type matching {call_type}, and determines which one of
  the following cases holds:
  \begin{enumerate}
  \item there is no declared subprogram that matches {name}, {caller_arg_types}, \\ and {call_type};
  \item there is exactly one subprogram that matches {name}, {caller_arg_types}, \\ and {call_type};
  \end{enumerate}
  If more than one subprogram that matches {name} and
  {caller_arg_types}, this is detected by the rule
  \TypingRuleRef{DeclareSubprograms}, which invokes the
  rule \\ \TypingRuleRef{DeclareOneFunc}, which invokes
  the rule \TypingRuleRef{AddNewFunc}, which results in
  a \typingerrorterm{}.",
  prose_application = "",
  math_layout = [_,_],
};

typing function filter_call_candidates(tenv: static_envs, formal_types: list0(ty), call_type: subprogram_type, candidates: powerset(Strings)) ->
         (matches: list0((Strings, func)))
{
  "iterates over the list of unique subprogram names in
  {candidates} and checks whether their lists of
  arguments clash with the types in {formal_types} and
  their subprogram types match {call_type} in {tenv}.
  The result is the set of pairs consisting of the names
  and function definitions of the subprograms whose
  arguments clash and subprogram types match in
  {candidates}. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function call_type_matches(func_def: func, call_type: subprogram_type) ->
         (b: Bool)
{
  "checks whether a function definition {func_def} is
  compatible with the subprogram type expected by a
  function call, {call_type}, yielding the result in
  {b}.",
  prose_application = "",
};

typing function has_arg_clash(tenv: static_envs, f_tys: list0(ty), args: list0((Identifier, ty))) ->
         (b: Bool) | type_error
{
  "checks whether a list of types {f_tys} clashes with
  the list of types appearing in the list of arguments
  {args} in {tenv}, yielding the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function type_clashes(tenv: static_envs, t: ty, s: ty) ->
         (b: Bool) | type_error
{
  "determines whether a type {t} \emph{\Prosetypeclashes}
  with a type {s} in environment {tenv}, returning the
  result {b}. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing relation annotate_exprs(tenv: static_envs, exprs: list0(expr)) ->
         (typed_exprs: list0((ty, expr, powerset(TSideEffect)))) | type_error
{
  "annotates a list of expressions {exprs} from left to
  right, yielding a list of tuples \\ {typed_exprs},
  each consisting of a type, an annotated expression,
  and a \sideeffectsetterm. \ProseOtherwiseTypeError",
  prose_application = "",
};

semantics relation eval_call(env: envs, name: Identifier, params: list0(expr), args: list0(expr)) ->
    ResultCall(vms2: (list0(value_read_from), XGraphs), new_env: envs) | TDynError | TDiverging
{
   prose_description = "evaluates a call to the subprogram named {name} in
                        the environment {env}, with the parameter expressions
                        {params} and the argument expressions {args}. The
                        evaluation results in either a list of returned
                        values, each one associated with an execution graph,
                        and a new environment. \ProseOtherwiseAbnormal",
 prose_application = "",
  math_layout = [_,_],
};

semantics relation eval_subprogram(
  env: envs,
  name: Identifier,
  params: list0((native_value, XGraphs)),
  actual_args: list0((native_value, XGraphs))) ->
  | ResultCall((list0(value_read_from), XGraphs), new_env: envs)
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
                        environment {new_env}. The values represent values
                        returned by the subprogram call and the identifiers
                        are used in generating execution graph constraints
                        for the returned values.",
 prose_application = "",
  math_layout = [_,_],
};

semantics relation assign_args((env: envs, g1: XGraphs), ids: list0(Identifier), actuals: list0((native_value, XGraphs))) -> (new_env: envs, new_g: XGraphs)
{
   prose_description = "updates the pair consisting of the environments {env}
                        and \executiongraphterm\ {g1} by assigning the values
                        given by {actuals} to the identifiers given by {ids},
                        yielding the updated pair ({new_env}, {new_g}).",
 prose_application = "",
};

semantics relation match_func_res(TContinuingOrReturning) ->
    (ResultCall(vms2: (list0(value_read_from), XGraphs), new_env: envs), envs)
{
   prose_description = "converts continuing configurations and returning
                        configurations into corresponding normal
                        configurations that can be returned by a subprogram
                        evaluation.",
 prose_application = "",
};

semantics relation check_recurse_limit(env: envs, name: Identifier, e_limit_opt: option(expr)) ->
         (g: XGraphs) | TDynError
{
  "checks whether the value in the optional expression
  {e_limit_opt} has reached the limit associated with
  {name} in {env}, yielding the execution graph
  resulting from evaluating the optional expression in
  {g}. Otherwise, the result is a
  \DynamicErrorConfigurationTerm{} indicating that the
  recursion limit has been reached.",
  prose_application = "",
};

semantics relation read_value_from(value_read_from) -> (native_value, XGraphs)
{
  "generates an execution graph for reading the given
  value to a variable given by the identifier, and pairs
  it with the given value.",
  prose_application = "",
};

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
};

typing relation annotate_func_sig(genv: global_static_envs, func_sig: func) ->
         (new_tenv: static_envs, new_func_sig: func, ses: TSideEffect) | type_error
{
  "annotates the signature of a function definition
  {func_sig} in the \globalstaticenvironmentterm{}
  {genv}, yielding a new function definition
  {new_func_sig}, a modified \staticenvironmentterm{}
  {new_tenv}, and an inferred \sideeffectsetterm{} {ses}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing relation annotate_params(tenv: static_envs, params: list0((Identifier, option(ty))), (new_tenv: static_envs, acc: list0((Identifier, ty)))) ->
         (tenv_with_params: static_envs, params1: Identifier, ty) | type_error
{
  "annotates each parameter in {params} with respect to
  {tenv}, and declares it in environment {new_tenv}. It
  returns the updated environment {tenv_with_params} and
  the annotated parameters {params1}, together with any
  annotated parameters already in the accumulator {acc}.
  \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing relation annotate_one_param(
    tenv: static_envs,
    new_tenv: static_envs,
    (x: Identifier, ty_opt: option(ty))) ->
    (new_tenv': static_envs, ty: ty) | type_error
{
   prose_description = "annotates the parameter given by {x} and the
                        \optionalterm{} type {ty_opt} with respect to {tenv} and
                        then declares the parameter {x} in environment
                        {new_tenv}. The updated environment {new_tenv'} and
                        annotated parameter type {ty} are returned.
                        \ProseOtherwiseTypeError",
 prose_application = "",
};

typing relation check_param_decls(tenv: static_envs, func_sig: func) ->
         (b: Bool) | type_error
{
  "checks the validity of the parameters declared in
  {func_sig}.",
  prose_application = "",
};

typing function extract_parameters(tenv: static_envs, func_sig: func) ->
         (unique_parameters: list0(Identifier)) | type_error
{
  "returns the parameter names declared in {func_sig}
    into {unique_parameters}, while checking their validity
    in the \staticenvironmentterm{} {tenv}.
    \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function func_sig_types(func_sig: func) ->
         (tys: list0(ty))
{
  "returns the list of types {tys} in the subprogram
  signature {func_sig}. Their ordering is return type
  first (if any), followed by argument types
  left-to-right.",
  prose_application = "",
};

typing function paramsofty(tenv: static_envs, ty: ty) ->
         (ids: list0(Identifier)) | type_error
{
  "extracts the list of parameters appearing in the type
  {ty}, assuming that {ty} appears in a function
  signature. \ProseOtherwiseTypeError",
  prose_application = "",
  math_macro = \paramsofty
};

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
};

typing function params_of_constraint(tenv: static_envs, c: int_constraint) ->
         (ids: list0(Identifier))
{
  "finds the list of parameters in the constraint {c}. It
  assumes that {c} appears within a
  \wellconstrainedintegertypeterm{} in a function
  signature.",
  prose_application = "",
  math_macro = \paramsofconstraint
};

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
};

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
};

typing relation annotate_return_type(tenv_with_params: static_envs, tenv_with_args: static_envs, return_type: option(ty), ses_in: powerset(TSideEffect)) ->
         (new_tenv: static_envs, new_return_type: ty, ses: powerset(TSideEffect)) | type_error
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
};

typing function check_subprogram_purity(qualifier: option(func_qualifier), ses: powerset(TSideEffect)) ->
         (b: Bool) | type_error
{
  "checks that the \sideeffectsetterm{} {ses} is
  consistent with the subprogram qualifier {qualifier}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

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
};

typing function subprogram_clash(tenv: static_envs, name: Strings, subpgm_type: subprogram_type, qualifier: func_qualifier, formal_types: list0(ty)) ->
         (b: Bool) | type_error
{
  "checks whether the unique subprogram associated with
  {name} clashes with another subprogram that has
  subprogram type {subpgm_type}, qualifier {qualifier},
  and list of formal types {formal_types}, yielding a
  Boolean value in {b}. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function subprogram_types_clash(s1: subprogram_type, s2: subprogram_type) ->
         (b: Bool)
{
  "defines whether the subprogram types {s1} and {s2}
  clash, yielding the result in {b}.",
  prose_application = "",
};

typing relation add_new_func(tenv: static_envs, name: Identifier, qualifier: option(func_qualifier), formals: list0(typed_identifier), subpgm_type: subprogram_type) ->
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
  math_layout = [_,_],
};

typing relation annotate_subprogram(tenv: static_envs, f: func, ses_func_sig: powerset(TSideEffect)) ->
         (f': func, ses: powerset(TSideEffect)) | type_error
{
  "annotates a subprogram {f} in an environment {tenv}
  and \sideeffectsetterm{} {ses_func_sig}, resulting in
  an annotated subprogram {f'} and inferred
  \sideeffectsetterm{} {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing function check_control_flow(tenv: static_envs, f: func, body: stmt) ->
         (constants_set(True)) | type_error
{
  "checks whether the annotated body statement {body} of the
  function definition {f} obeys the control-flow
  requirements in the \staticenvironmentterm{} {tenv}.",
  prose_application = "",
};

typing function allowed_abs_configs(f: func) ->
         (abs_configs: powerset(constants_set(Abs_Continuing,Abs_Returning,Abs_Abnormal)))
{
  "determines the set of \Proseabstractconfigurations{}
  allowed for the function definition {f}, yielding the
  result in {abs_configs}.",
  prose_application = "",
};

typing function approx_stmt(tenv: static_envs, s: stmt) ->
         (abs_configs: constants_set(Abs_Continuing,Abs_Returning,Abs_Abnormal))
{
  "returns in {abs_configs} a superset of the set of
  \Proseabstractconfigurations{} (defined next), that an
  evaluation of {s} in any environment consisting of the
  \staticenvironmentterm{} {tenv} yields.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Symbolic Equivalence Testing

typing function normalize(tenv: static_envs, e: expr) ->
         (new_e: expr) | type_error
{
  "\hypertarget{def-symbolicallysimplifies}{symbolically
  simplifies} an expression {e} in the
  \staticenvironmentterm{} {tenv}, yielding an
  expression {new_e}. \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-normalize}{simplifying} expression {e} in {tenv} yields {new_e}"
};

typing function reduce_constraint(tenv: static_envs, c: int_constraint) ->
         (new_c: int_constraint)
{
  "\symbolicallysimplifiesterm{} an integer constraint
  {c}, yielding the integer constraint {new_c}",
  prose_application = "",
};

typing function reduce_constraints(tenv: static_envs, cs: list0(int_constraint)) ->
         (new_cs: list0(int_constraint))
{
  "\symbolicallysimplifiesterm{} a list of integer
  constraints {cs}, yielding a list of integer
  constraints {new_cs}",
  prose_application = "",
};

typing function to_ir(tenv: static_envs, e: expr) ->
         (p: polynomial) | constants_set(CannotBeTransformed) | type_error
{
  "transforms a subset of ASL expressions into
  \symbolicexpressionsterm{}. If an ASL expression
  cannot be represented by a symbolic expression
  (because, for example, it contains operations that are
  not available in \symbolicexpressionsterm{}), the
  special value $\CannotBeTransformed$ is returned.",
  prose_application = "\hyperlink{relation-toir}{converting} expression {e} in {tenv} to symbolic form yields polynomial {p}"
};

typing function expr_equal(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "conservatively checks whether {e1} and {e2} are
  \equivalentexprsterm{} in the \staticenvironmentterm{}
  {tenv}. The result is given in {b} or a
  \typingerrorterm{}, if one is detected.",
  prose_application = "",
};

typing function expr_equal_norm(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the {e1} and {e2} are
  \equivalentexprsterm{} in the \staticenvironmentterm{}
  {tenv} by attempting to transform both expressions to
  their \symbolicexpressionterm{} form and, if
  successful, comparing the resulting
  \symbolicexpressionsterm{} for equality. The result is
  given in {b} or a \typingerrorterm{}, if one is
  detected.",
  prose_application = "",
};

typing function expr_equal_case(tenv: static_envs, e1: expr, e2: expr) ->
         (b: Bool) | type_error
{
  "specializes the equivalence test for expressions {e1}
  and {e2} in {tenv} for the different types of
  expressions. The result is given in {b} or a
  \typingerrorterm{}, if one is detected.",
  prose_application = "",
};

typing function type_equal(tenv: static_envs, t1: ty, t2: ty) ->
         (b: Bool) | type_error
{
  "conservatively tests whether {t1} and {t2} are
  \equivalenttypesterm{} in the \staticenvironmentterm{}
  {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function bitwidth_equal(tenv: static_envs, w1: expr, w2: expr) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the bitwidth expression
  {w1} is equivalent to the bitwidth expression {w2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function bitfields_equal(tenv: static_envs, bf1: list0(bitfield), bf2: list0(bitfield)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the list of bitfields
  {bf1} is equivalent to the list of bitfields {bf2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function bitfield_equal(tenv: static_envs, bf1: bitfield, bf2: bitfield) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the bitfield {bf1} is
  equivalent to the bitfield {bf2} in environment {tenv}
  and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function constraints_equal(tenv: static_envs, cs1: list0(int_constraint), cs2: list0(int_constraint)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the constraint list {cs1}
  is equivalent to the constraint list {cs2} in
  environment {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function constraint_equal(tenv: static_envs, c1: int_constraint, c2: int_constraint) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the constraint {c1} is
  equivalent to the constraint {c2} in environment
  {tenv} and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function slices_equal(tenv: static_envs, slices1: list0(slice), slices2: list0(slice)) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the list of slices
  {slices1} is equivalent to the list of slices
  {slices2} in environment {tenv} and yields the result
  in {b}.  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function slice_equal(tenv: static_envs, slice1: slice, slice2: slice) ->
         (b: Bool) | type_error
{
  "conservatively tests whether the slice {slice1} is
  equivalent to the slice {slice2} in environment {tenv}
  and yields the result in {b}. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function array_length_equal(l1: array_index, l2: array_index) ->
         (b: Bool) | type_error
{
  "tests whether the array lengths {l1} and {l2} are
  equivalent and yields the result in {b}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function mul_monomials(m1: unitary_monomial, m2: unitary_monomial) -> (m: unitary_monomial)
{
  "multiplies the unitary monomial {m1} with the unitary monomial {m2},
  yielding the unitary monomial {m}",
  prose_application = "multiplying the unitary monomial {m1} with unitary monomial {m2}
                      yields the unitary monomial {m}"
};

typing function add_polynomials(p1: polynomial, p2: polynomial) -> (p: polynomial)
{
  "adds the polynomial {p1} with the polynomial {p2},
  yielding the polynomial {p}",
  prose_application = "adding the polynomial {p1} with polynomial {p2}
                      yields the polynomial {p}"
};

typing function mul_polynomials(p1: polynomial, p2: polynomial) -> (p: polynomial)
{
  "multiplies the polynomial {p1} with the polynomial {p2}, yielding the polynomial {p}.",
  prose_application = "multiplying the polynomial {p1} with polynomial {p2}
                      yields the polynomial {p}"
};

typing function polynomial_divide_by_term(p1: polynomial, m: unitary_monomial, f: Q) -> (p: polynomial) | constants_set(CannotBeTransformed)
{
  "returns the result of dividing the polynomial {p1} by the unitary monomial {m} multiplied by {f},
  yielding the polynomial {p}. Otherwise, the result is $\CannotBeTransformed$.",
  prose_application = "dividing the polynomial {p1} by the unitary monomial {m} multiplied by {f},
    yields {p}",
  math_layout = ([_,_,_], _),
};

typing function polynomial_to_expr(p: polynomial) ->
         (e: expr)
{
  "transforms a polynomial {p} into the corresponding expression {e}.",
  prose_application = "\hyperlink{relation-polynomialtoexpr}{converting} polynomial {p} to an expression yields {e}"
};

typing function compare_monomial_bindings((m1: monomial, q1: Q), (m2: monomial, q2: Q)) ->
         (s: Sign)
{
  "compares two monomial bindings given by $(\vmone,
  \vqone)$ and $(\vmtwo, \vqtwo)$ and yields in {s} $-1$
  to mean that the first monomial binding should be
  ordered before the second, $0$ to mean that any
  ordering of the monomial bindings is acceptable, and
  $1$ to mean that the second monomial binding should be
  ordered before the first.",
  prose_application = "",
};

typing function monomials_to_expr(monoms: list0((m: unitary_monomial, q: Q))) ->
         (e: expr, s: Sign)
{
  "transforms a list consisting of pairs of unitary
  monomials and rational factors {monoms} (so, general
  monomials), into an expression {e}, which represents
  the absolute value of the sum of all the monomials,
  and a sign value {s}, which indicates the sign of the
  resulting sum.",
  prose_application = "\hyperlink{relation-monomialstoexpr}{converting} monomial list {monoms} to expression form
                        yields absolute value {e} and sign {s}"
};

typing function monomial_to_expr(e: expr, q: N) ->
         (new_e: expr, s: Sign)
{
  "transforms an expression {e} and rational $q$ into the
  expression {new_e}, which represents the absolute
  value of {e} multiplied by $q$, and the sign of $q$ as
  {s}.",
  prose_application = "",
};

typing function sym_add_expr(e1: expr, s1: Sign, e2: expr, s2: Sign) ->
         (e: expr, s: Sign)
{
  "symbolically sums the expressions {e1} and {e2} with
  respective signs {s1} and {s2} yielding the expression
  {e} and sign {s}.",
  prose_application = "\hyperlink{relation-symaddexpr}{symbolically summing} expressions {e1} with sign {s1}
                        and {e2} with sign {s2} yields expression {e} with sign {s}"
};

typing function unitary_monomials_to_expr(monoms: list0((Identifier, N))) ->
         (e: expr)
{
  "transforms a list of single-variable unitary monomials
  {monoms} into an expression {e}. Intuitively, {monoms}
  represented a multiplication of the single-variable
  unitary monomials.",
  prose_application = "",
};

typing function type_of(tenv: static_envs, s: Identifier) ->
         (ty: ty) | type_error
{
  "looks up the environment {tenv} for a type {ty}
  associated with an identifier {s}. The result is
  \typingerrorterm{} if {s} is not associated with any
  type.",
  prose_application = "",
};

typing function normalize_opt(tenv: static_envs, e: expr) ->
         (new_e_opt: option(expr)) | type_error
{
  "is similar to $\normalize$, except that it returns
  $\None$ when {e} is not an expression that can be
  symbolically simplified. \ProseOtherwiseTypeError",
  prose_application = "",
};

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
  prose_application = "\hyperlink{relation-symdomsubsetunions}{testing} whether {sd1} is subsumed by {sd2} in {tenv} yields {b}"
};

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
  prose_application = "\hyperlink{relation-symdomnormalize}{normalizing} {symdoms} by merging finite sets yields {new_symdoms}"
};

typing function symdom_of_type(tenv: static_envs, t: ty) ->
         (d: symdom_or_top)
{
  "transforms a type {t} in a \staticenvironmentterm{}
  {tenv} into a symbolic domain {d}. It assumes its
  input type has an \underlyingtypeterm{} which is an
  integer.",
  prose_application = "",
};

typing function symdom_of_width_expr(e: expr) ->
         (d: symdom_or_top)
{
  "assigns a symbolic domain {d} to an \underline{integer
  typed} expression {e}, where {e} is assumed to be the
  expression conveying the width of a
  \bitvectortypeterm.",
  prose_application = "",
};

typing function symdom_of_constraint(tenv: static_envs, c: int_constraint) ->
         (d: symdom)
{
  "transforms an integer constraint {c} into a symbolic
  domain {d} in the context of the
  \staticenvironmentterm{} {tenv}. It produces $\Top$
  when the expressions involved in the integer
  constraints cannot be simplified to integers.",
  prose_application = "",
};

typing function symdom_eval(tenv: static_envs, e: expr) ->
         (n: Z) | constants_set(Top)
{
  "\symbolicallysimplifiesterm{} the
  \underline{integer-typed} expression {e} and returns
  the resulting integer or $\Top$ if the result of the
  simplification is not an integer.",
  prose_application = "",
};

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
};

typing function approx_constraints(tenv: static_envs, approx: constants_set(Over,Under), cs: list1(int_constraint)) ->
         (s: powerset_finite(Z)) | constants_set(CannotOverapproximate)
{
  "conservatively approximates the non-empty list of
  constraints {cs} by a set of integers {s}. The
  approximation is over all environments consisting of
  the \staticenvironmentterm{} {tenv}. The approximation
  is either overapproximation or underapproximation,
  based on the \approximationdirectionterm{} {approx}.",
  prose_application = "\hyperlink{relation-approxconstraints}{approximating} constraints {cs} in {tenv}
                        with direction {approx} yields integer set {s}"
};

typing function approx_constraint(tenv: static_envs, approx: constants_set(Over,Under), c: int_constraint) ->
         (s: powerset(Z)) | constants_set(CannotOverapproximate)
{
  "conservatively approximates the constraint {c} by a
  set of integers {s}. The approximation is over all
  environments that consist of the
  \staticenvironmentterm{} {tenv}. The approximation is
  either overapproximation or underapproximation, based
  on the \approximationdirectionterm{} {approx}. If {c}
  cannot be overapproximated, the result is
  $\CannotOverapproximate$.",
  prose_application = "",
};

typing function make_interval(approx: constants_set(Over,Under), z1: Z, z2: Z) ->
         (s: powerset_finite(Z)) | constants_set(empty_set,CannotOverapproximate)
{
  "returns the interval between the integers {z1} and
  {z2}, or an approximation based on {approx}.",
  prose_application = "",
};

typing function approx_expr_min(tenv: static_envs, e: expr) ->
         (z: Z) | constants_set(CannotOverapproximate)
{
  "approximates the minimal integer represented by the
  expression {e} in any environment consisting of the
  \staticenvironmentterm{} {tenv}. The result, yielded
  in {z} is either an integer or
  $\CannotOverapproximate$.",
  prose_application = "",
};

typing function approx_expr_max(tenv: static_envs, e: expr) ->
         (z: Z) | constants_set(CannotOverapproximate)
{
  "approximates the maximal integer represented by the
  expression {e} in any environment consisting of the
  \staticenvironmentterm{} {tenv}. The result, yielded
  in {z} is either an integer or
  $\CannotOverapproximate$.",
  prose_application = "",
};

typing function approx_bottom_top(approx: constants_set(Under,Over)) ->
         (s: powerset(Z)) | constants_set(CannotOverapproximate)
{
  "returns in {s} either the empty set or the set of all
  integers, depending on the
  \approximationdirectionterm{} {approx}.",
  prose_application = "",
};

typing function intset_to_constraints(s: powerset_finite(Z)) ->
         (cs: list0(int_constraint))
{
  "converts a finite set of integers {s} into an
  equivalent list of constraints.",
  prose_application = "",
};

typing function approx_expr(tenv: static_envs, approx: constants_set(Over,Under), e: expr) ->
         (s: powerset(Z)) | constants_set(CannotOverapproximate)
{
  "conservatively approximates the expression {e} by a
  set of integers {s} in the \staticenvironmentterm{}
  {tenv}. The approximation is either overapproximation
  or underapproximation, based on the
  \approximationdirectionterm{} {approx}.",
  prose_application = "",
};

typing function approx_constraint_binop(tenv: static_envs, approx: constants_set(Over,Under), op: binop, s1: list1(int_constraint), s2: list1(int_constraint)) ->
         (s: list1(int_constraint), plf: precision_loss_indicator)
{
  "approximates the application of the binary operator
  {op} to lists of constraints {s1} and {s2} with the
  \approximationdirectionterm{} {approx} in the context
  of the static environment {tenv}, resulting in the
  list of constraints {s} and \precisionlossindicatorterm{}
  {plf}.",
  prose_application = "",
  math_layout = [_,_],
};

typing function approx_type(tenv: static_envs, approx: constants_set(Over,Under), t: ty) ->
         (s: powerset(Z)) | constants_set(CannotOverapproximate)
{
  "conservatively approximates the type {t} by a set of
  integers {s} in the \staticenvironmentterm{} {tenv}.
  The approximation is either overapproximation or
  underapproximation, based on the
  \approximationdirectionterm{} {approx}.",
  prose_application = "",
};

typing function constraint_binop(op: binop, cs1: list0(int_constraint), cs2: list0(int_constraint)) ->
         (new_cs: constraint_kind)
{
  "symbolically applies the binary operation {op} to the
  lists of integer constraints {cs1} and {cs2}, yielding
  the integer constraints {new_cs}.",
  prose_application = "\hyperlink{relation-constraintbinop}{applying} operator {op}
                        to constraints {cs1} and {cs2} yields constraints {new_cs}"
};

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
};

typing function possible_extremities_right(op: binop, c: expr, d: expr) ->
         (extpairs: list0((expr, expr)))
{
  "yields a list of pairs of expressions {extpairs} given
  the binary operation {op} and pair of expressions {c}
  and {d}, which are needed to form constraints for
  cases where applying {op} to {c} and {d} would lead to
  a \dynamicerrorterm{}.",
  prose_application = "",
};

typing function constraint_mod(c: int_constraint) ->
         (new_c: int_constraint)
{
  "yields a range constraint {new_c} from $0$ to the
  expression in {c} that is maximal. This is needed to
  apply the modulus operation to a pair of constraints.",
  prose_application = "",
};

typing function constraint_pow(c1: int_constraint, c2: int_constraint) ->
         (new_cs: list1(int_constraint))
{
  "yields a list of range constraints {new_cs} that are
  needed to calculate the result of applying a $\POW$
  operation to the constraints {c1} and {c2}.",
  prose_application = "\hyperlink{relation-constraintpow}{symbolically applying} the $\POW$
                        operation to {c1} and {c2} yields constraint list {new_cs}"
};

//////////////////////////////////////////////////
// Relations for Type Attributes

typing function is_builtin_singular(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \emph{builtin singular type}, yielding the result in {b}.",
    prose_application = "testing whether {ty} is a builtin singular type yields {b}",
};

typing function is_named(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \emph{named type}.",
    prose_application = "testing whether {ty} is a named type yields {b}",
};

typing function is_anonymous(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is an \emph{\anonymoustype}.",
    prose_application = "testing whether {ty} is an \anonymoustype{} yields {b}",
};

typing function is_singular(tenv: static_envs, ty: ty) -> (b: Bool) | type_error
{
    "tests whether the type {ty} is a \emph{\singulartypeterm} in the \staticenvironmentterm{} {tenv},
    yielding the result in {b}. \ProseOtherwiseTypeError",
    prose_application = "tests whether {ty} is a \singulartypeterm{} in {tenv},
    yields {b}\ProseOrTypeError",
};

typing function is_structured(ty: ty) -> (b: Bool)
{
    "tests whether the type {ty} is a \structuredtypeterm{}.",
    prose_application = "testing whether {ty} is a \structuredtypeterm{} yields {b}",
};

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
};

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
};

typing function check_constrained_integer(tenv: static_envs, t: ty) ->
         (constants_set(True)) | type_error
{
  "checks whether the type {t} is a
  \constrainedintegerterm{} type. If so, the result is
  $\True$, otherwise the result is a \typingerrorterm.",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Type Declarations

typing relation declare_type(genv: global_static_envs, name: Identifier, ty: ty, s: option((Identifier, list0(field)))) ->
         (new_genv: global_static_envs, t2: ty, s': option((Identifier, list0(field)))) | type_error
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
};

typing relation annotate_extra_fields(tenv: static_envs, name: Identifier, ty: ty, s: option((super: Identifier, extra_fields: list0(field)))) ->
         (new_tenv: static_envs, new_ty: ty, s': list0(field)) | type_error
{
  "annotates the type {ty} with the \optionalterm{} extra
  fields {s} in {tenv}, yielding the modified
  environment {new_tenv}, type {new_ty}, and \optionalterm{}
  extra fields {s'}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing relation annotate_type_opt(tenv: static_envs, ty_opt: option(t: ty)) ->
         (ty_opt': option(ty)) | type_error
{
  "annotates the type {t} inside an \optionalterm{} {ty_opt},
  if there is one, and leaves it as is if {ty_opt} is
  $\None$. \ProseOtherwiseTypeError",
  prose_application = "",
};

typing relation annotate_expr_opt(tenv: static_envs, expr_opt: option(expr)) ->
         (res: (option(expr), option(ty))) | type_error
{
  "annotates the \optionalterm{} expression {expr_opt} in
  {tenv} and returns a pair of \optionalterm{} expressions
  for the type and annotated expression in {res}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing relation declared_type(tenv: static_envs, id: Identifier) ->
         (t: ty) | type_error
{
  "retrieves the type associated with the identifier {id}
  in the \staticenvironmentterm{} {tenv}. If the
  identifier is not associated with a declared type, the
  result is a \typingerrorterm.",
  prose_application = "",
};

typing function declare_enum_labels(tenv: static_envs, name: Identifier, ids: list1(Identifier)) ->
         (new_tenv: static_envs) | type_error
{
  "updates the \staticenvironmentterm{} {tenv} with the
  identifiers {ids} listed by an \enumerationtypeterm{},
  yielding the modified environment {new_tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

typing function declare_const(genv: global_static_envs, name: Identifier, ty: ty, v: literal) ->
         (new_genv: global_static_envs) | type_error
{
  "adds a constant given by the identifier {name}, type
  {ty}, and literal {v} to the
  \globalstaticenvironmentterm{} {genv}, yielding the
  modified environment {new_genv}.
  \ProseOtherwiseTypeError",
  prose_application = "",
};

//////////////////////////////////////////////////
// Relations for Types

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
};

typing relation annotate_constraint(tenv: static_envs, c: int_constraint) ->
         (new_c: int_constraint, ses: powerset(TSideEffect)) | type_error
{
  "annotates an integer constraint {c} in the
  \staticenvironmentterm{} {tenv} yielding the annotated
  integer constraint {new_c} and \sideeffectsetterm\
  {ses}. \ProseOtherwiseTypeError",
  prose_application = "",
  math_layout = [_,_],
};

typing function get_variable_enum(tenv: static_envs, e: expr) ->
         (option((x: Identifier, labels: list1(Identifier))))
{
  "tests whether the expression {e} represents a variable
  of an \enumerationtypeterm{}. If so, the result is {x}
  --- the name of the variable and the list of labels
  {labels}, declared for the \enumerationtypeterm{}.
  Otherwise, the result is $\None$.",
  prose_application = "",
};

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
};

typing function check_underlying_integer(tenv: static_envs, t: ty) -> constants_set(True) | type_error
{
  "returns $\True$ if {t} has the \underlyingtypeterm{} of an \integertypeterm{} in the \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "checking whether {t} has the \underlyingtypeterm{} of an \integertypeterm{} in {tenv} yields
  $\True$\ProseOrTypeError",
};

//////////////////////////////////////////////////
// Relations for Type System Utilities

typing function check_no_duplicates(ids: list1(Identifier)) ->
         (constants_set(True)) | type_error
{
  "checks whether the non-empty list of identifiers {ids}
  contains a duplicate identifier. If it does not, the
  result is $\True$ and otherwise the result is a
  \typingerrorterm{}.",
  prose_application = "",
};

typing function find_bitfield_opt(name: Identifier, bitfields: list0(bitfield)) ->
         (r: option(bitfield))
{
  "returns the bitfield associated with the name {name}
  in the list of bitfields {bitfields}, if there is one.
  Otherwise, the result is $\None$.",
  prose_application = "",
};

typing function type_of_array_length(size: array_index) ->
         (t: ty)
{
  "returns the type for the array length {size} in {t}.",
  prose_application = "",
};

typing function with_empty_local(genv: global_static_envs) ->
         (tenv: static_envs)
{
  "constructs a \staticenvironmentterm{} from the
  \globalstaticenvironmentterm{} {genv} and the empty
  \localstaticenvironmentterm.",
  prose_application = "",
};

typing function check_var_not_in_env(tenv: static_envs, id: Strings) ->
         (constants_set(True)) | type_error
{
  "checks whether {id} is already declared in {tenv}. If
  it is, the result is a \typingerrorterm{}, and
  otherwise the result is $\True$.",
  prose_application = "",
};

typing function check_var_not_in_genv(genv: global_static_envs, id: Strings) ->
         (constants_set(True)) | type_error
{
  "checks whether {id} is already declared in the
  \globalstaticenvironmentterm{} {genv}. If it is, the
  result is a \typingerrorterm{}, and otherwise the
  result is $\True$.",
  prose_application = "",
};

typing function add_local(tenv: static_envs, id: Identifier, ty: ty, ldk: local_decl_keyword) ->
         (new_tenv: static_envs)
{
  "adds the identifier {id} as a local storage element
  with type {ty} and local declaration keyword {ldk} to
  the local environment of {tenv}, resulting in the
  \staticenvironmentterm{} {new_tenv}.",
  prose_application = "",
};

typing function is_undefined(tenv: static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether the identifier {x} is defined as a
  storage element in the \staticenvironmentterm{}
  {tenv}.",
  prose_application = "",
};

typing function is_global_undefined(genv: global_static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether the identifier {x} is defined in the
  \globalstaticenvironmentterm{} {genv} when subprogram
  definitions are ignored (see
  \RequirementRef{GlobalNamespace}), yielding the result
  in {b}.",
  prose_application = "",
};

typing function is_local_undefined(lenv: local_static_envs, x: Identifier) ->
         (b: Bool)
{
  "checks whether {x} is declared as a local storage
  element in the static local environment {lenv},
  yielding the result in {b}.",
  prose_application = "",
};

typing function lookup_constant(tenv: static_envs, s: Identifier) ->
         (v: literal) | constants_set(bot)
{
  "looks up the environment {tenv} for a constant {v}
  associated with an identifier {s}. The result is
  $\bot$ if {s} is not associated with any constant.",
  prose_application = "",
};

typing function add_global_constant(genv: global_static_envs, name: Identifier, v: literal) ->
         (new_genv: global_static_envs)
{
  "binds the identifier {name} to the literal {v} in the
  \globalstaticenvironmentterm{} {genv}, yielding the
  updated \globalstaticenvironmentterm{} {new_genv}.",
  prose_application = "",
};

typing function lookup_immutable_expr(tenv: static_envs, x: Identifier) ->
         (e: expr) | constants_set(bot)
{
  "looks up the \staticenvironmentterm{} {tenv} for an
  immutable expression associated with the identifier
  {x}, returning $\bot$ if there is none.",
  prose_application = "",
};

typing function add_global_immutable_expr(tenv: static_envs, x: Identifier, e: expr) ->
         (new_tenv: static_envs)
{
  "binds the identifier {x}, which is assumed to name a
  global storage element, to the expression {e}, which
  is assumed to be \symbolicallyevaluableterm, in the
  \staticenvironmentterm{} {tenv}, resulting in the
  updated environment {new_tenv}.",
  prose_application = "",
};

typing function add_local_immutable_expr(tenv: static_envs, x: Identifier, e: expr) ->
         (new_tenv: static_envs)
{
  "binds the identifier {x}, which is assumed to name a
  local storage element, to the expression {e}, which is
  assumed to be \symbolicallyevaluableterm, in the
  \staticenvironmentterm{} {tenv}, resulting in the
  updated environment {new_tenv}.",
  prose_application = "",
};

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
};

typing function add_immutable_expression(
    tenv: static_envs,
    ldk: local_decl_keyword,
    e_opt: option((e: expr, ses_e: powerset(TSideEffect))), x: Identifier) ->
         (new_tenv: static_envs) | type_error
{
  "conditionally updates the \staticenvironmentterm{}
  {tenv} for a \localdeclarationitem{} {ldk}, an
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
  math_layout = [_,_],
};

typing function add_subprogram(tenv: static_envs, name: Strings, func_def: func, s: powerset(TSideEffect)) ->
         (new_tenv: static_envs)
{
  "updates the global environment of {tenv} by mapping
  the (unique) subprogram identifier {name} to the
  function definition {func_def} and
  \sideeffectdescriptorsterm{} {s} in {tenv}, resulting
  in a new \staticenvironmentterm{} {new_tenv}.",
  prose_application = "",
};

typing function add_type(tenv: static_envs, name: Identifier, ty: ty, f: TPurity) ->
         (new_tenv: static_envs)
{
  "binds the type {ty} and \purity{} {f} to the
  identifier {name} in the \staticenvironmentterm{}
  {tenv}, yielding the modified \staticenvironmentterm{}
  {new_tenv}.",
  prose_application = "",
};
