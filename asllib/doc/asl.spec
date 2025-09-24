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

typedef Bool
{  prose_description = "Boolean",
    math_macro = \Bool,
};

typedef Bit
{ prose_description = "bit", };

typedef N
{  prose_description = "natural number",
    math_macro = \N,
};

typedef Z
{ prose_description = "integer",
    math_macro = \Z,
};

typedef Q
{ prose_description = "rational",
   math_macro = \Q,
};

typedef Identifier
{ prose_description = "identifier",
   math_macro = \Identifier,
};

typedef Strings
{ prose_description = "string",
   math_macro = \Strings,
};

typedef ASTLabels
{ prose_description = "AST label",
   math_macro = \ASTLabels,
};

////////////////////////////////////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////////////////////////////////////

ast literal { prose_description = "literal", } =
    | L_Int(whole_number: Z)
    { prose_description = "integer literal for {whole_number}", }
    | L_Bool(boolean_value: Bool)
    { prose_description = "Boolean literal for {boolean_value}", }
    | L_Real(rational_number: Q)
    { prose_description = "rational literal for {rational_number}", }
    | L_Bitvector(bits: list0(Bit))
    { prose_description = "bitvector literal for {bits}", }
    | L_String(string: Strings)
    { prose_description = "string literal for {string}", }
    | L_Label(enumeration_label: Identifier)
    { prose_description = "enumeration label {enumeration_label}", }
;

ast unop { prose_description = "unary operator", } =
    | BNOT
    { prose_description = "Boolean negation operator", }
    | NEG
    { prose_description = "integer negation operator", }
    | NOT
    { prose_description = "bitvector negation operator", }
;

ast binop { prose_description = "binary operator", } =
    | BAND
    { prose_description = "Boolean conjunction operator", }
    | BOR
    { prose_description = "Boolean disjunction operator", }
    | IMPL
    { prose_description = "Boolean implication operator", }
    | BEQ
    { prose_description = "Boolean bi-implication operator", }
    | EQ
    { prose_description = "equality operator", }
    | NE
    { prose_description = "inequality operator", }
    | GT
    { prose_description = "greater than operator", }
    | GE
    { prose_description = "greater than or equal to operator", }
    | LT
    { prose_description = "less than operator", }
    | LE
    { prose_description = "less than or equal to operator", }
    | ADD
    { prose_description = "addition operator", }
    | SUB
    { prose_description = "subtraction operator", }
    | OR
    { prose_description = "bitvector or operator", }
    | XOR
    { prose_description = "bitvector xor operator", }
    | AND
    { prose_description = "bitvector and operator", }
    | MUL
    { prose_description = "multiplication operator", }
    | DIV
    { prose_description = "exact division operator", }
    | DIVRM
    { prose_description = "rounding division operator", }
    | MOD
    { prose_description = "modulus operator", }
    | SHL
    { prose_description = "shift left operator", }
    | SHR
    { prose_description = "shift right operator", }
    | RDIV
    { prose_description = "rational division operator", }
    | POW
    { prose_description = "exponentiation operator", }
    | BV_CONCAT
    { prose_description = "bitvector concatenation operator", }
    | STR_CONCAT
    { prose_description = "string concatenation operator", }
;

render unop_and_binop = unop(-), binop(-);

ast expr { prose_description = "expression", } =
////////////////////////////////////////////////
// Unyped AST
////////////////////////////////////////////////
    | E_Literal(value: literal)
    { prose_description = "literal expression for {value}", }
    | E_Var(name: Identifier)
    { prose_description = "variable expression for {name}", }
    | E_ATC(source: expr, type: ty)
    { prose_description = "asserting type conversion for the source expression {e} and type {type}", }
    | E_Binop(operator: binop, left: expr, right: expr)
    { prose_description = "binary expression for the operator {operator}, left expression {left} and right expression {right}", }
    | E_Unop(operator: unop, subexpression: expr)
    { prose_description = "unary expression for the unary operator {operator} and subexpression {e}", }
    | E_Call(call_descriptor: call)
    { prose_description = "call expression for the call descriptor {call_descriptor}", }
    | E_Slice(base: expr, slices: list0(slice))
    { prose_description = "slice expression for the base expression {base} and slices {slices}", }
    | E_Cond(test: expr, true_branch: expr, false_branch: expr)
    { prose_description = "condition expression for the test expression {test}
            true branch expression {true_branch} and
            false branch expression {false_branch}",
    }
    | E_GetArray(base: expr, index: expr)
    { prose_description = "array read expression for the base expression {base} and index expression {index}", }
    | E_GetField(record: expr, field_name: Identifier)
    { prose_description = "field read expression for the record expression {record} and field name {field_name}", }
    | E_GetFields(record: expr, field_names: list0(Identifier))
    { prose_description = "multi-field read expression for the record expression {record} and field names {field_names}", }
    | E_Record(record_type: ty, field_initializers: list0( (field_name: Identifier, initializer: expr) ))
    { prose_description = "a record construction expression for the record type {record_type} and field initializers {field_initializers}", }
    | E_Tuple(components: list1(expr))
    { prose_description = "a tuple expression for the components {components}", }
    | E_Arbitrary(type: ty)
    { prose_description = "an arbitrary value choice expression for {type}", }
    | E_Pattern(discriminant: expr, pattern: pattern)
    { prose_description = "a pattern expression for {discriminant} and {pattern}", }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////
    | E_GetItem(base: expr, index: N)
    { prose_description = "an access to tuple expression {base} of the component at index {index}", }
    | E_Array[length: expr, value: expr]
    { prose_description = "array construction {base} of the component at index {index}", }
    | E_EnumArray[labels: list1(Identifier), value: expr]
    { prose_description = "array construction {base} of the component at index {index}", }
    | E_GetEnumArray(base: expr, key: expr)
    { prose_description = "access to enumeration-indexed array {base} with key expression {key}", }
    | E_GetCollectionFields(collection_name: Identifier, field_names: list0(Identifier))
    { prose_description = "access to the list of fields given by {field_names} of the collection variable named {collection_name}", }
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
{ prose_description = "\texttt{0}", math_macro = \zerobit };

constant one_bit
{ prose_description = "\texttt{1}", math_macro = \onebit };

constant x_bit
{ prose_description = "\texttt{x}", math_macro = \xbit };

ast pattern { prose_description = "pattern", } =
    | Pattern_All
    { prose_description = "match-all pattern", }
    | Pattern_Any(patterns: list0(pattern))
    { prose_description = "match-any pattern", }
    | Pattern_Geq(subexpression: expr)
    { prose_description = "greater-or-equal pattern", }
    | Pattern_Leq(subexpression: expr)
    { prose_description = "less-or-equal pattern", }
    | Pattern_Mask(mask_constant: list0(constants_set(zero_bit, one_bit, x_bit)))
    { prose_description = "mask pattern", }
    | Pattern_Not(subexpression: expr)
    { prose_description = "negation pattern", }
    | Pattern_Range(lower: expr, upper: expr)
    { prose_description = "range pattern", }
    | Pattern_Single(subexpression: expr)
    { prose_description = "single-expression pattern", }
    | Pattern_Tuple(patterns:list0(pattern))
    { prose_description = "tuple pattern", }
;

ast slice
{ prose_description = "slice", } =
////////////////////////////////////////
// Untyped AST
////////////////////////////////////////
    | Slice_Single(index: expr)
    { prose_description = "slice at position {index}", }
    | Slice_Range(upper_index: expr, lower_index: expr)
    { prose_description = "slice from position {upper_index} down to position {lower_index}", }
    | Slice_Length(start_index: expr, length: expr)
    { prose_description = "slice from position {start_index} of {length} elements", }
    | Slice_Star(factor: expr, scale: expr)
    { prose_description = "slice from position {factor}*{scale} of {scale} elements", }

////////////////////////////////////////
// Typed AST
////////////////////////////////////////
    | typed_Slice_Length(start_index: expr, length: expr)
    {
        prose_description = "slice from position {start_index} of {length} elements",
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

ast call { prose_description = "call descriptor", } =
    [   name: Strings,
        params: list0(expr),
        args: list0(expr),
        call_type: subprogram_type,
    ]
    { prose_description = "call of {call_type} subprogram {name}with parameters {params}, arguments {args}", }
;

render calls = expr(E_Call), stmt(S_Call);

ast ty { prose_description = "type", } =
    | T_Int(kind: constraint_kind)
    { prose_description = "integer type", }
    | T_Real
    { prose_description = "real type", }
    | T_String
    { prose_description = "string type", }
    | T_Bool
    { prose_description = "Boolean type", }
    | T_Bits(width: expr, bitfields: list0(bitfield))
    { prose_description = "bitvector type of bitwidth {width} and bitfields {bitfields}", }
    | T_Tuple(component_types: list0(ty))
    { prose_description = "tuple type with components types {component_types}", }
    | T_Array(index: array_index, element_type: ty)
    { prose_description = "integer type with {array_index} and element_type {element_type}", }
    | T_Named(type_name: Identifier)
    { prose_description = "named type with name {type_name}", }
    | T_Enum(labels: list1(Identifier))
    { prose_description = "enumeration type with labels {labels}", }
    | T_Record(fields: list0(field))
    { prose_description = "record type with fields {record_fields}", }
    | T_Exception(fields: list0(field))
    { prose_description = "exception type with fields {record_fields}", }
    | T_Collection(fields: list0(field))
    { prose_description = "collection type with fields {record_fields}", }
;

ast constraint_kind { prose_description = "constraint kind", } =
//////////////////////////////////////////////////
// Untyped AST
//////////////////////////////////////////////////
    | Unconstrained
    { prose_description = "no constraint", }
    | WellConstrained(constraints: list1(int_constraint))
    { prose_description = "list of constraints {constraints}", }
    | Parameterized(parameter_name: Identifier)
    { prose_description = "parameter constraint for {parameter_name}", }
    | PendingConstrained
    { prose_description = "pending constraint", }

//////////////////////////////////////////////////
// Typed AST
//////////////////////////////////////////////////
    | typed_WellConstrained(constraints: list1(int_constraint), precision_loss: precision_loss_indicator)
    {
        prose_description = "list of constraints {constraints} with a \Proseprecisionlossindicator{} {precision_loss}",
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

ast precision_loss_indicator { prose_description = "\Proseprecisionlossindicator{}", } =
    | Precision_Full
    { prose_description = "no precision loss", }
    | Precision_Lost
    { prose_description = "some precision loss", }
;

render ty_int_constraint_and_kind = ty(T_Int), int_constraint(-), constraint_kind(-);

ast int_constraint { prose_description = "integer constraint", } =
    | Constraint_Exact(subexpression: expr)
    { prose_description = "exact constraint for the subexpression {subexpression}" }
    | Constraint_Range(start_expression: expr, end_expression: expr)
    { prose_description = "range constraint from the start expression {start_subexpression} to the end expression {end_subexpression}" }
;

ast bitfield { prose_description = "bitfield", } =
    | BitField_Simple(name: Identifier, slices: list0(slice))
    { prose_description = "bitfield named {name} with slices {slices}", }
    | BitField_Nested(name: Identifier, slices: list0(slice), nested_bitfields: list0(bitfield))
    { prose_description = "bitfield named {name} with slices {slices} and nested bitfields {nested_bitfields}", }
    | BitField_Type(name: Identifier, slices: list0(slice), type: ty)
    { prose_description = "bitfield named {name} with slices {slices} and type {type}", }
;

ast array_index { prose_description = "array index", } =
//////////////////////////////////////////////////
// Untyped AST
//////////////////////////////////////////////////
    ArrayLength_Expr(length: expr)
    { prose_description = "integer length expression {length}", }

//////////////////////////////////////////////////
// Typed AST
//////////////////////////////////////////////////
    | ArrayLengthEnum(enumeration_name: Identifier, enumeration_labels: list1(Identifier))
    { prose_description = "index for the enumeration {enumeration_name} with labels {enumeration_labels}", }
;

render untyped_array_index = array_index(ArrayLength_Expr);
render typed_array_index = array_index(ArrayLengthEnum);

ast field { prose_description = "field", } =
    (name: Identifier, type: ty)
    { prose_description = "field named {name} with type {type}", }
;

ast typed_identifier { prose_description = "typed identifier", } =
    (name: Identifier, type: ty)
    { prose_description = "identifier {name} with type {type}", }
;

ast lexpr { prose_description = "\assignableexpression{}", } =
////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////
    | LE_Discard
    { prose_description = "discarding \assignableexpression{}", }
    | LE_Var(var: Identifier)
    { prose_description = "assignable variable expression for {var}", }
    | LE_Slice(base: lexpr, slices: list0(slice))
    { prose_description = "assignable slice expression for {base} and {slices}", }
    | LE_SetArray(base: lexpr, index: expr)
    { prose_description = "assignable array write expression for {base} at index {index}", }
    | LE_SetField(base: lexpr, field_name: Identifier)
    { prose_description = "assignable field write expression for {base} and field name {field_name}", }
    | LE_SetFields(base: lexpr, field_names: list0(Identifier))
    { prose_description = "assignable multi-field write expression for {base} and field names {field_names}", }
    | LE_Destructuring(subexpressions: list0(lexpr))
    { prose_description = "multi-assignment for the list of \assignableexpressions{} {subexpressions}", }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////

    | LE_SetEnumArray(base: lexpr, index: expr)
    { prose_description = "assignable expression for the enumeration-indexed array {base} at index {index}", }
    | LE_SetCollectionFields(collection_name: Identifier, field_names: list0(Identifier))
    { prose_description = "assignable expression for the collection named {collection_name} and field names {field_names}", }
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

ast local_decl_keyword { prose_description = "local declaration keyword", } =
    | LDK_Var
    { prose_description = "local variable", }
    | LDK_Let
    { prose_description = "local immutable variable", }
;

ast local_decl_item { prose_description = "local declaration item", } =
  | LDI_Var(variable_name: Identifier)
  { prose_description = "local declaration item for the variable {variable_name}", }
  | LDI_Tuple(variable_names: list0(Identifier))
  { prose_description = "local declaration item for the list of variables {variable_names}", }
;

render local_decl_keyword_and_item = local_decl_keyword(-), local_decl_item(-);

ast for_direction { prose_description = "direction" } =
    | UP
    { prose_description = "upward", }
    | DOWN
    { prose_description = "downward", }
;

ast stmt { prose_description = "statement" } =
////////////////////////////////////////////////
// Untyped AST
////////////////////////////////////////////////
  | S_Pass
  { prose_description = "pass statement", }
  | S_Seq(first: stmt, second: stmt)
  { prose_description = "sequence statement for {first} and {second}", }
  | S_Decl(keyword: local_decl_keyword, item: local_decl_item, type_annotation: option(ty), initializer: option(expr))
  { prose_description = "declaration statement with keyword {keyword},
    item {item},
    optional type annotation {type_annotation}, and
    optional initializer {initializer}", }
  | S_Assign(left_hand_side: lexpr, right_hand_side: expr)
  { prose_description = "assignment statement of \assignableexpression{} {left_hand_side} by {right_hand_side}", }
  | S_Call(call_descriptor: call)
  { prose_description = "call statement with descriptor {call_descriptor}", }
  | S_Return(return_value: option(expr))
  { prose_description = "return statement with optional return expression {return_value}", }
  | S_Cond(condition: expr, then_statement: stmt, else_statement: stmt)
  { prose_description = "condition statement with condition expression {condition},
    true branch statement {then_statement}, and
    false branch statement {else_statement}", }
  | S_Assert(condition: expr)
  { prose_description = "assertion statement with {condition}", }
  | S_For [
    index_name : Identifier,
    start_e    : expr,
    dir        : for_direction,
    end_e      : expr,
    body       : stmt,
    limit      : option(expr)
  ]
  { prose_description = "for loop statement with
    index variable {index_name},
    start expression {start_e},
    direction {dir},
    end expression {end_e},
    body {body},
    and optional loop limit {limit}", }
  | S_While(condition: expr, loop_limit: option(expr), body: stmt)
  { prose_description = "while statement with condition {condition},
    optional loop limit {loop_limit},
    and body {body}", }
  | S_Repeat(body: stmt, condition: expr, loop_limit: option(expr))
  { prose_description = "repeat statement with body {body},
    condition {condition},
    and optional loop limit {loop_limit}", }
  | S_Throw(exception: expr)
  { prose_description = "throw statement with exception expression {exception}", }
  | S_Try(statement: stmt, catchers: list0(catcher), otherwise: option(stmt))
  { prose_description = "try statement with statement {stmt},
    list of catchers {catchers},
    and otherwise optional statement {otherwise}", }
  | S_Print(arguments: list0(expr), newline: Bool)
  { prose_description = "print statement with list of arguments {arguments} and newline choice {newline}", }
  | S_Pragma(pragma_name: Identifier, arguments: list0(expr))
  { prose_description = "pragma statement for the pragma name {pragma_name} and list of arguments {arguments}", }
  | S_Unreachable
  { prose_description = "unreachable statement", }

////////////////////////////////////////////////
// Typed AST
////////////////////////////////////////////////
   | typed_S_Throw(exception: expr, exception_type: ty)
    {
        prose_description = "throw statement with exception expression {exception} and inferred type {exception_type}",
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

ast case_alt { prose_description = "case alternative" } =
    [ pattern: pattern, where: option(expr), stmt: stmt ]
    { prose_description = "case alternative for the pattern {pattern},
        optional where expression {where},
        and statement {stmt}"
    }
;

ast catcher { prose_description = "catcher", } =
 (variable: option(Identifier), guard_type: ty, execute: stmt)
 { prose_description = "catcher for an exception of type {guard_type}
    with the optional variable name {variable}
    executing {execute}", }
;

ast subprogram_type { prose_description = "subprogram type", } =
    | ST_Procedure
    { prose_description = "procedure", }
    | ST_Function
    { prose_description = "function", }
    | ST_Getter
    { prose_description = "getter", }
    | ST_Setter
    { prose_description = "setter", }
;

ast qualifier { prose_description = "subprogram qualifier", } =
    | Pure
    { prose_description = "pure", }
    | Readonly
    { prose_description = "readonly", }
    | Noreturn
    { prose_description = "noreturn", }
;

ast override_info { prose_description = "override qualifier", } =
    | Impdef
    { prose_description = "impdef qualifier", }
    | Implementation
    { prose_description = "implementation qualifier", }
;

ast func { prose_description = "subprogram descriptor", } =
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
    { prose_description = "a subprogram descriptor for the subprogram name {name},
        parameter list {parameters},
        arguments {args},
        body {body},
        optional return type {return_type},
        subprogram type {subprogram_type},
        optional recursion limit {recurse_limit},
        builtin flag {builtin},
        subprogram qualifier {qualifier},
        and override qualifier {override}
    ", }
;

ast global_decl_keyword { prose_description = "global declaration keyword", } =
 | GDK_Constant
 { prose_description = "constant", }
 | GDK_Config
 { prose_description = "configuration", }
 | GDK_Let
 { prose_description = "immutable global storage", }
 | GDK_Var
 { prose_description = "mutable global storage", }
;

ast global_decl { prose_description = "global storage declaration", } =
    [
    keyword : global_decl_keyword,
    name : Identifier,
    ty : option(ty),
    initial_value : option(expr)
    ]
    { prose_description = "global storage declaration with the
        keyword {keyword},
        element name {name},
        optional type annotation {ty},
        and optional initializer {initial_value}" }
;

ast decl { prose_description = "global declaration", } =
    | D_Func(descriptor: func)
    { prose_description = "subprogram declaration with descriptor {descriptor}", }
    | D_GlobalStorage(storage_declaration: global_decl)
    { prose_description = "global storage declaration with {storage_declaration}", }
    | D_TypeDecl(type_name: Identifier, annotation: ty, extra_fields: option((super_type: Identifier, with_fields: list0(field))))
    { prose_description = "type declaration for the type name {type_name},
        type annotation {annotation},
        optional extra fields {extra_fields} in addition to those in {super_type}", }
    | D_Pragma(pragma_name: Identifier, arguments: list0(expr))
    { prose_description = "pragma declaration for the pragma name {pragma_name} and
        arguments {arguments}", }
;

render decl_global_storage = decl(D_GlobalStorage), global_decl(-), global_decl_keyword(-);
render decl_func = decl(D_Func), func(-) ,subprogram_type(-), qualifier(-), override_info(-), typed_identifier(-);
render decl_type = decl(D_TypeDecl), field(-);
render decl_global_pragma = decl(D_Pragma);

ast spec { prose_description = "specification", } =
 list0((declarations: decl))
  { prose_description = "list of declarations {declarations}", }
;

////////////////////////////////////////////////////////////////////////////////
// Type System Types
////////////////////////////////////////////////////////////////////////////////

typedef static_envs
    {
        prose_description = "static environment",
        math_macro = \staticenvs,
    } =
 (G: global_static_envs, L: local_static_envs)
    {
        prose_description = "static environment with global static environment {G} and local static environment {L}",
    }
;

typedef global_static_envs
    {
        prose_description = "global static environment",
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
    {  prose_description = "global static environment with", }
;

typedef local_static_envs
    {
        prose_description = "local static environment",
        math_macro = \localstaticenvs,
    } =
    [
        local_storage_types: partial Identifier -> (element_type: ty, declared_keyword: local_decl_keyword),
        expr_equiv: partial Identifier -> expr,
        return_type: option(ty)
    ]
    {  prose_description = "local static environment", }
;

render static_envs_and_components = static_envs(-), global_static_envs(-), local_static_envs(-);

constant empty_tenv {
    prose_description = "empty static environment",
    math_macro = \emptytenv,
};

typedef type_error
    {
        prose_description = "\typingerrorterm{}",
    } =
    TypeError(error_code: Strings)
    {
        prose_description = "\typingerrorterm{} with error code {error_code}",
    }
;

////////////////////////////////////////////////////////////////////////////////
// Side Effects Types

constant SE_Pure { prose_description = "purity descriptor for the evaluation of a \pure{} construct", };
constant SE_Readonly { prose_description = "purity descriptor for the evaluation of a \readonly{} construct", };
constant SE_Impure { prose_description = "purity descriptor for the evaluation of a construct that is neither \pure{} nor \readonly{}", };

typedef TPurity { prose_description = "purity descriptor", } =
    constants_set(SE_Pure, SE_Readonly, SE_Impure)
    { prose_description = "\purity", }
;

typedef TSideEffect { prose_description = "\sideeffectdescriptorterm{}", } =
    | LocalEffect(purity: TPurity)
    { prose_description = "local \sideeffectdescriptorterm{} with \purity{} {purity}" }
    | GlobalEffect(purity: TPurity)
    { prose_description = "global \sideeffectdescriptorterm{} with \purity{} {purity}" }
    | Immutability(immutable: Bool)
    { prose_description = "\sideeffectdescriptorterm{} for a construct that accesses storage elements whose immutability is given by {immutable}" }
;

////////////////////////////////////////////////////////////////////////////////
// Dynamic Semantics Types
////////////////////////////////////////////////////////////////////////////////

typedef native_value
    {
        prose_description = "\nativevalue{}",
        math_macro = \nativevalue,
    } =
    | NV_Literal(l: literal)
    { prose_description = "\nativevalue{} for the literal {l}", }
    | NV_Vector(values: list0(native_value))
    { prose_description = "\nativevalue{} for the vector {values}", }
    | NV_Record(field_to_value: partial Identifier -> native_value)
    { prose_description = "\nativevalue{} record {field_to_value}", }
;

typedef tint
   {
       prose_description = "native integer type",
       math_macro = \tint,
   } =
   (NV_Literal(L_Int(v: Z)))
   {
       prose_description = "native integer for {v}",
   }
;

typedef tbool
   {
       prose_description = "native Boolean type",
       math_macro = \tbool,
   } =
   (NV_Literal(L_Bool(v: Bool)))
   {
       prose_description = "native Boolean for {v}",
   }
;

typedef treal
   {
       prose_description = "native real type",
       math_macro = \treal,
   } =
   (NV_Literal(L_Real(v: Q)))
   {
       prose_description = "native real for {v}",
   }
;

typedef tlabel
   {
       prose_description = "native label type",
       math_macro = \tlabel,
   } =
   (NV_Literal(L_Label(v: Identifier)))
   {
       prose_description = "native label for {v}",
   }
;

typedef tstring
   {
       prose_description = "native string type",
       math_macro = \tstring,
   } =
   (NV_Literal(L_String(v: Strings)))
   {
       prose_description = "native string for {v}",
   }
;

typedef tbitvector
   {
       prose_description = "native bitvector type",
       math_macro = \tbitvector,
   } =
   (NV_Literal(L_Bitvector(v: list0(Bit))))
   {
       prose_description = "native bitvector for {v}",
   }
;

typedef tvector
   {
       prose_description = "native vector type",
       math_macro = \tvector,
   } =
   (NV_Vector(values: list0(native_value)))
   {
       prose_description = "native vector for {values}",
   }
;

typedef trecord
   {
       prose_description = "native record type",
       math_macro = \trecord,
   } =
   (NV_Record(field_to_value: partial Identifier -> native_value))
   {
       prose_description = "native record for {field_to_value}",
   }
;

render native_types = tint(-), tbool(-), treal(-), tlabel(-), tstring(-), tbitvector(-), tvector(-), trecord(-);

typedef dynamic_envs
    {
        prose_description = "dynamic environment",
        math_macro = \dynamicenvs,
    } =
    (G: global_dynamic_envs, L: local_dynamic_envs)
    {
        prose_description = "dynamic environment with global dynamic environment {G} and local dynamic environment {L}",
    }
;

typedef global_dynamic_envs
    {
        prose_description = "global dynamic environment",
        math_macro = \globaldynamicenvs,
    } =
    [
        storage: partial Identifier -> native_value,
        pending_calls: partial Identifier -> N
    ]
    {
        prose_description = "global dynamic environment with storage mapping {storage} and pending calls mapping {pending_calls}",
    }
;

typedef local_dynamic_envs
    {
        prose_description = "local dynamic environment",
        math_macro = \localdynamicenvs,
    } =
    (partial Identifier -> native_value)
    {
        prose_description = "local dynamic environment as a partial mapping from identifiers to native values",
    }
;

render dynamic_envs_and_components = dynamic_envs(-), global_dynamic_envs(-), local_dynamic_envs(-);

constant empty_denv
    {
        prose_description = "empty dynamic environment",
        math_macro = \emptydenv,
    }
;

typedef envs
    {
        prose_description = "environment",
        math_macro = \envs,
    } =
    (static: static_envs, dynamic: dynamic_envs)
    {
        prose_description = "environment with static environment {static} and dynamic environment {dynamic}",
    }
;

////////////////////////////////////////////////////////////////////////////////
// Concurrent Execution Graphs

constant Read
{ prose_description = "read effect", math_macro = \Read };

constant Write
{ prose_description = "write effect", math_macro = \Write };

constant asldata
{ prose_description = "data dependency", math_macro = \asldata };

constant aslctrl
{ prose_description = "control dependency", math_macro = \aslctrl };

constant aslpo
{ prose_description = "program order dependency", math_macro = \aslpo };

typedef Labels
{
    prose_description = "execution graph labels",
    math_macro = \Labels,
} =
    constants_set(asldata, aslctrl, aslpo)
    { prose_description = "set of execution graph labels including data, control, and program order dependencies", }
;

typedef Nodes
{
    prose_description = "execution graph nodes",
    math_macro = \Nodes,
} =
    (node_id: N, effect_type: constants_set(Read, Write), storage_element: Identifier)
    {
        prose_description = "execution graph node with identifier {node_id}, operation type {operation_type}, and storage element {storage_element}",
    }
;

typedef XGraphs
{
    prose_description = "\executiongraph{}",
    math_macro = \XGraphs,
} =
    (vertices: powerset(Nodes), edges: powerset((source: Nodes, label: Labels, target: Nodes)), output_nodes: powerset(Nodes))
    {
        prose_description = "\executiongraph{} with vertices {vertices}, labeled edges {edges}, and output nodes {output_nodes}",
    }
;

render xgraphs_and_components = XGraphs(-), Nodes(-), Labels(-);

constant empty_graph { prose_description = "empty execution graph", math_macro = \emptygraph, };

ast symdom { prose_description = "\symbolicdomain{}", } =
    | Finite(powerset_finite(Z))
    { prose_description = "symbolic finite set integer domain", }
    | ConstrainedDom(int_constraint)
    { prose_description = "symbolic constrained integer domain", }
;

ast symdom_or_top { prose_description = "symbolic integer set", } =
    | Top
    { prose_description = "symbolic unconstrained integer domain", }
    | Subdomains(list1(symdom))
    { prose_description = "symbolic subdomains", }
;

render symbolic_domains = symdom(-), symdom_or_top(-);

////////////////////////////////////////////////////////////////////////////////
// Dynamic Semantics Configurations

typedef TNormal
{
    prose_description = "normal execution result",
} =
    | ResultExpr(value_and_graph: (native_value, XGraphs), environment: envs)
    { prose_description = "expression result with value-graph pair {value_and_graph} and {environment}", }
    | ResultExprSEF(value: native_value, graph: XGraphs)
    { prose_description = "side-effect-free expression result with {value} and {graph}", }
    | ResultLexpr(graph: XGraphs, environment: envs)
    { prose_description = "assignable expression result with {graph} and {environment}", }
    | ResultLDI(graph: XGraphs, environment: envs)
    { prose_description = "local declaration item result with {graph} and {environment}", }
    | ResultSlices(slices_and_graph: (list0((native_value, native_value)), XGraphs), environment: envs)
    { prose_description = "slices result with slice list and graph {slices_and_graph} and {environment}", }
    | ResultExprList(values_and_graph: (list0(native_value), XGraphs), environment: envs)
    { prose_description = "expression list result with values and graph {values_and_graph} and {environment}", }
    | ResultExprListM(value_graph_pairs: list0((native_value, XGraphs)), environment: envs)
    { prose_description = "expression list result with value-graph pairs {value_graph_pairs} and {environment}", }
    | ResultPattern(boolean_value: tbool, graph: XGraphs)
    { prose_description = "pattern result with {boolean_value} and {graph}", }
    | ResultCall(value_graph_pairs: list0((native_value, XGraphs)), environment: envs)
    { prose_description = "call result with value-graph pairs {value_graph_pairs} and {environment}", }
;

typedef TThrowing
{
    prose_description = "throwing execution result",
} =
    Throwing(exception_value: native_value, exception_type: ty, graph: XGraphs, environment: envs)
    { prose_description = "throwing result with exception value {exception_value}, type {exception_type}, {graph}, and {environment}", }
;

typedef TContinuing
{
    prose_description = "continuing execution result",
} =
    Continuing(graph: XGraphs, environment: envs)
    { prose_description = "continuing result with {graph} and {environment}", }
;

typedef TReturning
{
    prose_description = "returning execution result",
} =
    Returning(values_and_graph: (list0(native_value), XGraphs), environment: envs)
    { prose_description = "returning result with values and graph {values_and_graph} and {environment}", }
;

typedef TDynError
{
    prose_description = "dynamic error result",
} =
    DynamicError(error_code: Strings)
    { prose_description = "dynamic error with error code {error_code}",
        math_macro = \DynamicError,
    }
;

constant Diverging
{ prose_description = "diverging execution",  };

typedef TDiverging
{
    prose_description = "diverging execution result",
} =
    constants_set(Diverging)
    { prose_description = "diverging execution result", }
;

relation annotate_literal(tenv: static_envs, l: literal) -> (t: ty)
{
    prose_description = "annotates a literal {l} in the \staticenvironmentterm{} {tenv}, resulting in a type {t}.",
    prose_application = "annotating {l} in {tenv} yields {t}",
};

relation annotate_expr(tenv: static_envs, e: expr) -> (t: ty, new_e: expr, ses: powerset(TSideEffect)) | type_error
{
    prose_description = "annotates the expression {e} in the \staticenvironmentterm{} {tenv},
                        resulting in the following:
                        {t} is the type inferred for {e};
                        {new_e} is the \typedast{} for {e}, also known as the \emph{annotated expression};
                        and {ses} is the \sideeffectsetterm{} inferred for {e}. \ProseOtherwiseTypeError",
    prose_application = "annotating {e} in {tenv} yields
        {t}, the annotated expression {new_e} and {ses}\ProseOrTypeError",
    math_layout = ((_,_),(_,_)),
};
