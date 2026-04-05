////////////////////////////////////////////////////////////////////////////////
// AST builders
////////////////////////////////////////////////////////////////////////////////

typedef build_error_code
{
  "build error code",
} =
  | BE_ParseError
  { math_macro = \ParseError }
  | BE_ReservedIdentifier
  { math_macro = \ReservedIdentifier }
  | BE_BinopPrecedence
  { math_macro = \BinopPrecedence }
  | BE_BuildBadDeclaration
  { math_macro = \BuildBadDeclaration }
;

ast build_error
{
  "build error",
  math_macro = \TBuildError,
  short_circuit_macro = \BuildErrorConfig,
} =
  | BuildError(code: build_error_code)
  { math_macro = \BuildError }
;

ast field_or_array_access
{
  "field or array access",
  math_macro = \fieldorarrayaccess,
} =
  | FieldAccess(field_name: Identifier)
  { math_macro = \FieldAccess }
  | ArrayAccess(index: expr)
  { math_macro = \ArrayAccess }
;

typedef lhs_access
{
  "left-hand-side access",
  math_macro = \lhsaccess,
} =
  [
    access: list0(field_or_array_access)
    { math_macro = \lhsaccessaccess },
    slices: list0(slice)
    { math_macro = \lhsaccessslices }
  ]
;

typedef accessor_pair
{
  "accessor pair",
  math_macro = \accessorpair,
} =
  [
    is_readonly: Bool
    { math_macro = \accessorpairisreadonly },
    getter: stmt
    { math_macro = \accessorpairgetter },
    setter: stmt
    { math_macro = \accessorpairsetter }
  ]
;

function be_check(condition: Bool, code: build_error_code) -> CheckResult | build_error
{
  "returns $\True$ if {condition} holds and a build error with {code}. \ProseOtherwiseBuildError",
  prose_transition = "checking whether {condition} holds yields",
} =
  case be_check_true {
    condition = True;
    --
    True;
  }

  case be_check_false {
    condition = False;
    --
    BuildError(code);
  }
;

////////////////////////////////////////////////////////////////////////////////
// Primitive builders
////////////////////////////////////////////////////////////////////////////////

function build_value(parsed: N_value) -> (ast_node: literal)
{
  prose_description = "transforms a parse node {parsed} for $\Nvalue$, except for enumeration labels, into an AST node {ast_node} for $\literal$.",
  prose_transition = "building a literal from {parsed} yields",
} =
  case integer {
    parsed =: Tok_intlit(i);
    --
    L_Int(i);
  }

  case boolean {
    parsed =: Tok_boollit(b);
    --
    L_Bool(b);
  }

  case real {
    parsed =: Tok_reallit(q);
    --
    L_Real(q);
  }

  case bitvector {
    parsed =: Tok_bitvectorlit(bits);
    --
    L_Bitvector(bits);
  }

  case string {
    parsed =: Tok_stringlit(s);
    --
    L_String(s);
  }
;

function build_unop(parsed: N_unop) -> (ast_node: unop)
{
  prose_description = "transforms the syntax node {parsed} into the unary operator AST node {ast_node}.",
  prose_transition = "building a unary operator from {parsed} yields",
} =
  case boolean_not {
    parsed = Tok_bnot;
    --
    BNOT;
  }

  case negate {
    parsed = Tok_minus;
    --
    NEG;
  }

  case bitvector_not {
    parsed = Tok_not;
    --
    NOT;
  }
;

function build_binop(parsed: N_binop) -> (ast_node: binop)
{
  prose_description = "transforms the syntax node {parsed} into the binary operator AST node {ast_node}.",
  prose_transition = "building a binary operator from {parsed} yields",
} =
  case and_case {
    parsed = Tok_and;
    --
    AND;
  }

  case band {
    parsed = Tok_band;
    --
    BAND;
  }

  case bor {
    parsed = Tok_bor;
    --
    BOR;
  }

  case beq {
    parsed = Tok_beq;
    --
    BEQ;
  }

  case div {
    parsed = Tok_div;
    --
    DIV;
  }

  case divrm {
    parsed = Tok_divrm;
    --
    DIVRM;
  }

  case xor {
    parsed = Tok_xor;
    --
    XOR;
  }

  case eq {
    parsed = Tok_eqop;
    --
    EQ;
  }

  case neq {
    parsed = Tok_neq;
    --
    NE;
  }

  case gt {
    parsed = Tok_gt;
    --
    GT;
  }

  case geq {
    parsed = Tok_geq;
    --
    GE;
  }

  case impl {
    parsed = Tok_bimpl;
    --
    IMPL;
  }

  case lt {
    parsed = Tok_lt;
    --
    LT;
  }

  case leq {
    parsed = Tok_leq;
    --
    LE;
  }

  case add {
    parsed = Tok_plus;
    --
    ADD;
  }

  case sub {
    parsed = Tok_minus;
    --
    SUB;
  }

  case mod {
    parsed = Tok_mod;
    --
    MOD;
  }

  case mul {
    parsed = Tok_mul;
    --
    MUL;
  }

  case or_case {
    parsed = Tok_or;
    --
    OR;
  }

  case rdiv {
    parsed = Tok_rdiv;
    --
    RDIV;
  }

  case shl {
    parsed = Tok_shl;
    --
    SHL;
  }

  case shr {
    parsed = Tok_shr;
    --
    SHR;
  }

  case pow {
    parsed = Tok_pow;
    --
    POW;
  }

  case bitvector_concat {
    parsed = Tok_coloncolon;
    --
    BV_CONCAT;
  }

  case string_concat {
    parsed = Tok_plusplus;
    --
    STR_CONCAT;
  }
;

////////////////////////////////////////////////////////////////////////////////
// Keyword and qualifier builders
////////////////////////////////////////////////////////////////////////////////

function build_direction(parsed: N_direction) -> (ast_node: for_direction)
{
  prose_description = "transforms the syntax node {parsed} into the loop direction AST node {ast_node}.",
  prose_transition = "building a loop direction from {parsed} yields",
} =
  case to {
    parsed = Tok_to;
    --
    UP;
  }

  case downto {
    parsed = Tok_downto;
    --
    DOWN;
  }
;

function build_func_qualifier(parsed: N_qualifier) -> (ast_node: option(func_qualifier))
{
  prose_description = "transforms the syntax node {parsed} into the subprogram qualifier AST node {ast_node}.",
  prose_transition = "building a subprogram qualifier from {parsed} yields",
} =
  case none {
    parsed =: epsilon;
    --
    none;
  }

  case pure {
    parsed = Tok_pure;
    --
    some(Pure);
  }

  case readonly {
    parsed = Tok_readonly;
    --
    some(Readonly);
  }

  case noreturn {
    parsed = Tok_noreturn;
    --
    some(Noreturn);
  }
;

function build_is_readonly(parsed: N_is_readonly) -> (ast_node: Bool)
{
  prose_description = "transforms the syntax node {parsed} into the read-only flag {ast_node}.",
  prose_transition = "building the read-only flag from {parsed} yields",
} =
  case none {
    parsed =: epsilon;
    --
    False;
  }

  case readonly {
    parsed = Tok_readonly;
    --
    True;
  }
;

function build_override(parsed: N_override) -> (ast_node: option(override_info))
{
  prose_description = "transforms the syntax node {parsed} into the override information AST node {ast_node}.",
  prose_transition = "building override information from {parsed} yields",
} =
  case none {
    parsed =: epsilon;
    --
    none;
  }

  case impdef {
    parsed = Tok_impdef;
    --
    some(Impdef);
  }

  case implementation {
    parsed = Tok_implementation;
    --
    some(Implementation);
  }
;

function build_global_decl_keyword_non_config(parsed: N_global_decl_keyword_non_config) -> (ast_node: global_decl_keyword)
{
  prose_description = "transforms the syntax node {parsed} into the non-config global declaration keyword AST node {ast_node}.",
  prose_transition = "building a non-config global declaration keyword from {parsed} yields",
} =
  case let_case {
    parsed = Tok_let;
    --
    GDK_Let;
  }

  case constant_case {
    parsed = Tok_constant;
    --
    GDK_Constant;
  }

  case var {
    parsed = Tok_var;
    --
    GDK_Var;
  }
;

function build_global_decl_keyword(parsed: N_global_decl_keyword) -> (ast_node: global_decl_keyword)
{
  prose_description = "transforms the syntax node {parsed} into the global declaration keyword AST node {ast_node}.",
  prose_transition = "building a global declaration keyword from {parsed} yields",
} =
  case non_config {
    parsed =: ND_Global_Decl_Keyword_One(keyword);
    build_global_decl_keyword_non_config(keyword) -> keyword_ast;
    --
    keyword_ast;
  }

  case config {
    parsed =: ND_Global_Decl_Keyword_Two(Tok_config);
    --
    GDK_Config;
  }
;

function build_local_decl_keyword(parsed: N_local_decl_keyword) -> (ast_node: local_decl_keyword)
{
  prose_description = "transforms the syntax node {parsed} into the local declaration keyword AST node {ast_node}.",
  prose_transition = "building a local declaration keyword from {parsed} yields",
} =
  case let_case {
    parsed = Tok_let;
    --
    LDK_Let;
  }

  case constant_alias {
    parsed = Tok_constant;
    --
    LDK_Let;
  }

  case var {
    parsed = Tok_var;
    --
    LDK_Var;
  }
;

relation build_ignored_or_identifier(parsed: N_ignored_or_identifier) -> (ast_node: Identifier)
{
  prose_description = "transforms the syntax node {parsed} into the identifier {ast_node}, generating a fresh identifier for discards.",
  prose_transition = "building an identifier from {parsed} yields",
} =
  case discard {
    parsed = Tok_minus;
    id := fresh_identifier();
    --
    id;
  }

  case identifier {
    parsed =: Tok_identifier(id);
    --
    id;
  }
;

function build_identifier_token(parsed: token) -> (ast_node: Identifier)
{
  prose_description = "extracts the identifier payload from the token {parsed} into {ast_node}.",
  prose_transition = "building an identifier from the token {parsed} yields",
} =
  case identifier {
    parsed =: Tok_identifier(id);
    --
    id;
  }
;

function build_optional_ty_annotation(parsed: option((Tok_colon, N_ty))) -> (ast_node: option(ty)) | build_error
{
  prose_description = "transforms the optional type annotation {parsed} into the optional type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an optional type annotation from {parsed} yields",
} =
  case none {
    parsed = none;
    --
    none;
  }

  case some_case {
    parsed =: some((Tok_colon, ty_node));
    build_ty(ty_node) -> ty;
    --
    some(ty);
  }
;

function make_uninit_var_decl(name: Identifier, ty: ty) -> (ast_node: decl)
{
  prose_description = "constructs the uninitialized variable declaration {ast_node} for the identifier {name} and type {ty}.",
  prose_transition = "constructing an uninitialized variable declaration for {name} and {ty} yields",
} =
  case make {
    --
    D_GlobalStorage([
      keyword: GDK_Var,
      global_decl_name: name,
      global_decl_ty: some(ty),
      initial_value: none
    ]);
  }
;

////////////////////////////////////////////////////////////////////////////////
// Signature-only declarations
////////////////////////////////////////////////////////////////////////////////

relation build_decl(parsed: N_decl) -> (ast_node: list0(decl)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building declarations from {parsed} yields",
} =
  case func_decl {
    parsed =: ND_Decl_One(purity_keyword, override, Tok_func, Tok_identifier(name), params_opt, func_args, return_type, recurse_limit, func_body);
    build_func_qualifier(purity_keyword) -> qualifier;
    build_override(override) -> override_info;
    build_params_opt(params_opt) -> parameters;
    build_func_args(func_args) -> args;
    build_return_type(return_type) -> return_ty;
    build_recurse_limit(recurse_limit) -> recurse_limit_ast;
    build_func_body(func_body) -> func_body_ast;
    descriptor := [
      name: name,
      parameters: parameters,
      args: args,
      func_body: func_body_ast,
      return_type: some(return_ty),
      func_subprogram_type: ST_Function,
      recurse_limit: recurse_limit_ast,
      builtin: False,
      qualifier: qualifier,
      override: override_info
    ];
    --
    match_singleton_list(D_Func(descriptor));
  }

  case procedure_decl {
    parsed =: ND_Decl_Two(qualifier_node, override, Tok_func, Tok_identifier(name), params_opt, func_args, recurse_limit, func_body);
    build_func_qualifier(qualifier_node) -> qualifier;
    build_override(override) -> override_info;
    build_params_opt(params_opt) -> parameters;
    build_func_args(func_args) -> args;
    build_recurse_limit(recurse_limit) -> recurse_limit_ast;
    build_func_body(func_body) -> func_body_ast;
    descriptor := [
      name: name,
      parameters: parameters,
      args: args,
      func_body: func_body_ast,
      return_type: none,
      func_subprogram_type: ST_Procedure,
      recurse_limit: recurse_limit_ast,
      builtin: False,
      qualifier: qualifier,
      override: override_info
    ];
    --
    match_singleton_list(D_Func(descriptor));
  }

  case accessor {
    parsed =: ND_Decl_Three(override, Tok_accessor, Tok_identifier(name), params_opt, func_args, Tok_beq, Tok_identifier(setter_arg), as_ty, accessor_body);
    build_override(override) -> override_info;
    build_params_opt(params_opt) -> parameters;
    build_func_args(func_args) -> args;
    build_as_ty(as_ty) -> ty;
    build_accessor_body(accessor_body) -> accessors;
    desugar_accessor_pair(override_info, name, parameters, args, setter_arg, ty, accessors) -> decls;
    --
    decls;
  }

  case type_decl {
    parsed =: ND_Decl_Four(Tok_type, Tok_identifier(type_name), Tok_of, ty_decl, subtype_opt, Tok_semicolon);
    build_ty_decl(ty_decl) -> annotation;
    build_subtype_opt(subtype_opt) -> extra_fields;
    --
    match_singleton_list(D_TypeDecl(type_name, annotation, extra_fields));
  }

  case subtype_decl {
    parsed =: ND_Decl_Five(Tok_type, Tok_identifier(type_name), subtype, Tok_semicolon);
    build_subtype(subtype) -> (super_type, with_fields);
    --
    match_singleton_list(D_TypeDecl(type_name, T_Named(super_type), some((super_type, with_fields))));
  }

  case global_storage {
    parsed =: ND_Decl_Six(keyword, Tok_identifier(name), ty_opt, Tok_eq, initial_value, Tok_semicolon);
    build_global_decl_keyword_non_config(keyword) -> global_keyword;
    build_optional_ty_annotation(ty_opt) -> ty_ast_opt;
    build_expr(initial_value) -> expr;
    gd := [
      keyword: global_keyword,
      global_decl_name: name,
      global_decl_ty: ty_ast_opt,
      initial_value: some(expr)
    ];
    --
    match_singleton_list(D_GlobalStorage(gd));
  }

  case global_uninit_var {
    parsed =: ND_Decl_Eight(Tok_var, Tok_identifier(name), Tok_colon, ty_annot, Tok_semicolon);
    build_ty_or_collection(ty_annot) -> ty;
    gd := [
      keyword: GDK_Var,
      global_decl_name: name,
      global_decl_ty: some(ty),
      initial_value: none
    ];
    --
    match_singleton_list(D_GlobalStorage(gd));
  }

  case global_uninit_multiple_vars {
    parsed =: ND_Decl_Nine(Tok_var, ids, as_ty, Tok_semicolon);
    build_as_ty(as_ty) -> ty;
    INDEX(i, ids: build_identifier_token(ids[i]) -> names[i]);
    INDEX(i, names: make_uninit_var_decl(names[i], ty) -> decls[i]);
    --
    decls;
  }

  case global_storage_config {
    parsed =: ND_Decl_Seven(Tok_config, Tok_identifier(name), Tok_colon, ty_node, Tok_eq, initial_value, Tok_semicolon);
    build_ty(ty_node) -> ty;
    build_expr(initial_value) -> expr;
    gd := [
      keyword: GDK_Config,
      global_decl_name: name,
      global_decl_ty: some(ty),
      initial_value: some(expr)
    ];
    --
    match_singleton_list(D_GlobalStorage(gd));
  }

  case global_storage_elided_parameter {
    parsed =: ND_Decl_Ten(keyword, Tok_identifier(name), Tok_colon, ty_node, Tok_eq, call_node, Tok_semicolon);
    keyword =: ND_Global_Decl_Keyword_One(keyword_non_config);
    build_global_decl_keyword_non_config(keyword_non_config) -> global_keyword;
    build_ty(ty_node) -> ty;
    build_elided_param_call(call_node) -> call;
    desugar_elided_parameter(ty, call) -> expr_opt;
    gd := [
      keyword: global_keyword,
      global_decl_name: name,
      global_decl_ty: some(ty),
      initial_value: expr_opt
    ];
    --
    match_singleton_list(D_GlobalStorage(gd));
  }

  case global_storage_config_elided_parameter {
    parsed =: ND_Decl_Ten(keyword, Tok_identifier(name), Tok_colon, ty_node, Tok_eq, call_node, Tok_semicolon);
    keyword =: ND_Global_Decl_Keyword_Two(Tok_config);
    build_ty(ty_node) -> ty;
    build_elided_param_call(call_node) -> call;
    desugar_elided_parameter(ty, call) -> expr_opt;
    gd := [
      keyword: GDK_Config,
      global_decl_name: name,
      global_decl_ty: some(ty),
      initial_value: expr_opt
    ];
    --
    match_singleton_list(D_GlobalStorage(gd));
  }

  case global_pragma {
    parsed =: ND_Decl_Eleven(Tok_pragma, Tok_identifier(pragma_name), args, Tok_semicolon);
    INDEX(i, args: build_expr(args[i]) -> arg_asts[i]);
    --
    match_singleton_list(D_Pragma(pragma_name, arg_asts));
  }
;

relation build_ast(parsed: N_spec) -> (ast_node: spec)
{
  prose_description = "transforms the parse node {parsed} into the AST specification node {ast_node}.",
  prose_transition = "building an AST from {parsed} yields",
};

function build_subtype(parsed: N_subtype) -> (ast_node: (Identifier, list0(field))) | build_error
{
  prose_description = "transforms the parse node {parsed} into the subtyping information node {ast_node}.",
  prose_transition = "building subtyping information from {parsed} yields",
};

function build_subtype_opt(parsed: N_subtype_opt) -> (ast_node: option((Identifier, list0(field)))) | build_error
{
  prose_description = "transforms the parse node {parsed} into the optional subtyping information node {ast_node}.",
  prose_transition = "building an optional subtyping information node from {parsed} yields",
};

function build_fields(parsed: N_fields) -> (ast_node: list0(field)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the list of fields node {ast_node}.",
  prose_transition = "building fields from {parsed} yields",
};

function build_ty(parsed: N_ty) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a type from {parsed} yields",
};

function build_ty_decl(parsed: N_ty_decl) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type declaration AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a named type from {parsed} yields",
};

function build_ty_or_collection(parsed: N_ty_or_collection) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type or collection AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a type or collection from {parsed} yields",
};

function build_as_ty(parsed: N_as_ty) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an annotated type from {parsed} yields",
};

function build_constraint_kind_opt(parsed: N_constraint_kind_opt) -> (ast_node: constraint_kind)
{
  prose_description = "transforms the parse node {parsed} into the constraint kind AST node {ast_node}.",
  prose_transition = "building an optional constraint kind from {parsed} yields",
};

function build_constraint_kind(parsed: N_constraint_kind) -> (ast_node: constraint_kind)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a constraint kind from {parsed} yields",
};

function build_int_constraint(parsed: N_int_constraint) -> (ast_node: int_constraint)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an integer constraint from {parsed} yields",
};

relation build_accessor_body(parsed: N_accessor_body) -> (ast_node: accessor_pair)
{
  prose_description = "transforms the parse node {parsed} into the accessor pair {ast_node}.",
  prose_transition = "building an accessor body from {parsed} yields",
};

function build_recurse_limit(parsed: N_recurse_limit) -> (ast_node: option(expr))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a recursion limit from {parsed} yields",
};

function build_typed_identifier(parsed: N_typed_identifier) -> (ast_node: typed_identifier)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a typed identifier from {parsed} yields",
};

function build_opt_typed_identifier(parsed: N_opt_typed_identifier) -> (ast_node: (Identifier, option(ty)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optionally typed identifier from {parsed} yields",
};

function build_return_type(parsed: N_return_type) -> (ast_node: ty)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a return type from {parsed} yields",
};

function build_params_opt(parsed: N_params_opt) -> (ast_node: list0((Identifier, option(ty))))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building optional parameters from {parsed} yields",
};

function build_func_args(parsed: N_func_args) -> (ast_node: list0((Identifier, ty)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building function arguments from {parsed} yields",
};

function build_func_body(parsed: N_func_body) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a function body from {parsed} yields",
};

function build_accessors(parsed: N_accessors) -> (ast_node: accessor_pair)
{
  prose_description = "transforms the parse node {parsed} into the accessor pair {ast_node}.",
  prose_transition = "building accessors from {parsed} yields",
};

function build_decl_item(parsed: N_decl_item) -> (ast_node: local_decl_item)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a declaration item from {parsed} yields",
};

function build_slice(parsed: N_slice) -> (ast_node: slice)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a slice from {parsed} yields",
};

function build_slices(parsed: N_slices) -> (ast_node: list1(slice))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building slices from {parsed} yields",
};

function build_expr(parsed: N_expr) -> (ast_node: expr) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an expression from {parsed} yields",
};

function build_call(parsed: N_call) -> (ast_node: call) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a call from {parsed} yields",
};

function build_field_assign(parsed: N_field_assign) -> (ast_node: (Identifier, expr))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a field assignment from {parsed} yields",
};

function build_pattern(parsed: N_pattern) -> (ast_node: pattern)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a pattern from {parsed} yields",
};

function build_pattern_set(parsed: N_pattern_set) -> (ast_node: pattern)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a pattern set from {parsed} yields",
};

function build_pattern_list(parsed: N_pattern_list) -> (ast_node: pattern)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a pattern list from {parsed} yields",
};

function build_expr_pattern(parsed: N_expr_pattern) -> (ast_node: expr)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a pattern expression from {parsed} yields",
};

function build_lexpr(parsed: N_lexpr) -> (ast_node: lexpr)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an assignable expression from {parsed} yields",
};

function build_basic_lexpr(parsed: N_basic_lexpr) -> (ast_node: (Identifier, lhs_access))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a basic assignable expression from {parsed} yields",
};

function build_access(parsed: N_access) -> (ast_node: list0(field_or_array_access))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an access path from {parsed} yields",
};

function build_discard_or_basic_lexpr(parsed: N_discard_or_basic_lexpr) -> (ast_node: option((Identifier, lhs_access)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional basic assignable expression from {parsed} yields",
};

function build_discard_or_identifier(parsed: N_discard_or_identifier) -> (ast_node: option(Identifier))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional identifier from {parsed} yields",
};

function build_bitfields(parsed: N_bitfields) -> (ast_node: list0(bitfield))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building bitfields from {parsed} yields",
};

function build_bitfield(parsed: N_bitfield) -> (ast_node: bitfield)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a bitfield from {parsed} yields",
};

function build_catcher(parsed: N_catcher) -> (ast_node: catcher)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a catcher from {parsed} yields",
};

function build_stmt(parsed: N_stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a statement from {parsed} yields",
};

function build_elided_param_call(parsed: N_elided_param_call) -> (ast_node: call)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an elided-parameter call from {parsed} yields",
};

function build_stmt_list(parsed: N_stmt_list) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a statement list from {parsed} yields",
};

function build_s_else(parsed: N_s_else) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an else branch from {parsed} yields",
};

function build_case_alt_list(parsed: N_case_alt_list) -> (ast_node: list1(case_alt))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building case alternatives from {parsed} yields",
};

function build_case_alt(parsed: N_case_alt) -> (ast_node: case_alt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a case alternative from {parsed} yields",
};

function build_loop_limit(parsed: N_loop_limit) -> (ast_node: option(expr))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a loop limit from {parsed} yields",
};

function build_otherwise_opt(parsed: N_otherwise_opt) -> (ast_node: option(stmt))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional otherwise branch from {parsed} yields",
};

////////////////////////////////////////////////////////////////////////////////
// Helper and desugaring signatures
////////////////////////////////////////////////////////////////////////////////

function check_not_same_prec(op: binop, e: expr) -> CheckResult | build_error
{
  prose_description = "checks whether the expression AST node {e} is a binary operator, either with a different operator to {op} but the same \emph{precedence}, or the same non-associative operator {op}. In either case, it is considered an error. Surrounding {e} by parenthesis fixes the error. \ProseOtherwiseBuildError",
  prose_transition = "checking that {e} does not conflict in precedence with {op} yields",
};

function make_setter(call: call, arg: expr) -> (ast_node: call)
{
  prose_description = "constructs the setter call {ast_node} using the base call {call} and right-hand side {arg}.",
  prose_transition = "constructing a setter call from {call} and {arg} yields",
};

function desugar_setter(call: call, access: lhs_access, rhs: expr) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from an assignment of expression {rhs} to a setter invocation of {call} with accesses given by {access}.",
  prose_transition = "desugaring the setter assignment for {call} yields",
};

function desugar_setter_set_fields(call: call, fields: list0(Identifier), rhs: expr) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from an assignment of {rhs} to a setter invocation of {call} with concatenated field accesses given by {fields}.",
  prose_transition = "desugaring the setter field assignment for {call} yields",
};

function read_modify_write(call: call, name: Identifier, modify: stmt) -> (ast_node: stmt)
{
  prose_description = "builds the sequence of statements {ast_node} to read from the getter invocation of {call}, modify the resulting value using {modify}, and write it to a setter invocation of {call}",
  prose_transition = "constructing a read-modify-write sequence from {call} yields",
};

function desugar_elided_parameter(ty_annot: ty, call: call) -> (ast_node: option(expr)) | build_error
{
  prose_description = "builds the expression {ast_node} from an assignment of the call {call} to the left-hand side with type annotation {ty_annot}, where the call has an elided parameter. \ProseOtherwiseBuildError",
  prose_transition = "desugaring the elided parameter in {call} yields",
};

function stmt_from_list(stmts: list0(stmt)) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from the possibly-empty list of statements {stmts}.",
  prose_transition = "building a statement from the list {stmts} yields",
};

function sequence_stmts(s1: stmt, s2: stmt) -> (ast_node: stmt)
{
  prose_description = "combines the statement {s1} with {s2} into the statement {ast_node}, while filtering away instances of $\SPass$.",
  prose_transition = "sequencing {s1} with {s2} yields",
};

relation desugar_case_stmt(discriminant: expr, cases: list0(case_alt), otherwise_stmt: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the \casediscriminantterm{} {discriminant}, the list of \casealternativesterm{} {cases}, and the statement {otherwise_stmt} into the statement {ast_node}.",
  prose_transition = "desugaring the case statement for {discriminant} yields",
};

relation cases_to_cond(e: expr, cases: list0(case_alt), otherwise_stmt: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the expression {e}, the list of \texttt{case} alternatives {cases}, and the statement {otherwise_stmt} into the statement {ast_node}.",
  prose_transition = "converting the case alternatives {cases} yields",
};

relation case_to_cond(discriminant: expr, case_alt: case_alt, tail: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the expression {discriminant}, used as the condition for a \texttt{case} statement, the single \texttt{case} alternative {case_alt}, and the statement {tail}, which represents a list of \texttt{case} alternatives already converted to conditionals, into the condition statement {ast_node}.",
  prose_transition = "converting the case alternative {case_alt} yields",
};

function desugar_accessor_pair(
  override: option(override_info),
  name: Identifier,
  parameters: list0((Identifier, option(ty))),
  args: list0(typed_identifier),
  setter_arg: Identifier,
  ty: ty,
  accessors: accessor_pair
) -> (ast_node: list0(decl))
{
  prose_description = "transforms the optional override information {override}, name {name}, parameters {parameters}, arguments {args}, setter argument {setter_arg}, type {ty}, and accessor pair {accessors} into the AST node {ast_node}.",
  prose_transition = "desugaring the accessor pair {accessors} yields",
};

////////////////////////////////////////////////////////////////////////////////
// Top-level AST-to-AST helpers
////////////////////////////////////////////////////////////////////////////////

function rename_locals(decls: list0(decl)) -> (ast_node: list0(decl))
{
  prose_description = "renames the local storage elements appearing in {decls}, yielding the list of declarations {ast_node}.",
  prose_transition = "renaming local storage elements in {decls} yields",
};

function rename_locals_decl(decl: decl) -> (ast_node: list0(decl))
{
  prose_description = "renames the local storage elements appearing in {decl}, yielding the declaration {ast_node}.",
  prose_transition = "renaming local storage elements in {decl} yields",
};

function rename_locals_func(func_desc: func) -> (ast_node: func)
{
  prose_description = "renames the local storage elements appearing in the subprogram description {func_desc}, yielding the subprogram description {ast_node}.",
  prose_transition = "renaming local storage elements in {func_desc} yields",
};

function rename_locals_args(args: list0((Identifier, ty))) -> (ast_node: list0((Identifier, ty)))
{
  prose_description = "renames the local storage elements appearing in the list of arguments {args}, yielding the list of arguments {ast_node}.",
  prose_transition = "renaming local storage elements in the arguments {args} yields",
};

function rename_locals_named_args(params: list0((Identifier, option(ty)))) -> (ast_node: list0((Identifier, option(ty))))
{
  prose_description = "renames the local storage elements appearing in the list of parameters {params}, yielding the list of parameters {ast_node}.",
  prose_transition = "renaming local storage elements in the parameters {params} yields",
};

function rename_locals_ty(ty_desc: ty) -> (ast_node: ty)
{
  prose_description = "renames the local storage elements appearing in the type {ty_desc}, yielding the type {ast_node}.",
  prose_transition = "renaming local storage elements in the type {ty_desc} yields",
};

function rename_locals_stmt(stmt_desc: stmt) -> (ast_node: stmt)
{
  prose_description = "renames the local storage elements appearing in the statement {stmt_desc}, yielding the statement {ast_node}.",
  prose_transition = "renaming local storage elements in the statement {stmt_desc} yields",
};

function rename_locals_expr(e: expr) -> (ast_node: expr)
{
  prose_description = "renames the local storage elements appearing in the expression {e}, yielding the expression {ast_node}.",
  prose_transition = "renaming local storage elements in the expression {e} yields",
};

function rename_locals_lexpr(le: lexpr) -> (ast_node: lexpr)
{
  prose_description = "renames the local storage elements appearing in the \assignableexpression{} {le}, yielding the \assignableexpression{} {ast_node}.",
  prose_transition = "renaming local storage elements in the assignable expression {le} yields",
};

function rename_locals_ldi(item: local_decl_item) -> (ast_node: local_decl_item)
{
  prose_description = "renames the local storage elements appearing in the local declaration item {item}, yielding the local declaration item {ast_node}.",
  prose_transition = "renaming local storage elements in the local declaration item {item} yields",
};

function rename_locals_constraint(c: int_constraint) -> (ast_node: int_constraint)
{
  prose_description = "renames the local storage elements appearing in the constraint {c}, yielding the constraint {ast_node}.",
  prose_transition = "renaming local storage elements in the constraint {c} yields",
};

function rename_locals_slice(s: slice) -> (ast_node: slice)
{
  prose_description = "renames the local storage elements appearing in the slice {s}, yielding {ast_node}.",
  prose_transition = "renaming local storage elements in the slice {s} yields",
};

function rename_locals_name(name: Identifier) -> (ast_node: Identifier)
{
  prose_description = "renames the local identifier {name}, yielding the identifier {ast_node}.",
  prose_transition = "renaming the local identifier {name} yields",
};

function rename_locals_array_index(index: array_index) -> (ast_node: array_index)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the array index {index}, yielding the array index {ast_node}.",
  prose_transition = "renaming local storage elements in the array index {index} yields",
};

function rename_locals_pattern(pattern_desc: pattern) -> (ast_node: pattern)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the pattern {pattern_desc}, yielding the pattern {ast_node}.",
  prose_transition = "renaming local storage elements in the pattern {pattern_desc} yields",
};

function rename_locals_catcher(catcher_desc: catcher) -> (ast_node: catcher)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the \catcherterm{} {catcher_desc}, yielding the \catcherterm{} {ast_node}.",
  prose_transition = "renaming local storage elements in the catcher {catcher_desc} yields",
};

function set_builtin(decl: decl) -> (ast_node: decl) | build_error
{
  prose_description = "sets the $\funcbuiltin$ flag of the top-level function declaration {decl}, which is used to identify standard library functions in \TypingRuleRef{InsertStdlibParam}. \ProseOtherwiseBuildError",
  prose_transition = "marking {decl} as builtin yields",
};
