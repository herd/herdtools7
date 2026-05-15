operator clistone_to_list[T](elements: clistone[[T]]) -> list1(T)
{
  math_macro = \clistonetolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator clisttwo_to_list[T](elements: clisttwo[[T]]) -> list1(T)
{
  math_macro = \clisttwotolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator clistzero_to_list[T](elements: clistzero[[T]]) -> list0(T)
{
  math_macro = \clistzerotolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator tclistone_to_list[T](elements: tclistone[[T]]) -> list1(T)
{
  math_macro = \tclistonetolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator tclistzero_to_list[T](elements: tclistzero[[T]]) -> list0(T)
{
  math_macro = \tclistzerotolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator plisttwo_to_list[T](elements: plisttwo[[T]]) -> list1(T)
{
  math_macro = \plisttwotolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator plistzero_to_list[T](elements: plistzero[[T]]) -> list0(T)
{
  math_macro = \plistzerotolist,
  custom = true,
  prose_application = "the list of elements in {elements}",
};

operator option_to_Option[T](element: p_option[[T]]) -> option(T)
{
  math_macro = \poptiontoOption,
  custom = true,
  prose_application = "the optional element in {element}",
};

constant stdlib_local_prefix : Strings
{
  math_macro = \stdliblocalprefix,
};

////////////////////////////////////////////////////////////////////////////////
// AST builders
////////////////////////////////////////////////////////////////////////////////

typedef build_error_code
{
  "build error code",
} =
  | BE_ParseError
  {
    "the parse-error build error code",
    math_macro = \ParseError
  }
  | BE_ReservedIdentifier
  {
    "the reserved-identifier build error code",
    math_macro = \ReservedIdentifier
  }
  | BE_BinopPrecedence
  {
    "the binary-operator precedence build error code",
    math_macro = \BinopPrecedence
  }
  | BE_BuildBadDeclaration
  {
    "the bad-declaration build error code",
    math_macro = \BuildBadDeclaration
  }
;

typedef build_error
{
  "build error",
  math_macro = \TBuildError,
  short_circuit_macro = \BuildErrorConfig,
} =
  | BuildError(code: build_error_code)
  {
    "the \builderrorterm{} with code {code}",
    math_macro = \BuildError
  }
;

render build_error_and_codes = build_error(-), build_error_code(-);

ast field_or_array_access
{ "field or array access", } =
  | FieldAccess(field_name: Identifier)
  { "the field access for field name {field_name}", }
  | ArrayAccess(index: expr)
  { "the array access for index expression {index}", }
;

typedef lhs_access
{
  "left-hand-side access",
} =
  [
    access: list0(field_or_array_access),
    slices: list0(slice)
  ]
  { "the left-hand-side access descriptor with accesses given by {access} and slices given by {slices}" }
;

render lhs_access_and_field_or_array_access = lhs_access(-), field_or_array_access(-);

typedef accessor_pair
{ "accessor pair", } =
  [
    is_readonly: Bool,
    getter: stmt,
    setter: stmt
  ]
  { "the accessor descriptor with \texttt{is\_readonly} flag given by {is_readonly},
     getter statement given by {getter},
     and setter statement given by {setter}"
  }
;

function be_check(condition: Bool, code: build_error_code) -> CheckResult | build_error
{
  "returns $\True$ if {condition} holds and a build error with {code} otherwise.",
  prose_transition = "checking the condition --- {condition} --- yields",
  prose_application = "the result of checking {condition} with error code {code}",
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
  prose_application = "the literal AST node for {parsed}",
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
  prose_application = "the unary operator AST node for {parsed}",
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
  prose_application = "the binary operator AST node for {parsed}",
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
  prose_application = "the loop direction AST node for {parsed}",
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
  prose_application = "the subprogram qualifier AST node for {parsed}",
} =
  case none {
    parsed =: epsilon_sentence;
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
  prose_application = "the read-only flag for {parsed}",
} =
  case none {
    parsed =: epsilon_sentence;
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
  prose_application = "the override information AST node for {parsed}",
} =
  case none {
    parsed =: epsilon_sentence;
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
  prose_application = "the non-config global declaration keyword AST node for {parsed}",
  math_layout = [_,_],
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
  prose_application = "the global declaration keyword AST node for {parsed}",
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
  prose_application = "the local declaration keyword AST node for {parsed}",
} =
  case let_case {
    parsed = Tok_let;
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
  prose_application = "the identifier for {parsed}",
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

function build_identifier(parsed: token) -> (ast_node: Identifier)
{
  prose_description = "extracts the identifier payload from the token {parsed} into {ast_node}.",
  prose_transition = "building an identifier from the token {parsed} yields",
  prose_application = "the identifier for {parsed}",
} =
  parsed =: Tok_identifier(id);
  --
  id;
;

////////////////////////////////////////////////////////////////////////////////
// Signature-only declarations
////////////////////////////////////////////////////////////////////////////////

relation build_decl(parsed: N_decl) -> (ast_node: list0(decl)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building declarations from {parsed} yields",
  prose_application = "the declaration AST nodes for {parsed}",
} =
  case subprogram_decl {
    case func_decl {
      parsed =: ND_Decl_One(purity_keyword, override, Tok_func, Tok_identifier(name), params_opt, func_args, return_type, recurse_limit, func_body)
      { (_, [_]) };
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
      parsed =: ND_Decl_Two(qualifier_node, override, Tok_func, Tok_identifier(name), params_opt, func_args, recurse_limit, func_body)
      { (_, [_]) };
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
      parsed =: ND_Decl_Three(override, Tok_accessor, Tok_identifier(name), params_opt, func_args, Tok_beq, Tok_identifier(setter_arg), as_ty, accessor_body)
      { (_, [_]) };
      build_override(override) -> override_info;
      build_params_opt(params_opt) -> parameters;
      build_func_args(func_args) -> args;
      build_as_ty(as_ty) -> ty;
      build_accessor_body(accessor_body) -> accessors;
      desugar_accessor_pair(override_info, name, parameters, args, setter_arg, ty, accessors) -> decls
      { ([_], _)};
      --
      decls;
    }
  }

  case type_decl {
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
      match_singleton_list(D_TypeDecl(type_name, T_Named(super_type), some((super_type, with_fields))))
      { (_, [_]) };
    }
  }

  case global_storage_decl {
    case global_storage {
      parsed =: ND_Decl_Six(keyword, Tok_identifier(name), ty_opt, Tok_eq, initial_value, Tok_semicolon);
      build_global_decl_keyword_non_config(keyword) -> global_keyword;
      build_optional_as_ty(ty_opt) -> ty_ast_opt;
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
      ids_list := clisttwo_to_list(ids);
      INDEX(i, ids_list: build_identifier(ids_list[i]) -> names[i]);
      decls := list_map(i, indices(names),
        D_GlobalStorage([
          keyword: GDK_Var,
          global_decl_name: names[i],
          global_decl_ty: some(ty),
          initial_value: none
        ])
      )
      { [_] };
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
      build_global_decl_keyword_non_config(keyword_non_config) -> global_keyword
      { [_] };
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
  }

  case global_pragma {
    parsed =: ND_Decl_Eleven(Tok_pragma, Tok_identifier(pragma_name), args, Tok_semicolon);
    args_list := clistzero_to_list(args);
    INDEX(i, args_list: build_expr(args_list[i]) -> arg_asts[i]);
    --
    match_singleton_list(D_Pragma(pragma_name, arg_asts));
  }
;

render rule build_decl_GlobalStorageDecl = build_decl(global_storage_decl);
render rule build_decl_TypeDecl = build_decl(type_decl);
render rule build_decl_SubprogramDecl = build_decl(subprogram_decl);
render rule build_decl_GlobalPragma = build_decl(global_pragma);

relation build_ast(parsed: N_spec) -> (ast_node: spec)
{
  prose_description = "transforms the parse node {parsed} into the AST specification node {ast_node}.",
  prose_transition = "building an AST from {parsed} yields",
  prose_application = "the AST specification node for {parsed}",
} =
  parsed =: decls;
  INDEX(i, decls: build_decl(decls[i]) -> decl_lists[i]);
  ast_node := list_flatten(decl_lists);
  --
  ast_node;
;

function build_subtype(parsed: N_subtype) -> (ast_node: (Identifier, list0(field))) | build_error
{
  prose_description = "transforms the parse node {parsed} into the subtyping information node {ast_node}.",
  prose_transition = "building subtyping information from {parsed} yields",
  prose_application = "the subtyping information node for {parsed}",
} =
  case with_fields {
    parsed =: ND_Subtype_One(Tok_subtypes, Tok_identifier(id), Tok_with, fields);
    build_fields(fields) -> field_asts;
    --
    (id, field_asts);
  }

  case no_fields {
    parsed =: ND_Subtype_Two(Tok_subtypes, Tok_identifier(id));
    --
    (id, empty_list);
  }
;

function build_subtype_opt(parsed: N_subtype_opt) -> (ast_node: option((Identifier, list0(field)))) | build_error
{
  prose_description = "transforms the parse node {parsed} into the optional subtyping information node {ast_node}.",
  prose_transition = "building an optional subtyping information node from {parsed} yields",
  prose_application = "the optional subtyping information node for {parsed}",
} =
  case none {
    parsed = none;
    --
    none;
  }

  case some_case {
    parsed =: some(subtype);
    build_subtype(subtype) -> subtype_ast;
    --
    some(subtype_ast);
  }
;

function build_fields(parsed: N_fields) -> (ast_node: list0(field)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the list of fields node {ast_node}.",
  prose_transition = "building a list of field AST nodes from {parsed} yields",
  prose_application = "the list of field AST nodes for {parsed}",
} =
  case empty {
    parsed =: ND_Fields_One(Tok_lbrace, Tok_minus, Tok_rbrace);
    --
    empty_list;
  }

  case non_empty {
    parsed =: ND_Fields_Two(Tok_lbrace, fields, Tok_rbrace);
    fields_list := tclistone_to_list(fields);
    ast_node := list_map(field, fields_list, build_typed_identifier(field));
    --
    ast_node;
  }
;

function build_ty(parsed: N_ty) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a type from {parsed} yields",
  prose_application = "the type AST node for {parsed}",
} =
  case t_int {
    parsed =: ND_Ty_One(Tok_integer, constraint_kind_opt);
    build_constraint_kind_opt(constraint_kind_opt) -> kind;
    --
    T_Int(kind);
  }

  case t_real {
    parsed =: ND_Ty_Two(Tok_real);
    --
    T_Real;
  }

  case t_string {
    parsed =: ND_Ty_Three(Tok_string);
    --
    T_String;
  }

  case t_bool {
    parsed =: ND_Ty_Four(Tok_boolean);
    --
    T_Bool;
  }

  case t_bits {
    case bit {
      parsed =: ND_Ty_Five(Tok_bit);
      --
      T_Bits(E_Literal(L_Int(one)), empty_list);
    }

    case bits {
      parsed =: ND_Ty_Six(Tok_bits, Tok_lpar, width_expr, Tok_rpar, bitfields_opt);
      build_expr(width_expr) -> width_ast;
      bitfields_opt_ast := build_bitfields_opt(bitfields_opt);
      --
      T_Bits(width_ast, bitfields_opt_ast);
    }
  }

  case t_tuple {
    parsed =: ND_Ty_Eight(types);
    type_nodes := plisttwo_to_list(types);
    INDEX(i, type_nodes: build_ty(type_nodes[i]) -> type_asts[i]);
    --
    T_Tuple(type_asts);
  }

  case paren_type {
    parsed =: ND_Ty_Seven(Tok_lpar, ty_node, Tok_rpar);
    build_ty(ty_node) -> ast_node;
    --
    ast_node;
  }

  case t_array {
    parsed =: ND_Ty_Ten(Tok_array, Tok_llbracket, index_expr, Tok_rrbracket, Tok_of, element_ty);
    build_expr(index_expr) -> index_ast;
    build_ty(element_ty) -> element_ty_ast;
    --
    T_Array(ArrayLength_Expr(index_ast), element_ty_ast);
  }

  case t_named {
    parsed =: ND_Ty_Nine(Tok_identifier(id));
    --
    T_Named(id);
  }
;

render rule build_ty_t_int = build_ty(t_int);
render rule build_ty_t_real = build_ty(t_real);
render rule build_ty_t_string = build_ty(t_string);
render rule build_ty_t_bool = build_ty(t_bool);
render rule build_ty_t_bits = build_ty(t_bits);
render rule build_ty_t_tuple = build_ty(t_tuple);
render rule build_ty_paren_type = build_ty(paren_type);
render rule build_ty_t_array = build_ty(t_array);
render rule build_ty_t_named = build_ty(t_named);

function build_ty_decl(parsed: N_ty_decl) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type declaration AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a named type from {parsed} yields",
  prose_application = "the type declaration AST node for {parsed}",
} =
  case t_enum {
    parsed =: ND_Ty_Decl_Two(Tok_enumeration, Tok_lbrace, ids, Tok_rbrace);
    id_tokens := clistone_to_list(ids);
    id_asts := list_map(i, indices(id_tokens), build_identifier(id_tokens[i]));
    ast_node := T_Enum(match_non_empty_list(id_asts));
    --
    ast_node;
  }

  case t_record {
    parsed =: ND_Ty_Decl_Three(Tok_record, fields);
    build_fields(fields) -> field_asts;
    --
    T_Record(field_asts);
  }

  case t_exception {
    parsed =: ND_Ty_Decl_Four(Tok_exception, fields);
    build_fields(fields) -> field_asts;
    --
    T_Exception(field_asts);
  }

  case tydecl {
    parsed =: ND_Ty_Decl_One(ty);
    build_ty(ty) -> ast_node;
    --
    ast_node;
  }
;

render rule build_ty_decl_t_enum = build_ty_decl(t_enum);
render rule build_ty_decl_t_record = build_ty_decl(t_record);
render rule build_ty_decl_t_exception = build_ty_decl(t_exception);
render rule build_ty_decl_tydecl = build_ty_decl(tydecl);

function build_ty_or_collection(parsed: N_ty_or_collection) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type or collection AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a type or collection from {parsed} yields",
  prose_application = "the type or collection AST node for {parsed}",
} =
  case ty_case {
    parsed =: ND_Ty_Or_Collection_One(ty_node);
    build_ty(ty_node) -> ast_node;
    --
    ast_node;
  }

  case collection_case {
    parsed =: ND_Ty_Or_Collection_Two(Tok_collection, fields);
    build_fields(fields) -> field_asts;
    ast_node := T_Collection(field_asts);
    --
    ast_node;
  }
;

function build_as_ty(parsed: N_as_ty) -> (ast_node: ty) | build_error
{
  prose_description = "transforms the parse node {parsed} into the type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an annotated type from {parsed} yields",
  prose_application = "the annotated type AST node for {parsed}",
} =
  parsed =: (Tok_colon, ty_node);
  build_ty(ty_node) -> ast_node;
  --
  ast_node;
;

function build_optional_as_ty(parsed: p_option[[N_as_ty]]) -> (ast_node: option(ty)) | build_error
{
  prose_description = "transforms the optional type annotation {parsed} into the optional type AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an optional type annotation from {parsed} yields",
  prose_application = "the optional type AST node for {parsed}",
} =
  parsed_opt := option_to_Option(parsed);

  case none {
    parsed_opt = none;
    --
    none;
  }

  case Some {
    parsed_opt =: some(as_ty);
    build_as_ty(as_ty) -> ty;
    --
    some(ty);
  }
;

function build_constraint_kind_opt(parsed: N_constraint_kind_opt) -> (ast_node: constraint_kind)
{
  prose_description = "transforms the parse node {parsed} into the constraint kind AST node {ast_node}.",
  prose_transition = "building an optional constraint kind from {parsed} yields",
  prose_application = "the optional constraint kind AST node for {parsed}",
} =
  case constrained {
    parsed =: ND_Constraint_Kind_Opt_One(constraint_kind);
    build_constraint_kind(constraint_kind) -> ast_node;
    --
    ast_node;
  }

  case unconstrained {
    parsed =: ND_Constraint_Kind_Opt_Two(epsilon_sentence);
    --
    Unconstrained;
  }
;

function build_constraint_kind(parsed: N_constraint_kind) -> (ast_node: constraint_kind)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a constraint kind from {parsed} yields",
  prose_application = "the constraint kind AST node for {parsed}",
} =
  case well_constrained {
    parsed =: ND_Constraint_Kind_One(Tok_lbrace, constraints, Tok_rbrace);
    constraint_nodes := clistone_to_list(constraints);
    ( INDEX(i, constraint_nodes: build_int_constraint(constraint_nodes[i]) -> constraint_asts[i]) )
    { ([_]) };
    ast_node := WellConstrained(match_non_empty_list(constraint_asts));
    --
    ast_node;
  }

  case pending_constrained {
    parsed =: ND_Constraint_Kind_Two(Tok_lbrace, Tok_rbrace);
    --
    PendingConstrained;
  }
;

function build_int_constraint(parsed: N_int_constraint) -> (ast_node: int_constraint)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an integer constraint from {parsed} yields",
  prose_application = "the integer constraint AST node for {parsed}",
} =
  case exact {
    parsed =: ND_Int_Constraint_One(expr);
    build_expr(expr) -> expr_ast;
    ast_node := Constraint_Exact(expr_ast);
    --
    ast_node;
  }

  case range {
    parsed =: ND_Int_Constraint_Two(from_expr, Tok_slicing, to_expr);
    build_expr(from_expr) -> from_expr_ast;
    build_expr(to_expr) -> to_expr_ast;
    ast_node := Constraint_Range(from_expr_ast, to_expr_ast);
    --
    ast_node;
  }
;

relation build_accessor_body(parsed: N_accessor_body) -> (ast_node: accessor_pair)
{
  prose_description = "transforms the parse node {parsed} into the accessor pair {ast_node}.",
  prose_transition = "building an accessor body from {parsed} yields",
  prose_application = "the accessor pair for {parsed}",
} =
  parsed =: (Tok_begin, accessors, Tok_end, Tok_semicolon);
  build_accessors(accessors) -> ast_node;
  --
  ast_node;
;

function build_recurse_limit(parsed: N_recurse_limit) -> (ast_node: option(expr))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a recursion limit from {parsed} yields",
  prose_application = "the recursion limit AST node for {parsed}",
} =
  case limit {
    parsed =: ND_Recurse_Limit_One(Tok_recurselimit, expr);
    build_expr(expr) -> expr_ast;
    --
    some(expr_ast);
  }

  case no_limit {
    parsed =: ND_Recurse_Limit_Two(epsilon_sentence);
    --
    none;
  }
;

function build_typed_identifier(parsed: N_typed_identifier) -> (ast_node: typed_identifier)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a typed identifier from {parsed} yields",
  prose_application = "the typed identifier AST node for {parsed}",
} =
  parsed =: (Tok_identifier(id), as_ty);
  build_as_ty(as_ty) -> ty_ast;
  ast_node := (id, ty_ast);
  --
  ast_node;
;

function build_opt_typed_identifier(parsed: N_opt_typed_identifier) -> (ast_node: (Identifier, option(ty)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optionally typed identifier from {parsed} yields",
  prose_application = "the optionally typed identifier AST node for {parsed}",
} =
  parsed =: (Tok_identifier(id), as_ty_opt);
  as_ty_opt1 := option_to_Option(as_ty_opt);

  case none_case {
    as_ty_opt1 = none;
    ast_node := (id, none);
    --
    ast_node;
  }

  case some_case {
    as_ty_opt1 =: some(as_ty);
    build_as_ty(as_ty) -> ty_ast;
    ast_node := (id, some(ty_ast));
    --
    ast_node;
  }
;

function build_return_type(parsed: N_return_type) -> (ast_node: ty)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a return type from {parsed} yields",
  prose_application = "the return type AST node for {parsed}",
} =
  parsed =: (Tok_arrow, ty_node);
  build_ty(ty_node) -> ast_node;
  --
  ast_node;
;

function build_params_opt(parsed: N_params_opt) -> (ast_node: list0((Identifier, option(ty))))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building optional parameters from {parsed} yields",
  prose_application = "the optional parameter AST nodes for {parsed}",
} =
  case empty {
    parsed =: ND_Params_Opt_One(epsilon_sentence);
    --
    empty_list;
  }

  case non_empty {
    parsed =: ND_Params_Opt_Two(Tok_lbrace, ids, Tok_rbrace);
    id_nodes := clistzero_to_list(ids);
    INDEX(i, id_nodes: build_opt_typed_identifier(id_nodes[i]) -> ast_node[i]);
    --
    ast_node;
  }
;

function build_func_args(parsed: N_func_args) -> (ast_node: list0((Identifier, ty)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building function arguments from {parsed} yields",
  prose_application = "the function argument AST nodes for {parsed}",
} =
  parsed =: ND_Func_Args_One(arg_nodes);
  arg_nodes1 := plistzero_to_list(arg_nodes);
  INDEX(i, arg_nodes1: build_typed_identifier(arg_nodes1[i]) -> ast_node[i]);
  --
  ast_node;
;

function build_func_body(parsed: N_func_body) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a function body from {parsed} yields",
  prose_application = "the function body AST node for {parsed}",
} =
  parsed =: (Tok_begin, stmt_list, Tok_end, Tok_semicolon);
  build_stmt_list(stmt_list) -> ast_node;
  --
  ast_node;
;

function build_accessors(parsed: N_accessors) -> (ast_node: accessor_pair)
{
  prose_description = "transforms the parse node {parsed} into the accessor pair {ast_node}.",
  prose_transition = "building accessors from {parsed} yields",
  prose_application = "the accessor pair for {parsed}",
} =
  case getter_then_setter {
    parsed =: ND_Accessors_One(is_readonly, Tok_getter, getter_stmt_list, Tok_end, Tok_semicolon, Tok_setter, setter_stmt_list, Tok_end, Tok_semicolon)
    { (_, [_]) };
  }
  case setter_then_getter {
    parsed =: ND_Accessors_Two(Tok_setter, setter_stmt_list, Tok_end, Tok_semicolon, is_readonly, Tok_getter, getter_stmt_list, Tok_end, Tok_semicolon)
    { (_, [_]) };
  }
  build_is_readonly(is_readonly) -> is_readonly_ast;
  build_stmt_list(getter_stmt_list) -> getter_ast;
  build_stmt_list(setter_stmt_list) -> setter_ast;
  ast_node := [
    is_readonly: is_readonly_ast,
    getter: getter_ast,
    setter: setter_ast
  ];
  --
  ast_node;
;

function build_decl_item(parsed: N_decl_item) -> (ast_node: local_decl_item) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a declaration item from {parsed} yields",
  prose_application = "the declaration item AST node for {parsed}",
} =
  case var_case {
    parsed =: ND_Decl_Item_One(Tok_identifier(name));
    ast_node := LDI_Var(name);
    --
    ast_node;
  }

  case tuple_case {
    parsed =: ND_Decl_Item_Two(ids);
    id_nodes := clisttwo_to_list(ids);
    all_discards := list_forall(id_node, id_nodes, id_node = Tok_minus);
    be_check(not(all_discards), BE_BuildBadDeclaration) -> True;
    ( INDEX(i, id_nodes: build_ignored_or_identifier(id_nodes[i]) -> id_asts[i]) )
    { ([_]) };
    ast_node := LDI_Tuple(id_asts);
    --
    ast_node;
  }
;

function build_slice(parsed: N_slice) -> (ast_node: slice)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a slice from {parsed} yields",
  prose_application = "the slice AST node for {parsed}",
} =
  case single {
    parsed =: ND_Slice_One(expr);
    build_expr(expr) -> index;
    --
    Slice_Single(index);
  }

  case range {
    parsed =: ND_Slice_Two(upper_expr, Tok_colon, lower_expr);
    build_expr(upper_expr) -> upper_index;
    build_expr(lower_expr) -> lower_index;
    --
    Slice_Range(upper_index, lower_index);
  }

  case length {
    parsed =: ND_Slice_Three(start_expr, Tok_pluscolon, length_expr);
    build_expr(start_expr) -> start_index;
    build_expr(length_expr) -> length;
    --
    Slice_Length(start_index, length);
  }

  case scaled {
    parsed =: ND_Slice_Four(factor_expr, Tok_starcolon, scale_expr);
    build_expr(factor_expr) -> factor;
    build_expr(scale_expr) -> scale;
    --
    Slice_Star(factor, scale);
  }

  case width {
    parsed =: ND_Slice_Five(Tok_colon, expr);
    build_expr(expr) -> width;
    --
    Slice_Length(E_Literal(L_Int(zero)), width);
  }
;

function build_slices(parsed: N_slices) -> (ast_node: list1(slice))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building slices from {parsed} yields",
  prose_application = "the slice AST nodes for {parsed}",
} =
  parsed =: (Tok_lbracket, slices, Tok_rbracket);
  slice_nodes := clistone_to_list(slices);
  ast_node := list_map(slice_node, slice_nodes, build_slice(slice_node));
  --
  match_non_empty_list(ast_node);
;

function build_expr(parsed: N_expr) -> (ast_node: expr) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an expression from {parsed} yields",
  prose_application = "the expression AST node for {parsed}",
} =
  case e_lit {
    parsed =: ND_Expr_One(value);
    build_value(value) -> literal;
    --
    E_Literal(literal);
  }

  case e_var {
    parsed =: ND_Expr_Two(Tok_identifier(id));
    --
    E_Var(id);
  }

  case e_binop {
    parsed =: ND_Expr_Three(left_expr, binop_node, right_expr);
    build_expr(left_expr) -> left_ast;
    build_expr(right_expr) -> right_ast;
    build_binop(binop_node) -> op;
    check_not_same_prec(op, left_ast) -> True;
    check_not_same_prec(op, right_ast) -> True;
    --
    E_Binop(op, left_ast, right_ast);
  }

  case e_unop {
    parsed =: ND_Expr_Four(unop_node, expr);
    build_unop(unop_node) -> op;
    build_expr(expr) -> expr_ast;
    --
    E_Unop(op, expr_ast);
  }

  case e_cond {
    parsed =: ND_Expr_Five(Tok_if, cond_expr, Tok_then, then_expr, Tok_else, else_expr);
    build_expr(cond_expr) -> cond_ast;
    build_expr(then_expr) -> then_ast;
    build_expr(else_expr) -> else_ast;
    --
    E_Cond(cond_ast, then_ast, else_ast);
  }

  case e_call {
    parsed =: ND_Expr_Six(call_node);
    build_call(call_node) -> call_ast;
    --
    E_Call(call_ast);
  }

  case e_slice {
    parsed =: ND_Expr_Seven(base_expr, slices_node);
    build_expr(base_expr) -> base_ast;
    build_slices(slices_node) -> slices_ast;
    --
    E_Slice(base_ast, slices_ast);
  }

  case e_get_array {
    parsed =: ND_Expr_Eight(base_expr, Tok_llbracket, index_expr, Tok_rrbracket);
    build_expr(base_expr) -> base_ast;
    build_expr(index_expr) -> index_ast;
    --
    E_GetArray(base_ast, index_ast);
  }

  case e_get_field {
    parsed =: ND_Expr_Nine(base_expr, Tok_dot, Tok_identifier(id));
    build_expr(base_expr) -> base_ast;
    --
    E_GetField(base_ast, id);
  }

  case e_get_fields {
    parsed =: ND_Expr_Ten(base_expr, Tok_dot, Tok_lbracket, ids, Tok_rbracket);
    build_expr(base_expr) -> base_ast;
    id_tokens := clistone_to_list(ids);
    field_names := list_map(id_token, id_tokens, build_identifier(id_token));
    --
    E_GetFields(base_ast, field_names);
  }

  case atc {
    case type {
      parsed =: ND_Expr_Eleven(expr, Tok_as, ty);
      build_expr(expr) -> expr_ast;
      build_ty(ty) -> ty_ast;
      --
      E_ATC(expr_ast, ty_ast);
    }

    case int_constraints {
      parsed =: ND_Expr_Twelve(expr, Tok_as, constraint_kind);
      build_expr(expr) -> expr_ast;
      build_constraint_kind(constraint_kind) -> kind_ast;
      --
      E_ATC(expr_ast, T_Int(kind_ast));
    }
  }

  case e_pattern {
    case in_case {
      parsed =: ND_Expr_Thirteen(expr, Tok_in, pattern_set);
      build_expr(expr) -> expr_ast;
      build_pattern_set(pattern_set) -> pattern_ast;
      --
      E_Pattern(expr_ast, pattern_ast);
    }

    case eq_case {
      parsed =: ND_Expr_Fourteen(expr, Tok_eqop, Tok_masklit(mask));
      build_expr(expr) -> expr_ast;
      pattern_ast := Pattern_Mask(mask);
      --
      E_Pattern(expr_ast, pattern_ast);
    }

    case neq_case {
      parsed =: ND_Expr_Fifteen(expr, Tok_neq, Tok_masklit(mask));
      build_expr(expr) -> expr_ast;
      mask_pattern := Pattern_Mask(mask);
      negated_pattern := Pattern_Not(mask_pattern);
      ast_node := E_Pattern(expr_ast, negated_pattern);
      --
      ast_node;
    }
  }

  case e_arbitrary {
    parsed =: ND_Expr_Sixteen(Tok_arbitrary, Tok_colon, ty);
    build_ty(ty) -> ty_ast;
    --
    E_Arbitrary(ty_ast);
  }

  case e_record {
    case empty {
      parsed =: ND_Expr_Seventeen(Tok_identifier(record_type), Tok_lbrace, Tok_minus, Tok_rbrace);
      --
      E_Record(T_Named(record_type), empty_list);
    }

    case non_empty {
      parsed =: ND_Expr_Eighteen(Tok_identifier(record_type), Tok_lbrace, field_assigns, Tok_rbrace);
      field_assign_nodes := clistone_to_list(field_assigns);
      INDEX(i, field_assign_nodes: build_field_assign(field_assign_nodes[i]) -> field_initializer_asts[i]);
      --
      E_Record(T_Named(record_type), field_initializer_asts);
    }
  }

  case paren_expr {
    parsed =: ND_Expr_Nineteen(Tok_lpar, expr, Tok_rpar);
    build_expr(expr) -> expr_ast;
    --
    E_Tuple(make_singleton_list(expr_ast));
  }

  case e_tuple {
    parsed =: ND_Expr_Twenty(exprs);
    expr_nodes := plisttwo_to_list(exprs);
    INDEX(i, expr_nodes: build_expr(expr_nodes[i]) -> expr_asts[i]);
    --
    E_Tuple(match_non_empty_list(expr_asts));
  }
;

render rule build_expr_ELit = build_expr(e_lit);
render rule build_expr_EVar = build_expr(e_var);
render rule build_expr_EBinop = build_expr(e_binop);
render rule build_expr_EUnop = build_expr(e_unop);
render rule build_expr_ECond = build_expr(e_cond);
render rule build_expr_ECall = build_expr(e_call);
render rule build_expr_ESlice = build_expr(e_slice);
render rule build_expr_EGetArray = build_expr(e_get_array);
render rule build_expr_EGetField = build_expr(e_get_field);
render rule build_expr_EGetFields = build_expr(e_get_fields);
render rule build_expr_ATC = build_expr(atc);
render rule build_expr_EPattern = build_expr(e_pattern);
render rule build_expr_EArbitrary = build_expr(e_arbitrary);
render rule build_expr_ERecord = build_expr(e_record);
render rule build_expr_ETuple = build_expr(e_tuple);
render rule build_expr_ParenExpr = build_expr(paren_expr);

function build_call(parsed: N_call) -> (ast_node: call) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a call from {parsed} yields",
  prose_application = "the call AST node for {parsed}",
} =
  case args_only {
    parsed =: ND_Call_One(Tok_identifier(id), args);
    arg_nodes := plistzero_to_list(args);
    INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
    --
    [
      call_name: id,
      params: empty_list,
      call_args: arg_asts,
      call_type: ST_Function
    ];
  }

  case params_only {
    parsed =: ND_Call_Two(Tok_identifier(id), Tok_lbrace, params, Tok_rbrace);
    param_nodes := clistone_to_list(params);
    INDEX(i, param_nodes: build_expr(param_nodes[i]) -> param_asts[i]);
    --
    [
      call_name: id,
      params: param_asts,
      call_args: empty_list,
      call_type: ST_Function
    ];
  }

  case params_and_args {
    parsed =: ND_Call_Three(Tok_identifier(id), Tok_lbrace, params, Tok_rbrace, args);
    param_nodes := clistone_to_list(params);
    INDEX(i, param_nodes: build_expr(param_nodes[i]) -> param_asts[i]);
    arg_nodes := plistzero_to_list(args);
    INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
    --
    [
      call_name: id,
      params: param_asts,
      call_args: arg_asts,
      call_type: ST_Function
    ];
  }
;

function build_field_assign(parsed: N_field_assign) -> (ast_node: (Identifier, expr)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a field assignment from {parsed} yields",
  prose_application = "the field assignment AST node for {parsed}",
} =
  parsed =: (Tok_identifier(id), Tok_eq, expr);
  build_expr(expr) -> expr_ast;
  --
  (id, expr_ast);
;

function build_pattern(parsed: N_pattern) -> (ast_node: pattern) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a pattern from {parsed} yields",
  prose_application = "the pattern AST node for {parsed}",
} =
  case p_single {
    parsed =: ND_Pattern_One(expr_pattern);
    build_expr_pattern(expr_pattern) -> expr_ast;
    --
    Pattern_Single(expr_ast);
  }

  case p_range {
    parsed =: ND_Pattern_Two(lower_expr, Tok_slicing, upper_expr);
    build_expr_pattern(lower_expr) -> lower_ast;
    build_expr(upper_expr) -> upper_ast;
    --
    Pattern_Range(lower_ast, upper_ast);
  }

  case p_all {
    parsed =: ND_Pattern_Three(Tok_minus);
    --
    Pattern_All;
  }

  case p_leq {
    parsed =: ND_Pattern_Four(Tok_leq, expr);
    build_expr(expr) -> expr_ast;
    --
    Pattern_Leq(expr_ast);
  }

  case p_geq {
    parsed =: ND_Pattern_Five(Tok_geq, expr);
    build_expr(expr) -> expr_ast;
    --
    Pattern_Geq(expr_ast);
  }

  case p_mask {
    parsed =: ND_Pattern_Six(Tok_masklit(mask));
    --
    Pattern_Mask(mask);
  }

  case p_tuple {
    parsed =: ND_Pattern_Seven(patterns);
    pattern_nodes := plisttwo_to_list(patterns);
    INDEX(i, pattern_nodes: build_pattern(pattern_nodes[i]) -> pattern_asts[i]);
    --
    Pattern_Tuple(pattern_asts);
  }

  case p_set {
    parsed =: ND_Pattern_Eight(pattern_set);
    build_pattern_set(pattern_set) -> ast_node;
    --
    ast_node;
  }
;

render rule build_pattern_PAll = build_pattern(p_all);
render rule build_pattern_PSingle = build_pattern(p_single);
render rule build_pattern_PRange = build_pattern(p_range);
render rule build_pattern_PLeq = build_pattern(p_leq);
render rule build_pattern_PGeq = build_pattern(p_geq);
render rule build_pattern_PMask = build_pattern(p_mask);
render rule build_pattern_PTuple = build_pattern(p_tuple);
render rule build_pattern_PSet = build_pattern(p_set);

function build_pattern_set(parsed: N_pattern_set) -> (ast_node: pattern) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a pattern set from {parsed} yields",
  prose_application = "the pattern-set AST node for {parsed}",
} =
  case not_case {
    parsed =: ND_Pattern_Set_One(Tok_bnot, Tok_lbrace, pattern_list, Tok_rbrace);
    build_pattern_list(pattern_list) -> pattern_list_ast;
    --
    Pattern_Not(pattern_list_ast);
  }

  case list_case {
    parsed =: ND_Pattern_Set_Two(Tok_lbrace, pattern_list, Tok_rbrace);
    build_pattern_list(pattern_list) -> ast_node;
    --
    ast_node;
  }
;

function build_pattern_list(parsed: N_pattern_list) -> (ast_node: pattern) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a pattern list from {parsed} yields",
  prose_application = "the pattern-list AST node for {parsed}",
} =
  parsed =: ND_Pattern_List_One(patterns);
  pattern_nodes := plisttwo_to_list(patterns);
  INDEX(i, pattern_nodes: build_pattern(pattern_nodes[i]) -> pattern_asts[i]);
  --
  Pattern_Any(pattern_asts);
;

function build_expr_pattern(parsed: N_expr_pattern) -> (ast_node: expr) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a pattern expression from {parsed} yields",
  prose_application = "the pattern expression AST node for {parsed}",
} =
  case e_lit {
    parsed =: ND_Expr_Pattern_One(value);
    build_value(value) -> literal;
    --
    E_Literal(literal);
  }

  case e_var {
    parsed =: ND_Expr_Pattern_Two(Tok_identifier(id));
    --
    E_Var(id);
  }

  case e_binop {
    parsed =: ND_Expr_Pattern_Three(left_expr, binop, right_expr);
    build_expr_pattern(left_expr) -> left_ast;
    build_binop(binop) -> op;
    build_expr(right_expr) -> right_ast;
    check_not_same_prec(op, left_ast) -> True;
    check_not_same_prec(op, right_ast) -> True;
    --
    E_Binop(op, left_ast, right_ast);
  }

  case e_unop {
    parsed =: ND_Expr_Pattern_Four(unop, expr);
    build_unop(unop) -> op;
    build_expr(expr) -> expr_ast;
    --
    E_Unop(op, expr_ast);
  }

  case e_cond {
    parsed =: ND_Expr_Pattern_Five(Tok_if, cond_expr, Tok_then, then_expr, Tok_else, else_expr);
    build_expr(cond_expr) -> cond_ast;
    build_expr(then_expr) -> then_ast;
    build_expr(else_expr) -> else_ast;
    --
    E_Cond(cond_ast, then_ast, else_ast);
  }

  case e_call {
    parsed =: ND_Expr_Pattern_Six(call);
    build_call(call) -> call_ast;
    --
    E_Call(call_ast);
  }

  case e_slice {
    parsed =: ND_Expr_Pattern_Seven(base_expr, slices);
    build_expr_pattern(base_expr) -> base_ast;
    build_slices(slices) -> slices_ast;
    --
    E_Slice(base_ast, slices_ast);
  }

  case e_get_array {
    parsed =: ND_Expr_Pattern_Eight(base_expr, Tok_llbracket, index_expr, Tok_rrbracket);
    build_expr_pattern(base_expr) -> base_ast;
    build_expr(index_expr) -> index_ast;
    --
    E_GetArray(base_ast, index_ast);
  }

  case e_get_field {
    parsed =: ND_Expr_Pattern_Nine(base_expr, Tok_dot, Tok_identifier(id));
    build_expr_pattern(base_expr) -> base_ast;
    --
    E_GetField(base_ast, id);
  }

  case e_get_fields {
    parsed =: ND_Expr_Pattern_Ten(base_expr, Tok_dot, Tok_lbracket, ids, Tok_rbracket);
    build_expr_pattern(base_expr) -> base_ast;
    id_tokens := clistone_to_list(ids);
    field_names := list_map(id_token, id_tokens, build_identifier(id_token));
    --
    E_GetFields(base_ast, field_names);
  }

  case atc {
    case type_case {
      parsed =: ND_Expr_Pattern_Eleven(expr, Tok_as, ty);
      build_expr_pattern(expr) -> expr_ast;
      build_ty(ty) -> ty_ast;
      --
      E_ATC(expr_ast, ty_ast);
    }

    case int_constraints {
      parsed =: ND_Expr_Pattern_Twelve(expr, Tok_as, constraint_kind);
      build_expr_pattern(expr) -> expr_ast;
      build_constraint_kind(constraint_kind) -> kind_ast;
      --
      E_ATC(expr_ast, T_Int(kind_ast));
    }
  }

  case e_pattern {
    case in_case {
      parsed =: ND_Expr_Pattern_Thirteen(expr, Tok_in, pattern_set);
      build_expr(expr) -> expr_ast;
      build_pattern_set(pattern_set) -> pattern_ast;
      --
      E_Pattern(expr_ast, pattern_ast);
    }

    case eq_case {
      parsed =: ND_Expr_Pattern_Fourteen(expr, Tok_eqop, Tok_masklit(mask));
      build_expr(expr) -> expr_ast;
      pattern_ast := Pattern_Mask(mask);
      --
      E_Pattern(expr_ast, pattern_ast);
    }

    case neq_case {
      parsed =: ND_Expr_Pattern_Fifteen(expr, Tok_neq, Tok_masklit(mask));
      build_expr(expr) -> expr_ast;
      mask_pattern := Pattern_Mask(mask);
      negated_pattern := Pattern_Not(mask_pattern);
      ast_node := E_Pattern(expr_ast, negated_pattern);
      --
      ast_node;
    }
  }

  case e_arbitrary {
    parsed =: ND_Expr_Pattern_Sixteen(Tok_arbitrary, Tok_colon, ty);
    build_ty(ty) -> ty_ast;
    --
    E_Arbitrary(ty_ast);
  }

  case e_record {
    case empty {
      parsed =: ND_Expr_Pattern_Seventeen(Tok_identifier(record_type), Tok_lbrace, Tok_minus, Tok_rbrace);
      --
      E_Record(T_Named(record_type), empty_list);
    }

    case non_empty {
      parsed =: ND_Expr_Pattern_Eighteen(Tok_identifier(record_type), Tok_lbrace, field_assigns, Tok_rbrace);
      field_assign_nodes := clistone_to_list(field_assigns);
      ( INDEX(i, field_assign_nodes: build_field_assign(field_assign_nodes[i]) -> field_init_asts[i]) )
      { ([_]) };
      --
      E_Record(T_Named(record_type), field_init_asts)
      { [_] };
    }
  }

  case paren_expr {
    parsed =: ND_Expr_Pattern_Nineteen(Tok_lpar, expr, Tok_rpar);
    build_expr_pattern(expr) -> expr_ast;
    --
    E_Tuple(make_singleton_list(expr_ast));
  }
;

function build_lexpr(parsed: N_lexpr) -> (ast_node: lexpr) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an assignable expression from {parsed} yields",
  prose_application = "the assignable expression AST node for {parsed}",
} =
  case discard {
    parsed =: ND_Lexpr_One(Tok_minus);
    --
    LE_Discard;
  }

  case basic_lexpr {
    parsed =: ND_Lexpr_Two(basic_lexpr);
    build_basic_lexpr(basic_lexpr) -> lhs_ast;
    desugar_lhs_access(lhs_ast) -> ast_node;
    --
    ast_node;
  }

  case multi_lexpr {
    parsed =: ND_Lexpr_Three(Tok_lpar, lexprs, Tok_rpar);
    lexpr_nodes := plisttwo_to_list(lexprs);
    ( INDEX(i, lexpr_nodes: build_discard_or_basic_lexpr(lexpr_nodes[i]) -> lexpr_asts[i]) )
    { ([_]) };
    desugar_lhs_tuple(lexpr_asts) -> ast_node;
    --
    ast_node;
  }

  case concat_fields {
    parsed =: ND_Lexpr_Four(Tok_identifier(id), Tok_dot, Tok_lbracket, fields, Tok_rbracket);
    field_tokens := clistone_to_list(fields);
    field_asts := list_map(field_token, field_tokens, build_identifier(field_token));
    --
    LE_SetFields(LE_Var(id), field_asts);
  }

  case tuple_fields {
    parsed =: ND_Lexpr_Five(Tok_identifier(id), Tok_dot, Tok_lpar, ids, Tok_rpar);
    id_nodes := plisttwo_to_list(ids);
    INDEX(i, id_nodes: build_discard_or_identifier(id_nodes[i]) -> id_asts[i]);
    desugar_lhs_fields_tuple(id, id_asts) -> ast_node;
    --
    ast_node;
  }
;

function build_basic_lexpr(parsed: N_basic_lexpr) -> (ast_node: (Identifier, lhs_access))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a basic assignable expression from {parsed} yields",
  prose_application = "the basic assignable expression AST node for {parsed}",
} =
  case no_slices {
    parsed =: ND_Basic_Lexpr_One(Tok_identifier(base), access);
    build_access(access) -> access_ast;
    lhs_access_ast := [
      access: access_ast,
      slices: empty_list
    ];
    --
    (base, lhs_access_ast);
  }

  case slices {
    parsed =: ND_Basic_Lexpr_Two(Tok_identifier(base), access, slices);
    build_access(access) -> access_ast;
    build_slices(slices) -> slices_ast;
    lhs_access_ast := [
      access: access_ast,
      slices: slices_ast
    ];
    --
    (base, lhs_access_ast);
  }
;

function build_access(parsed: N_access) -> (ast_node: list0(field_or_array_access))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an access path from {parsed} yields",
  prose_application = "the access path AST node for {parsed}",
} =
  case empty {
    parsed =: ND_Access_One(epsilon_sentence);
    --
    empty_list;
  }

  case field_access {
    parsed =: ND_Access_Two(Tok_dot, Tok_identifier(field), access);
    build_access(access) -> access_ast;
    --
    concat(make_singleton_list(FieldAccess(field)), access_ast);
  }

  case array_access {
    parsed =: ND_Access_Three(Tok_llbracket, expr, Tok_rrbracket, access);
    build_expr(expr) -> expr_ast;
    build_access(access) -> access_ast;
    --
    concat(make_singleton_list(ArrayAccess(expr_ast)), access_ast);
  }
;

function build_setter_access(parsed: N_setter_access) -> (ast_node: list0(field_or_array_access))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a setter access path from {parsed} yields",
  prose_application = "the setter access path AST node for {parsed}",
} =
  case empty {
    parsed =: ND_Setter_Access_One(epsilon_sentence);
    --
    empty_list;
  }

  case field_access {
    parsed =: ND_Setter_Access_Two(Tok_dot, Tok_identifier(field), access);
    build_setter_access(access) -> access_ast;
    --
    concat(make_singleton_list(FieldAccess(field)), access_ast);
  }
;

function build_discard_or_basic_lexpr(parsed: N_discard_or_basic_lexpr) -> (ast_node: option((Identifier, lhs_access)))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional basic assignable expression from {parsed} yields",
  prose_application = "the optional basic assignable expression AST node for {parsed}",
  math_layout = [_,_],
} =
  case discard {
    parsed =: ND_Discard_Or_Basic_Lexpr_One(Tok_minus);
    --
    none;
  }

  case basic {
    parsed =: ND_Discard_Or_Basic_Lexpr_Two(basic_lexpr);
    build_basic_lexpr(basic_lexpr) -> basic_lexpr_ast;
    --
    some(basic_lexpr_ast);
  }
;

function build_discard_or_identifier(parsed: N_discard_or_identifier) -> (ast_node: option(Identifier))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional identifier from {parsed} yields",
  prose_application = "the optional identifier AST node for {parsed}",
} =
  case none_case {
    parsed = Tok_minus;
    --
    none;
  }

  case some_case {
    parsed =: Tok_identifier(id);
    --
    some(id);
  }
;

function build_bitfields_opt(parsed: N_bitfieldsopt) -> (ast_node: list0(bitfield))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building optional bitfields from {parsed} yields",
  prose_application = "the optional bitfield AST node for {parsed}",
} =
  case none {
    parsed =: ND_N_bitfieldsopt_One(epsilon_sentence);
    --
    empty_list;
  }

  case some_case {
    parsed =: ND_N_bitfieldsopt_Two(bitfields);
    build_bitfields(bitfields) -> ast_node;
    --
    ast_node;
  }
;

function build_bitfields(parsed: N_bitfields) -> (ast_node: list0(bitfield))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building bitfields from {parsed} yields",
  prose_application = "the bitfield AST nodes for {parsed}",
} =
  parsed =: (Tok_lbrace, bitfields, Tok_rbrace);
  bitfield_nodes := tclistzero_to_list(bitfields);
  ast_node := list_map(bitfield, bitfield_nodes, build_bitfield(bitfield));
  --
  ast_node;
;

function build_bitfield(parsed: N_bitfield) -> (ast_node: bitfield)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a bitfield from {parsed} yields",
  prose_application = "the bitfield AST node for {parsed}",
} =
  case simple {
    parsed =: ND_Bitfield_One(slices, Tok_identifier(name));
    build_slices(slices) -> slice_asts;
    --
    BitField_Simple(name, slice_asts);
  }

  case nested {
    parsed =: ND_Bitfield_Two(slices, Tok_identifier(name), nested_bitfields);
    build_slices(slices) -> slice_asts;
    build_bitfields(nested_bitfields) -> nested_bitfield_asts;
    --
    BitField_Nested(name, slice_asts, nested_bitfield_asts)
    { (_, [_]) };
  }

  case type_case {
    parsed =: ND_Bitfield_Three(slices, Tok_identifier(name), Tok_colon, ty);
    build_slices(slices) -> slice_asts;
    build_ty(ty) -> ty_ast;
    --
    BitField_Type(name, slice_asts, ty_ast);
  }
;

function build_catcher(parsed: N_catcher) -> (ast_node: catcher)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a catcher from {parsed} yields",
  prose_application = "the catcher AST node for {parsed}",
} =
  case named {
    parsed =: ND_Catcher_One(Tok_when, Tok_identifier(id), Tok_colon, ty, Tok_arrow, stmt_list);
    build_ty(ty) -> guard_type;
    build_stmt_list(stmt_list) -> stmt_list_ast;
    --
    (some(id), guard_type, stmt_list_ast);
  }

  case unnamed {
    parsed =: ND_Catcher_Two(Tok_when, ty, Tok_arrow, stmt_list);
    build_ty(ty) -> guard_type;
    build_stmt_list(stmt_list) -> stmt_list_ast;
    --
    (none, guard_type, stmt_list_ast);
  }
;

function build_stmt(parsed: N_stmt) -> (ast_node: stmt) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a statement from {parsed} yields",
  prose_application = "the statement AST node for {parsed}",
} =
  case s_pass {
    parsed =: ND_Stmt_Seven(Tok_pass, Tok_semicolon);
    --
    S_Pass;
  }

  case s_assign {
    parsed =: ND_Stmt_Twelve(lexpr, Tok_eq, expr, Tok_semicolon);
    build_lexpr(lexpr) -> lexpr_ast;
    build_expr(expr) -> expr_ast;
    --
    S_Assign(lexpr_ast, expr_ast);
  }

  case setter_assign {
    case no_slices {
      parsed =: ND_Stmt_Thirteen(call, setter_access, Tok_eq, expr, Tok_semicolon);
      build_call(call) -> call_ast;
      build_setter_access(setter_access) -> access_ast;
      lhs_access_ast := [
        access: access_ast,
        slices: empty_list
      ];
      build_expr(expr) -> expr_ast;
      desugar_setter(call_ast, lhs_access_ast, expr_ast) -> ast_node;
      --
      ast_node;
    }

    case slices {
      parsed =: ND_Stmt_Fourteen(call, setter_access, slices, Tok_eq, expr, Tok_semicolon);
      build_call(call) -> call_ast;
      build_setter_access(setter_access) -> access_ast;
      build_slices(slices) -> slices_ast;
      lhs_access_ast := [
        access: access_ast,
        slices: slices_ast
      ];
      build_expr(expr) -> expr_ast;
      desugar_setter(call_ast, lhs_access_ast, expr_ast) -> ast_node;
      --
      ast_node;
    }

    case setfields {
      parsed =: ND_Stmt_Fifteen(call, Tok_dot, Tok_lbracket, fields, Tok_rbracket, Tok_eq, expr, Tok_semicolon);
      build_call(call) -> call_ast;
      field_tokens := clistone_to_list(fields);
      field_asts := list_map(field_token, field_tokens, build_identifier(field_token));
      build_expr(expr) -> expr_ast;
      desugar_setter_set_fields(call_ast, field_asts, expr_ast) -> ast_node;
      --
      ast_node;
    }
  }

  case s_decl {
    case let_constant {
      parsed =: ND_Stmt_Eleven(local_decl_keyword, decl_item, ty_opt, Tok_eq, expr, Tok_semicolon);
      build_local_decl_keyword(local_decl_keyword) -> keyword_ast;
      build_decl_item(decl_item) -> item_ast;
      build_optional_as_ty(ty_opt) -> ty_ast_opt;
      build_expr(expr) -> expr_ast;
      --
      S_Decl(keyword_ast, item_ast, ty_ast_opt, some(expr_ast))
      { (_, [_]) };
    }

    case uninit_var {
      parsed =: ND_Stmt_Seventeen(Tok_var, decl_item, ty, Tok_semicolon);
      build_decl_item(decl_item) -> item_ast;
      build_as_ty(ty) -> ty_ast;
      --
      S_Decl(LDK_Var, item_ast, some(ty_ast), none);
    }

    case multi_var {
      parsed =: ND_Stmt_Eighteen(Tok_var, ids, ty, Tok_semicolon);
      id_tokens := clisttwo_to_list(ids);
      INDEX(i, id_tokens: build_identifier(id_tokens[i]) -> id_asts[i]);
      build_as_ty(ty) -> ty_ast;
      ( stmts := list_map(id, id_asts, S_Decl(LDK_Var, LDI_Var(id), some(ty_ast), none)) )
      { ([_]) };
      stmt_from_list(stmts) -> ast_node;
      --
      ast_node;
    }
  }

  case elided_param_decl {
    parsed =: ND_Stmt_Sixteen(local_decl_keyword, decl_item, ty, Tok_eq, call, Tok_semicolon);
    build_local_decl_keyword(local_decl_keyword) -> keyword_ast;
    build_decl_item(decl_item) -> item_ast;
    build_as_ty(ty) -> ty_ast;
    build_elided_param_call(call) -> call_ast;
    desugar_elided_parameter(ty_ast, call_ast) -> expr_ast_opt;
    --
    S_Decl(keyword_ast, item_ast, some(ty_ast), expr_ast_opt)
    { (_, [_] ) };
  }

  case s_stmt_list_call {
    parsed =: ND_Stmt_Nine(call, Tok_semicolon);
    build_call(call) -> call_ast;
    set_call_type(call_ast, ST_Procedure) -> call_stmt_ast;
    --
    S_Call(call_stmt_ast);
  }

  case s_cond {
    parsed =: ND_Stmt_One(Tok_if, expr, Tok_then, stmt_list, s_else, Tok_end, Tok_semicolon);
    build_expr(expr) -> expr_ast;
    build_stmt_list(stmt_list) -> then_stmt_ast;
    build_s_else(s_else) -> else_stmt_ast;
    --
    S_Cond(expr_ast, then_stmt_ast, else_stmt_ast);
  }

  case s_case {
    case no_otherwise {
      parsed =: ND_Stmt_Two(Tok_case, discriminant, Tok_of, case_alt_list, Tok_end, Tok_semicolon);
      build_case_alt_list(case_alt_list) -> case_alt_asts;
      build_expr(discriminant) -> discriminant_ast;
      desugar_case_stmt(discriminant_ast, case_alt_asts, S_Unreachable) -> ast_node
      { [_] };
      --
      ast_node;
    }

    case otherwise {
      parsed =: ND_Stmt_Three(Tok_case, discriminant, Tok_of, case_alt_list, Tok_otherwise, Tok_arrow, otherwise_stmt, Tok_end, Tok_semicolon)
      { (_, [_]) };
      build_case_alt_list(case_alt_list) -> case_alt_asts;
      build_expr(discriminant) -> discriminant_ast;
      build_stmt_list(otherwise_stmt) -> otherwise_stmt_ast;
      desugar_case_stmt(discriminant_ast, case_alt_asts, otherwise_stmt_ast) -> ast_node
      { ([_], _) };
      --
      ast_node;
    }
  }

  case s_assert {
    parsed =: ND_Stmt_Ten(Tok_assert, expr, Tok_semicolon);
    build_expr(expr) -> expr_ast;
    --
    S_Assert(expr_ast);
  }

  case s_while {
    parsed =: ND_Stmt_Four(Tok_while, condition, loop_limit, Tok_do, body, Tok_end, Tok_semicolon);
    build_expr(condition) -> condition_ast;
    build_loop_limit(loop_limit) -> loop_limit_ast;
    build_stmt_list(body) -> body_ast;
    --
    S_While(condition_ast, loop_limit_ast, body_ast);
  }

  case s_repeat {
    parsed =: ND_Stmt_TwentyTwo(Tok_repeat, body, Tok_until, condition, loop_limit, Tok_semicolon);
    build_stmt_list(body) -> body_ast;
    build_expr(condition) -> condition_ast;
    build_loop_limit(loop_limit) -> loop_limit_ast;
    --
    S_Repeat(body_ast, condition_ast, loop_limit_ast);
  }

  case s_for {
    parsed =: ND_Stmt_Five(Tok_for, Tok_identifier(index_name), Tok_eq, start_expr, direction, end_expr, loop_limit, Tok_do, body, Tok_end, Tok_semicolon)
    { (_, [_]) };
    build_expr(start_expr) -> start_ast;
    build_direction(direction) -> dir_ast;
    build_expr(end_expr) -> end_ast;
    build_loop_limit(loop_limit) -> loop_limit_ast;
    build_stmt_list(body) -> body_ast;
    --
    S_For[
      index_name: index_name,
      start_e: start_ast,
      dir: dir_ast,
      end_e: end_ast,
      body: body_ast,
      limit: loop_limit_ast
    ];
  }

  case s_throw {
    parsed =: ND_Stmt_TwentyThree(Tok_throw, expr, Tok_semicolon);
    build_expr(expr) -> expr_ast;
    --
    S_Throw(expr_ast);
  }

  case s_try {
    parsed =: ND_Stmt_Six(Tok_try, stmt_list, Tok_catch, catchers, otherwise_opt, Tok_end, Tok_semicolon);
    build_stmt_list(stmt_list) -> stmt_ast;
    catcher_nodes := match_non_empty_list(catchers);
    catchers_ast := list_map(catcher, catcher_nodes, build_catcher(catcher));
    build_otherwise_opt(otherwise_opt) -> otherwise_ast_opt;
    --
    S_Try(stmt_ast, catchers_ast, otherwise_ast_opt);
  }

  case s_return {
    parsed =: ND_Stmt_Eight(Tok_return, expr_opt, Tok_semicolon);
    expr_opt1 := option_to_Option(expr_opt);
    case none_case {
      expr_opt1 = none;
      --
      S_Return(none);
    }

    case some_case {
      expr_opt1 =: some(expr);
      build_expr(expr) -> expr_ast;
      --
      S_Return(some(expr_ast));
    }
  }

  case s_print {
    case print_case {
      parsed =: ND_Stmt_Nineteen(Tok_print, args, Tok_semicolon);
      arg_nodes := plistzero_to_list(args);
      INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
      --
      S_Print(arg_asts, False);
    }

    case println_case {
      parsed =: ND_Stmt_Twenty(Tok_println, args, Tok_semicolon);
      arg_nodes := plistzero_to_list(args);
      INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
      --
      S_Print(arg_asts, True);
    }
  }

  case s_unreachable {
    parsed =: ND_Stmt_TwentyOne(Tok_unreachable, Tok_semicolon);
    --
    S_Unreachable;
  }

  case s_pragma {
    parsed =: ND_Stmt_TwentyFour(Tok_pragma, Tok_identifier(id), args, Tok_semicolon);
    INDEX(i, args: build_expr(args[i]) -> arg_asts[i]);
    --
    S_Pragma(id, arg_asts);
  }
;

render rule build_stmt_SPass = build_stmt(s_pass);
render rule build_stmt_SAssign = build_stmt(s_assign);
render rule build_stmt_SetterAssign = build_stmt(setter_assign);
render rule build_stmt_SDecl = build_stmt(s_decl);
render rule build_stmt_ElidedParamDecl = build_stmt(elided_param_decl);
render rule build_stmt_SCall = build_stmt(s_stmt_list_call);
render rule build_stmt_SCond = build_stmt(s_cond);
render rule build_stmt_SCase = build_stmt(s_case);
render rule build_stmt_SAssert = build_stmt(s_assert);
render rule build_stmt_SWhile = build_stmt(s_while);
render rule build_stmt_SRepeat = build_stmt(s_repeat);
render rule build_stmt_SFor = build_stmt(s_for);
render rule build_stmt_SThrow = build_stmt(s_throw);
render rule build_stmt_STry = build_stmt(s_try);
render rule build_stmt_SReturn = build_stmt(s_return);
render rule build_stmt_SPrint = build_stmt(s_print);
render rule build_stmt_SUnreachable = build_stmt(s_unreachable);
render rule build_stmt_SPragma = build_stmt(s_pragma);

function build_elided_param_call(parsed: N_elided_param_call) -> (ast_node: call) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building an elided-parameter call from {parsed} yields",
  prose_application = "the elided-parameter call AST node for {parsed}",
} =
  case no_params_no_args {
    parsed =: ND_Elided_Param_Call_One(Tok_identifier(id), Tok_lbrace, Tok_rbrace);
    --
    [
      call_name: id,
      params: empty_list,
      call_args: empty_list,
      call_type: ST_Function
    ];
  }

  case no_params_args {
    parsed =: ND_Elided_Param_Call_Two(Tok_identifier(id), Tok_lbrace, Tok_rbrace, args);
    arg_nodes := plistzero_to_list(args);
    INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
    --
    [
      call_name: id,
      params: empty_list,
      call_args: arg_asts,
      call_type: ST_Function
    ];
  }

  case params_no_args {
    parsed =: ND_Elided_Param_Call_Three(Tok_identifier(id), Tok_lbrace, Tok_comma, params, Tok_rbrace);
    param_nodes := tclistone_to_list(params);
    INDEX(i, param_nodes: build_expr(param_nodes[i]) -> param_asts[i]);
    --
    [
      call_name: id,
      params: param_asts,
      call_args: empty_list,
      call_type: ST_Function
    ];
  }

  case params_args {
    parsed =: ND_Elided_Param_Call_Four(Tok_identifier(id), Tok_lbrace, Tok_comma, params, Tok_rbrace, args);
    param_nodes := tclistone_to_list(params);
    INDEX(i, param_nodes: build_expr(param_nodes[i]) -> param_asts[i]);
    arg_nodes := plistzero_to_list(args);
    INDEX(i, arg_nodes: build_expr(arg_nodes[i]) -> arg_asts[i]);
    --
    [
      call_name: id,
      params: param_asts,
      call_args: arg_asts,
      call_type: ST_Function
    ];
  }
;

function build_stmt_list(parsed: N_stmt_list) -> (ast_node: stmt) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a statement list from {parsed} yields",
  prose_application = "the statement AST node for {parsed}",
} =
  parsed =: ND_Stmt_List_One(stmts);
  INDEX(i, stmts: build_stmt(stmts[i]) -> stmt_asts[i]);
  stmt_from_list(stmt_asts) -> ast_node;
  --
  ast_node;
;

function build_s_else(parsed: N_s_else) -> (ast_node: stmt)
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an else branch from {parsed} yields",
  prose_application = "the else-branch AST node for {parsed}",
} =
  case elseif_case {
    parsed =: ND_S_Else_One(Tok_elseif, expr, Tok_then, stmt_list, s_else);
    build_expr(expr) -> expr_ast;
    build_stmt_list(stmt_list) -> stmt_list_ast;
    build_s_else(s_else) -> s_else_ast;
    --
    S_Cond(expr_ast, stmt_list_ast, s_else_ast);
  }

  case pass_case {
    parsed =: ND_S_Else_Three(epsilon_sentence);
    --
    S_Pass;
  }

  case else_case {
    parsed =: ND_S_Else_Two(Tok_else, stmt_list);
    build_stmt_list(stmt_list) -> ast_node;
    --
    ast_node;
  }
;

function build_case_alt_list(parsed: N_case_alt_list) -> (ast_node: list1(case_alt)) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building case alternatives from {parsed} yields",
  prose_application = "the case alternative AST nodes for {parsed}",
} =
  parsed =: ND_Case_Alt_List_One(cases);
  INDEX(i, cases: build_case_alt(cases[i]) -> case_asts[i]);
  --
  match_non_empty_list(case_asts);
;

function build_case_alt(parsed: N_case_alt) -> (ast_node: case_alt) | build_error
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "building a case alternative from {parsed} yields",
  prose_application = "the case alternative AST node for {parsed}",
} =
  parsed =: (Tok_when, pattern_list, where, Tok_arrow, stmts);
  build_pattern_list(pattern_list) -> pattern_ast;
  build_stmt_list(stmts) -> stmt_ast;
  where1 := option_to_Option(where);

  case none_case {
    where1 = none;
    --
    [
      case_alt_pattern: pattern_ast,
      where: none,
      case_alt_stmt: stmt_ast
    ];
  }

  case some_case {
    where1 =: some((Tok_where, expr));
    build_expr(expr) -> where_ast;
    --
    [
      case_alt_pattern: pattern_ast,
      where: some(where_ast),
      case_alt_stmt: stmt_ast
    ];
  }
;

function build_loop_limit(parsed: N_loop_limit) -> (ast_node: option(expr))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building a loop limit from {parsed} yields",
  prose_application = "the loop limit AST node for {parsed}",
} =
  case limit {
    parsed =: ND_Loop_Limit_One(Tok_looplimit, expr);
    build_expr(expr) -> expr_ast;
    --
    some(expr_ast);
  }

  case no_limit {
    parsed =: ND_Loop_Limit_Two(epsilon_sentence);
    --
    none;
  }
;

function build_otherwise_opt(parsed: N_otherwise_opt) -> (ast_node: option(stmt))
{
  prose_description = "transforms the parse node {parsed} into the AST node {ast_node}.",
  prose_transition = "building an optional otherwise branch from {parsed} yields",
  prose_application = "the optional otherwise-branch AST node for {parsed}",
} =
  case non_empty {
    parsed =: ND_Otherwise_Opt_One(Tok_otherwise, Tok_arrow, stmts);
    build_stmt_list(stmts) -> stmts_ast;
    --
    some(stmts_ast);
  }

  case empty {
    parsed =: ND_Otherwise_Opt_Two(epsilon_sentence);
    --
    none;
  }
;

////////////////////////////////////////////////////////////////////////////////
// Helper and desugaring signatures
////////////////////////////////////////////////////////////////////////////////

function desugar_lhs_access(lhs_access: (Identifier, lhs_access)) -> (ast_node: lexpr)
{
  prose_description = "transforms the left-hand-side access {lhs_access} into the AST node {ast_node}.",
  prose_transition = "desugaring the left-hand-side access {lhs_access} yields",
  prose_application = "the AST node obtained by desugaring the left-hand-side access {lhs_access}",
} =
  case empty {
    lhs_access =: (name, [access: empty_list, slices: slices]);
    base := LE_Var(name);
    ast_node := if_then_else(slices = empty_list, base, LE_Slice(base, slices));
    --
    ast_node;
  }

  case field_access {
    lhs_access =: (name, [access: access_path, slices: slices]);
    access_path =: concat(access_prefix, match_singleton_list(FieldAccess(field)));
    prefix := [access: access_prefix, slices: empty_list];
    desugar_lhs_access((name, prefix)) -> base;
    updated := LE_SetField(base, field);
    ast_node := if_then_else(slices = empty_list, updated, LE_Slice(updated, slices));
    --
    ast_node;
  }

  case array_access {
    lhs_access =: (name, [access: access_path, slices: slices]);
    access_path =: concat(access_prefix, match_singleton_list(ArrayAccess(index)));
    prefix := [access: access_prefix, slices: empty_list];
    desugar_lhs_access((name, prefix)) -> base;
    updated := LE_SetArray(base, index);
    ast_node := if_then_else(slices = empty_list, updated, LE_Slice(updated, slices));
    --
    ast_node;
  }
;

function desugar_lhs_tuple(lhs_access_opts: list0(option((Identifier, lhs_access)))) -> (ast_node: lexpr)
{
  prose_description = "transforms the list of optional left-hand-side accesses {lhs_access_opts} into the AST node {ast_node}.",
  prose_transition = "desugaring the left-hand-side tuple {lhs_access_opts} yields",
  prose_application = "the AST node obtained by desugaring the left-hand-side tuple {lhs_access_opts}",
} =
  ( INDEX(i, lhs_access_opts: desugar_lhs_access_opt(lhs_access_opts[i]) -> lexprs[i]) )
  { ([_]) };
  ast_node := LE_Destructuring(lexprs);
  --
  ast_node;
;

function desugar_lhs_access_opt(lhs_access_opt: option((Identifier, lhs_access))) -> (ast_node: lexpr)
{
  prose_description = "transforms the optional left-hand-side access {lhs_access_opt} into the AST node {ast_node}.",
  prose_transition = "desugaring the optional left-hand-side access {lhs_access_opt} yields",
  prose_application = "the AST node obtained by desugaring the optional left-hand-side access {lhs_access_opt}",
} =
  case none_case {
    lhs_access_opt = none;
    --
    LE_Discard;
  }

  case some_case {
    lhs_access_opt =: some(lhs_access);
    desugar_lhs_access(lhs_access) -> ast_node;
    --
    ast_node;
  }
;

function desugar_lhs_fields_tuple(id: Identifier, field_opts: list0(option(Identifier))) -> (ast_node: lexpr) | build_error
{
  prose_description = "transforms an assignment to a tuple of fields {field_opts} of variable {id} into the AST node {ast_node}. \ProseOtherwiseBuildError",
  prose_transition = "desugaring the left-hand-side fields tuple for {id} yields",
  prose_application = "the AST node obtained by desugaring the left-hand-side fields tuple for {id} and {field_opts}",
} =
  fields := filter_option_list(field_opts);
  be_check(no_duplicates(fields), BE_ParseError) -> True;
  ( INDEX(i, field_opts: desugar_lhs_field_opt(id, field_opts[i]) -> lexprs[i]) )
  { ([_]) };
  ast_node := LE_Destructuring(lexprs);
  --
  ast_node;
;

function desugar_lhs_field_opt(id: Identifier, field_opt: option(Identifier)) -> (ast_node: lexpr)
{
  prose_description = "transforms the optional field {field_opt} of variable {id} into the AST node {ast_node}.",
  prose_transition = "desugaring the optional field {field_opt} for {id} yields",
  prose_application = "the AST node obtained by desugaring the optional field {field_opt} for {id}",
} =
  case none_case {
    field_opt = none;
    --
    LE_Discard;
  }

  case some_case {
    field_opt =: some(field);
    base := LE_Var(id);
    ast_node := LE_SetField(base, field);
    --
    ast_node;
  }
;

function binop_prec(op: binop) -> (level: N)
{
  prose_description = "assigns the precedence level {level} to the binary operator {op}.",
  prose_transition = "computing the precedence of {op} yields",
  prose_application = "the precedence level of {op}",
} =
  case precedence_5 {
    op = POW;
    --
    five;
  }

  case precedence_4 {
    op in make_set(MUL, DIV, DIVRM, RDIV, MOD, SHL, SHR);
    --
    four;
  }

  case precedence_3 {
    op in make_set(ADD, SUB, OR, XOR, AND, BV_CONCAT, STR_CONCAT);
    --
    three;
  }

  case precedence_2 {
    op in make_set(GT, GE, LT, LE);
    --
    two;
  }

  case precedence_1 {
    op in make_set(EQ, NE);
    --
    one;
  }

  case precedence_0 {
    op in make_set(BOR, BAND, IMPL, BEQ);
    --
    zero;
  }
;

function check_not_same_prec(op: binop, e: expr) -> CheckResult | build_error
{
  prose_description = "checks whether the expression AST node {e} is a binary operator, either with a different operator to {op} but the same \emph{precedence}, or the same non-associative operator {op}. In either case, it is considered an error. Surrounding {e} by parenthesis fixes the error. \ProseOtherwiseBuildError",
  prose_transition = "checking that {e} does not conflict in precedence with {op} yields",
  prose_application = "the result of checking {e} against {op} for precedence conflicts",
} =
  case not_binop {
    ast_label(e) != label_E_Binop;
    --
    True;
  }

  case binop_same {
    e =: E_Binop(op', _, _);
    op = op';
    associative_operators := make_set(ADD, MUL, BAND, BOR, AND, OR, XOR, BEQ, BV_CONCAT, STR_CONCAT)
    { (_, [_]) };
    be_check(op in associative_operators, BE_BinopPrecedence) -> True;
    --
    True;
  }

  case binop_different {
    e =: E_Binop(op', _, _);
    op != op';
    binop_prec(op) -> op_prec;
    binop_prec(op') -> op'_prec;
    be_check(op_prec != op'_prec, BE_BinopPrecedence) -> True;
    --
    True;
  }
;

function set_call_type(call: call, call_type: subprogram_type) -> (ast_node: call)
{
  prose_description = "changes the call type of {call} to {call_type}, yielding the call descriptor {ast_node}.",
  prose_transition = "changing the call type of {call} to {call_type} yields",
  prose_application = "the call descriptor obtained by changing the call type of {call} to {call_type}",
} =
  ast_node := call(call_type: call_type);
  --
  ast_node;
;

function make_setter(call: call, arg: expr) -> (ast_node: call)
{
  prose_description = "constructs the setter call {ast_node} using the base call {call} and right-hand side {arg}.",
  prose_transition = "constructing a setter call from {call} and {arg} yields",
  prose_application = "the setter call constructed from {call} and {arg}",
} =
  call =: [
    call_name: call_name,
    params: params,
    call_args: args,
    call_type: _
  ];
  setter_arg := make_singleton_list(arg);
  call_args := concat(setter_arg, args);
  call_type := ST_Setter;
  ast_node := [
    call_name: call_name,
    params: params,
    call_args: call_args,
    call_type: call_type
  ];
  --
  ast_node;
;

function desugar_setter(call: call, access: lhs_access, rhs: expr) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from an assignment of expression {rhs} to a setter invocation of {call} with accesses given by {access}.",
  prose_transition = "desugaring the setter assignment for {call} yields",
  prose_application = "the statement obtained by desugaring the setter assignment for {call}, {access}, and {rhs}",
} =
  case empty {
    access =: [access: empty_list, slices: empty_list];
    make_setter(call, rhs) -> setter_call;
    --
    S_Call(setter_call);
  }

  case non_empty {
    access =: [access: access_list, slices: slices];
    binary_or(access_list != empty_list, slices != empty_list);
    name := fresh_identifier();
    desugar_lhs_access((name, access)) -> lhs;
    modify := S_Assign(lhs, rhs);
    read_modify_write(call, name, modify) -> ast_node;
    --
    ast_node;
  }
;

function desugar_setter_set_fields(call: call, fields: list0(Identifier), rhs: expr) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from an assignment of {rhs} to a setter invocation of {call} with concatenated field accesses given by {fields}.",
  prose_transition = "desugaring the setter field assignment for {call} yields",
  prose_application = "the statement obtained by desugaring the setter field assignment for {call}, {fields}, and {rhs}",
} =
  name := fresh_identifier();
  lhs := LE_SetFields(LE_Var(name), fields);
  modify := S_Assign(lhs, rhs);
  read_modify_write(call, name, modify) -> ast_node;
  --
  ast_node;
;

function read_modify_write(call: call, name: Identifier, modify: stmt) -> (ast_node: stmt)
{
  prose_description = "builds the sequence of statements {ast_node} to read from the getter invocation of {call}, modify the resulting value using {modify}, and write it to a setter invocation of {call}",
  prose_transition = "constructing a read-modify-write sequence from {call} yields",
  prose_application = "the read-modify-write statement sequence for {call}, {name}, and {modify}",
} =
  set_call_type(call, ST_Getter) -> getter;
  getter_call := E_Call(getter);
  initial_value := some(getter_call);
  lhs_decl_item := LDI_Var(name);
  read := S_Decl(LDK_Var, lhs_decl_item, none, initial_value);
  setter_arg := E_Var(name);
  make_setter(call, setter_arg) -> setter;
  setter_stmt := S_Call(setter);
  prefix := S_Seq(read, modify);
  ast_node := S_Seq(prefix, setter_stmt);
  --
  ast_node;
;

function desugar_elided_parameter(ty_annot: ty, call: call) -> (ast_node: option(expr)) | build_error
{
  prose_description = "builds the expression {ast_node} from an assignment of the call {call} to the left-hand side with type annotation {ty_annot}, where the call has an elided parameter. \ProseOtherwiseBuildError",
  prose_transition = "desugaring the elided parameter in {call} yields",
  prose_application = "the optional expression obtained by desugaring the elided parameter in {call} with type annotation {ty_annot}",
} =
  be_check(ast_label(ty_annot) = label_T_Bits, BE_ParseError) -> True;
  ty_annot =: T_Bits(width, _);
  params := call.params;
  new_params := cons(width, params);
  callp := call(params: new_params);
  ast_node := some(E_Call(callp));
  --
  ast_node;
;

function stmt_from_list(stmts: list0(stmt)) -> (ast_node: stmt)
{
  prose_description = "builds the statement {ast_node} from the possibly-empty list of statements {stmts}.",
  prose_transition = "building a statement from the list {stmts} yields",
  prose_application = "the statement built from {stmts}",
} =
  case empty {
    stmts = empty_list;
    --
    S_Pass;
  }

  case non_empty {
    stmts =: match_cons(stmt, stmts1);
    stmt_from_list(stmts1) -> stmt1;
    sequence_stmts(stmt, stmt1) -> ast_node;
    --
    ast_node;
  }
;

function sequence_stmts(s1: stmt, s2: stmt) -> (ast_node: stmt)
{
  prose_description = "combines the statement {s1} with {s2} into the statement {ast_node}, while filtering away instances of $\SPass$.",
  prose_transition = "sequencing {s1} with {s2} yields",
  prose_application = "the statement sequence formed from {s1} and {s2}",
} =
  case s1_spass {
    s1 = S_Pass;
    --
    s2;
  }

  case s2_spass {
    s1 != S_Pass;
    s2 = S_Pass;
    --
    s1;
  }

  case no_spass {
    s1 != S_Pass;
    s2 != S_Pass;
    --
    S_Seq(s1, s2);
  }
;

relation desugar_case_stmt(discriminant: expr, cases: list0(case_alt), otherwise_stmt: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the \casediscriminantterm{} {discriminant}, the list of \casealternativesterm{} {cases}, and the statement {otherwise_stmt} into the statement {ast_node}.",
  prose_transition = "desugaring the case statement for {discriminant} yields",
  prose_application = "the statement obtained by desugaring the case statement for {discriminant}, {cases}, and {otherwise_stmt}",
} =
  case var_case {
    ast_label(discriminant) = label_E_Var;
    cases_to_cond(discriminant, cases, otherwise_stmt) -> ast_node;
    --
    ast_node;
  }

  case non_var_case {
    ast_label(discriminant) != label_E_Var;
    name := fresh_identifier();
    decl := S_Decl(LDK_Let, LDI_Var(name), none, some(discriminant));
    var_expr := E_Var(name);
    cases_to_cond(var_expr, cases, otherwise_stmt) -> cond_stmt;
    ast_node := S_Seq(decl, cond_stmt);
    --
    ast_node;
  }
;

relation cases_to_cond(e: expr, cases: list0(case_alt), otherwise_stmt: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the expression {e}, the list of \texttt{case} alternatives {cases}, and the statement {otherwise_stmt} into the statement {ast_node}.",
  prose_transition = "converting the case alternatives {cases} yields",
  prose_application = "the conditional statement obtained from {e}, {cases}, and {otherwise_stmt}",
} =
  case empty {
    cases = empty_list;
    --
    otherwise_stmt;
  }

  case last {
    cases =: match_singleton_list(case_alt);
    case_to_cond(e, case_alt, otherwise_stmt) -> ast_node;
    --
    ast_node;
  }

  case not_last {
    cases =: match_cons(case_alt, cases1);
    cases1 != empty_list;
    cases_to_cond(e, cases1, otherwise_stmt) -> tail;
    case_to_cond(e, case_alt, tail) -> ast_node;
    --
    ast_node;
  }
;

relation case_to_cond(discriminant: expr, case_alt: case_alt, tail: stmt) -> (ast_node: stmt)
{
  prose_description = "transforms the expression {discriminant}, used as the condition for a \texttt{case} statement, the single \texttt{case} alternative {case_alt}, and the statement {tail}, which represents a list of \texttt{case} alternatives already converted to conditionals, into the condition statement {ast_node}.",
  prose_transition = "converting the case alternative {case_alt} yields",
  prose_application = "the conditional statement obtained from {discriminant}, {case_alt}, and {tail}",
} =
  case_alt =: [
    case_alt_pattern: pattern,
    where: where_opt,
    case_alt_stmt: stmt
  ];
  pattern_expr := E_Pattern(discriminant, pattern);
  condition_expr := if_then_else(
    where_opt =: some(where_expr),
    E_Binop(BAND, pattern_expr, where_expr),
    pattern_expr
  )
  { (_, [_]) };
  ast_node := S_Cond(condition_expr, stmt, tail);
  --
  ast_node;
;

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
  prose_application = "the declarations obtained by desugaring the accessor pair {accessors}",
  math_layout = ([_,_,_,_,_,_,_],_),
} =
  accessors =: [
    is_readonly: is_readonly,
    getter: getter_body,
    setter: setter_body
  ];
  qualifier := if_then_else(is_readonly, some(Readonly), none);
  getter_func := [
    name: name,
    parameters: parameters,
    args: args,
    func_body: getter_body,
    return_type: some(ty),
    func_subprogram_type: ST_Getter,
    recurse_limit: none,
    builtin: False,
    qualifier: qualifier,
    override: override
  ];
  setter_args := cons((setter_arg, ty), args);
  setter_func := [
    name: name,
    parameters: parameters,
    args: setter_args,
    func_body: setter_body,
    return_type: none,
    func_subprogram_type: ST_Setter,
    recurse_limit: none,
    builtin: False,
    qualifier: none,
    override: override
  ];
  ast_node := make_list(D_Func(getter_func), D_Func(setter_func));
  --
  ast_node
  { ([_],_) };
;

////////////////////////////////////////////////////////////////////////////////
// Top-level AST-to-AST helpers
////////////////////////////////////////////////////////////////////////////////

function rename_locals(decls: list0(decl)) -> (ast_node: list0(decl))
{
  prose_description = "renames the local storage elements appearing in {decls}, yielding the list of declarations {ast_node}.",
  prose_transition = "renaming local storage elements in {decls} yields",
  prose_application = "the result of renaming local storage elements in {decls}",
} =
  INDEX(i, decls: rename_locals_decl(decls[i]) -> ast_node[i]);
  --
  ast_node;
;

function rename_locals_decl(decl: decl) -> (ast_node: decl)
{
  prose_description = "renames the local storage elements appearing in {decl}, yielding the declaration {ast_node}.",
  prose_transition = "renaming local storage elements in {decl} yields",
  prose_application = "the result of renaming local storage elements in {decl}",
} =
  case subprogram {
    decl =: D_Func(func_desc);
    rename_locals_func(func_desc) -> new_func_desc;
    ast_node := D_Func(new_func_desc);
    --
    ast_node;
  }

  case other {
    ast_label(decl) != label_D_Func;
    --
    decl;
  }
;

function rename_locals_func(func_desc: func) -> (ast_node: func)
{
  prose_description = "renames the local storage elements appearing in the subprogram description {func_desc}, yielding the subprogram description {ast_node}.",
  prose_transition = "renaming local storage elements in {func_desc} yields",
  prose_application = "the result of renaming local storage elements in {func_desc}",
} =
  func_desc =: [
    name: name,
    parameters: parameters,
    args: args,
    func_body: func_body,
    return_type: return_type,
    func_subprogram_type: func_subprogram_type,
    recurse_limit: recurse_limit,
    builtin: builtin,
    qualifier: qualifier,
    override: override
  ];
  rename_locals_args(args) -> new_args;
  rename_locals_named_args(parameters) -> new_parameters;
  rename_locals_stmt(func_body) -> new_func_body;

  case no_return_type {
    return_type = none;
    ast_node := [
      name: name,
      parameters: new_parameters,
      args: new_args,
      func_body: new_func_body,
      return_type: none,
      func_subprogram_type: func_subprogram_type,
      recurse_limit: recurse_limit,
      builtin: builtin,
      qualifier: qualifier,
      override: override
    ];
    --
    ast_node;
  }

  case some_return_type {
    return_type =: some(ty);
    rename_locals_ty(ty) -> new_ty;
    ast_node := [
      name: name,
      parameters: new_parameters,
      args: new_args,
      func_body: new_func_body,
      return_type: some(new_ty),
      func_subprogram_type: func_subprogram_type,
      recurse_limit: recurse_limit,
      builtin: builtin,
      qualifier: qualifier,
      override: override
    ];
    --
    ast_node;
  }
;

function rename_locals_args(args: list0((Identifier, ty))) -> (ast_node: list0((Identifier, ty)))
{
  prose_description = "renames the local storage elements appearing in the list of arguments {args}, yielding the list of arguments {ast_node}.",
  prose_transition = "renaming local storage elements in the arguments {args} yields",
  prose_application = "the result of renaming local storage elements in {args}",
} =
  case empty {
    args = empty_list;
    --
    empty_list;
  }

  case non_empty {
    args =: match_cons((name, ty), args1);
    rename_locals_name(name) -> name1;
    rename_locals_ty(ty) -> ty1;
    rename_locals_args(args1) -> args1';
    ast_node := cons((name1, ty1), args1');
    --
    ast_node;
  }
;

function rename_locals_named_args(params: list0((Identifier, option(ty)))) -> (ast_node: list0((Identifier, option(ty))))
{
  prose_description = "renames the local storage elements appearing in the list of parameters {params}, yielding the list of parameters {ast_node}.",
  prose_transition = "renaming local storage elements in the parameters {params} yields",
  prose_application = "the result of renaming local storage elements in {params}",
} =
  case empty {
    params = empty_list;
    --
    empty_list;
  }

  case non_empty {
    params =: match_cons((name, ty_opt), params1);
    rename_locals_name(name) -> name1;
    rename_locals_named_args(params1) -> params1';

    case unannotated {
      ty_opt = none;
      ast_node := cons((name1, none), params1');
      --
      ast_node;
    }

    case annotated {
      ty_opt =: some(ty);
      rename_locals_ty(ty) -> ty1;
      ast_node := cons((name1, some(ty1)), params1');
      --
      ast_node;
    }
  }
;

function rename_locals_ty(ty_desc: ty) -> (ast_node: ty)
{
  prose_description = "renames the local storage elements appearing in the type {ty_desc}, yielding the type {ast_node}.",
  prose_transition = "renaming local storage elements in the type {ty_desc} yields",
  prose_application = "the result of renaming local storage elements in {ty_desc}",
} =
  case unchanged {
    ast_label(ty_desc) in make_set(label_T_Real, label_T_String, label_T_Bool, label_T_Enum, label_T_Named);
    --
    ty_desc;
  }

  case t_int_unconstrained {
    ty_desc =: T_Int(c);
    ast_label(c) in make_set(label_Unconstrained, label_PendingConstrained);
    --
    ty_desc;
  }

  case t_int_parameterized {
    ty_desc =: T_Int(Parameterized(param_name));
    rename_locals_name(param_name) -> param_name';
    ast_node := T_Int(Parameterized(param_name'));
    --
    ast_node;
  }

  case t_int_wellconstrained {
    ty_desc =: T_Int(WellConstrained(cs));
    INDEX(i, cs: rename_locals_constraint(cs[i]) -> new_cs[i]);
    ast_node := T_Int(WellConstrained(match_non_empty_list(new_cs)));
    --
    ast_node;
  }

  case t_bits {
    ty_desc =: T_Bits(width, bitfields);
    rename_locals_expr(width) -> width';
    ast_node := T_Bits(width', bitfields);
    --
    ast_node;
  }

  case t_tuple {
    ty_desc =: T_Tuple(types);
    INDEX(i, types: rename_locals_ty(types[i]) -> new_types[i]);
    ast_node := T_Tuple(match_non_empty_list(new_types));
    --
    ast_node;
  }

  case t_array {
    ty_desc =: T_Array(index, elem_ty);
    rename_locals_array_index(index) -> new_index;
    rename_locals_ty(elem_ty) -> new_elem_ty;
    ast_node := T_Array(new_index, new_elem_ty);
    --
    ast_node;
  }

  case t_record {
    ty_desc =: T_Record(fields);
    new_fields := list_map(i, indices(fields),
      if fields[i] =: (field_name, field_ty)
      then (field_name, rename_locals_ty(field_ty))
      else fields[i]
    );
    ast_node := T_Record(new_fields);
    --
    ast_node;
  }

  case t_exception {
    ty_desc =: T_Exception(fields);
    new_fields := list_map(i, indices(fields),
      if fields[i] =: (field_name, field_ty)
      then (field_name, rename_locals_ty(field_ty))
      else fields[i]
    );
    ast_node := T_Exception(new_fields);
    --
    ast_node;
  }

  case t_collection {
    ty_desc =: T_Collection(fields);
    new_fields := list_map(i, indices(fields),
      if fields[i] =: (field_name, field_ty)
      then (field_name, rename_locals_ty(field_ty))
      else fields[i]
    );
    ast_node := T_Collection(new_fields);
    --
    ast_node;
  }
;

function rename_locals_stmt(stmt_desc: stmt) -> (ast_node: stmt)
{
  prose_description = "renames the local storage elements appearing in the statement {stmt_desc}, yielding the statement {ast_node}.",
  prose_transition = "renaming local storage elements in the statement {stmt_desc} yields",
  prose_application = "the result of renaming local storage elements in {stmt_desc}",
} =
  case s_pass {
    stmt_desc = S_Pass;
    --
    S_Pass;
  }

  case s_seq {
    stmt_desc =: S_Seq(first, second);
    rename_locals_stmt(first) -> first';
    rename_locals_stmt(second) -> second';
    ast_node := S_Seq(first', second');
    --
    ast_node;
  }

  case s_decl {
    stmt_desc =: S_Decl(keyword, item, ty_opt, expr_opt);
    rename_locals_ldi(item) -> item';

    case no_type_no_init {
      ty_opt = none;
      expr_opt = none;
      ast_node := S_Decl(keyword, item', none, none);
      --
      ast_node;
    }

    case with_type_no_init {
      ty_opt =: some(ty);
      expr_opt = none;
      rename_locals_ty(ty) -> ty';
      ast_node := S_Decl(keyword, item', some(ty'), none);
      --
      ast_node;
    }

    case no_type_with_init {
      ty_opt = none;
      expr_opt =: some(expr);
      rename_locals_expr(expr) -> expr';
      ast_node := S_Decl(keyword, item', none, some(expr'));
      --
      ast_node;
    }

    case with_type_with_init {
      ty_opt =: some(ty);
      expr_opt =: some(expr);
      rename_locals_ty(ty) -> ty';
      rename_locals_expr(expr) -> expr';
      ast_node := S_Decl(keyword, item', some(ty'), some(expr'));
      --
      ast_node;
    }
  }

  case s_assign {
    stmt_desc =: S_Assign(lhs, rhs);
    rename_locals_lexpr(lhs) -> lhs';
    rename_locals_expr(rhs) -> rhs';
    ast_node := S_Assign(lhs', rhs');
    --
    ast_node;
  }

  case s_call {
    stmt_desc =: S_Call(call_desc);
    call_desc =: [call_name: call_name, params: params, call_args: call_args, call_type: call_type];
    new_params := list_map(param, params, rename_locals_expr(param));
    new_args := list_map(arg, call_args, rename_locals_expr(arg));
    ast_node := S_Call([
      call_name: call_name,
      params: new_params,
      call_args: new_args,
      call_type: call_type
    ]);
    --
    ast_node;
  }

  case s_return {
    stmt_desc =: S_Return(expr_opt);

    case no_value {
      expr_opt = none;
      --
      S_Return(none);
    }

    case with_value {
      expr_opt =: some(expr);
      rename_locals_expr(expr) -> expr';
      ast_node := S_Return(some(expr'));
      --
      ast_node;
    }
  }

  case s_cond {
    stmt_desc =: S_Cond(condition, then_stmt, else_stmt);
    rename_locals_expr(condition) -> condition';
    rename_locals_stmt(then_stmt) -> then_stmt';
    rename_locals_stmt(else_stmt) -> else_stmt';
    ast_node := S_Cond(condition', then_stmt', else_stmt');
    --
    ast_node;
  }

  case s_assert {
    stmt_desc =: S_Assert(condition);
    rename_locals_expr(condition) -> condition';
    ast_node := S_Assert(condition');
    --
    ast_node;
  }

  case s_for {
    stmt_desc =: S_For[
      index_name: index_name,
      start_e: start_e,
      dir: dir,
      end_e: end_e,
      body: body,
      limit: limit
    ];
    rename_locals_name(index_name) -> index_name';
    rename_locals_expr(start_e) -> start_e';
    rename_locals_expr(end_e) -> end_e';
    rename_locals_stmt(body) -> body';

    case no_limit {
      limit = none;
      ast_node := S_For[
        index_name: index_name',
        start_e: start_e',
        dir: dir,
        end_e: end_e',
        body: body',
        limit: none
      ];
      --
      ast_node;
    }

    case with_limit {
      limit =: some(limit_expr);
      rename_locals_expr(limit_expr) -> limit_expr';
      ast_node := S_For[
        index_name: index_name',
        start_e: start_e',
        dir: dir,
        end_e: end_e',
        body: body',
        limit: some(limit_expr')
      ];
      --
      ast_node;
    }
  }

  case s_while {
    stmt_desc =: S_While(condition, limit, body);
    rename_locals_expr(condition) -> condition';
    rename_locals_stmt(body) -> body';

    case no_limit {
      limit = none;
      ast_node := S_While(condition', none, body');
      --
      ast_node;
    }

    case with_limit {
      limit =: some(limit_expr);
      rename_locals_expr(limit_expr) -> limit_expr';
      ast_node := S_While(condition', some(limit_expr'), body');
      --
      ast_node;
    }
  }

  case s_repeat {
    stmt_desc =: S_Repeat(body, condition, limit);
    rename_locals_stmt(body) -> body';
    rename_locals_expr(condition) -> condition';

    case no_limit {
      limit = none;
      ast_node := S_Repeat(body', condition', none);
      --
      ast_node;
    }

    case with_limit {
      limit =: some(limit_expr);
      rename_locals_expr(limit_expr) -> limit_expr';
      ast_node := S_Repeat(body', condition', some(limit_expr'));
      --
      ast_node;
    }
  }

  case s_throw {
    stmt_desc =: S_Throw(exception);
    rename_locals_expr(exception) -> exception';
    ast_node := S_Throw(exception');
    --
    ast_node;
  }

  case s_try {
    stmt_desc =: S_Try(statement, catchers, otherwise_opt);
    rename_locals_stmt(statement) -> statement';
    new_catchers := list_map(catcher, catchers, rename_locals_catcher(catcher));

    case no_otherwise {
      otherwise_opt = none;
      ast_node := S_Try(statement', new_catchers, none);
      --
      ast_node;
    }

    case with_otherwise {
      otherwise_opt =: some(otherwise_stmt);
      rename_locals_stmt(otherwise_stmt) -> otherwise_stmt';
      ast_node := S_Try(statement', new_catchers, some(otherwise_stmt'));
      --
      ast_node;
    }
  }

  case s_print {
    stmt_desc =: S_Print(args, newline);
    new_args := list_map(arg, args, rename_locals_expr(arg));
    ast_node := S_Print(new_args, newline);
    --
    ast_node;
  }

  case s_unreachable {
    stmt_desc = S_Unreachable;
    --
    S_Unreachable;
  }

  case s_pragma {
    stmt_desc =: S_Pragma(name, args);
    new_args := list_map(arg, args, rename_locals_expr(arg));
    ast_node := S_Pragma(name, new_args);
    --
    ast_node;
  }
;

function rename_locals_expr(e: expr) -> (ast_node: expr)
{
  prose_description = "renames the local storage elements appearing in the expression {e}, yielding the expression {ast_node}.",
  prose_transition = "renaming local storage elements in the expression {e} yields",
  prose_application = "the result of renaming local storage elements in {e}",
} =
  case e_literal {
    ast_label(e) = label_E_Literal;
    --
    e;
  }

  case e_var {
    e =: E_Var(name);
    rename_locals_name(name) -> name';
    ast_node := E_Var(name');
    --
    ast_node;
  }

  case e_arbitrary {
    e =: E_Arbitrary(ty);
    rename_locals_ty(ty) -> ty';
    ast_node := E_Arbitrary(ty');
    --
    ast_node;
  }

  case e_atc {
    e =: E_ATC(source, ty);
    rename_locals_expr(source) -> source';
    rename_locals_ty(ty) -> ty';
    ast_node := E_ATC(source', ty');
    --
    ast_node;
  }

  case e_binop {
    e =: E_Binop(op, left, right);
    rename_locals_expr(left) -> left';
    rename_locals_expr(right) -> right';
    ast_node := E_Binop(op, left', right');
    --
    ast_node;
  }

  case e_unop {
    e =: E_Unop(op, subexpression);
    rename_locals_expr(subexpression) -> subexpression';
    ast_node := E_Unop(op, subexpression');
    --
    ast_node;
  }

  case e_call {
    e =: E_Call(call_desc);
    call_desc =: [call_name: call_name, params: params, call_args: call_args, call_type: call_type];
    new_params := list_map(param, params, rename_locals_expr(param));
    new_args := list_map(arg, call_args, rename_locals_expr(arg));
    ast_node := E_Call([
      call_name: call_name,
      params: new_params,
      call_args: new_args,
      call_type: call_type
    ]);
    --
    ast_node;
  }

  case e_slice {
    e =: E_Slice(base, slices);
    rename_locals_expr(base) -> base';
    new_slices := list_map(slice, slices, rename_locals_slice(slice));
    ast_node := E_Slice(base', new_slices);
    --
    ast_node;
  }

  case e_cond {
    e =: E_Cond(test, true_branch, false_branch);
    rename_locals_expr(test) -> test';
    rename_locals_expr(true_branch) -> true_branch';
    rename_locals_expr(false_branch) -> false_branch';
    ast_node := E_Cond(test', true_branch', false_branch');
    --
    ast_node;
  }

  case e_getarray {
    e =: E_GetArray(base, index);
    rename_locals_expr(base) -> base';
    rename_locals_expr(index) -> index';
    ast_node := E_GetArray(base', index');
    --
    ast_node;
  }

  case e_getfield {
    e =: E_GetField(record, field_name);
    rename_locals_expr(record) -> record';
    ast_node := E_GetField(record', field_name);
    --
    ast_node;
  }

  case e_getfields {
    e =: E_GetFields(record, field_names);
    rename_locals_expr(record) -> record';
    ast_node := E_GetFields(record', field_names);
    --
    ast_node;
  }

  case e_record {
    e =: E_Record(record_type, field_initializers);
    new_field_initializers := list_map(i, indices(field_initializers),
      if field_initializers[i] =: (field_name, initializer)
      then (field_name, rename_locals_expr(initializer))
      else field_initializers[i]
    );
    ast_node := E_Record(record_type, new_field_initializers);
    --
    ast_node;
  }

  case e_tuple {
    e =: E_Tuple(components);
    new_components := list_map(component, components, rename_locals_expr(component));
    ast_node := E_Tuple(match_non_empty_list(new_components));
    --
    ast_node;
  }

  case e_pattern {
    e =: E_Pattern(discriminant, pattern);
    rename_locals_expr(discriminant) -> discriminant';
    rename_locals_pattern(pattern) -> pattern';
    ast_node := E_Pattern(discriminant', pattern');
    --
    ast_node;
  }
;

function rename_locals_lexpr(le: lexpr) -> (ast_node: lexpr)
{
  prose_description = "renames the local storage elements appearing in the \assignableexpression{} {le}, yielding the \assignableexpression{} {ast_node}.",
  prose_transition = "renaming local storage elements in the assignable expression {le} yields",
  prose_application = "the result of renaming local storage elements in {le}",
} =
  case le_discard {
    le = LE_Discard;
    --
    LE_Discard;
  }

  case le_var {
    le =: LE_Var(name);
    rename_locals_name(name) -> name';
    ast_node := LE_Var(name');
    --
    ast_node;
  }

  case le_slice {
    le =: LE_Slice(base, slices);
    rename_locals_lexpr(base) -> base';
    new_slices := list_map(slice, slices, rename_locals_slice(slice));
    ast_node := LE_Slice(base', new_slices);
    --
    ast_node;
  }

  case le_setarray {
    le =: LE_SetArray(base, index);
    rename_locals_lexpr(base) -> base';
    rename_locals_expr(index) -> index';
    ast_node := LE_SetArray(base', index');
    --
    ast_node;
  }

  case le_setfield {
    le =: LE_SetField(base, field_name);
    rename_locals_lexpr(base) -> base';
    ast_node := LE_SetField(base', field_name);
    --
    ast_node;
  }

  case le_setfields {
    le =: LE_SetFields(base, field_names);
    rename_locals_lexpr(base) -> base';
    ast_node := LE_SetFields(base', field_names);
    --
    ast_node;
  }

  case le_destructuring {
    le =: LE_Destructuring(subexpressions);
    new_subexpressions := list_map(subexpression, subexpressions, rename_locals_lexpr(subexpression));
    ast_node := LE_Destructuring(new_subexpressions);
    --
    ast_node;
  }
;

function rename_locals_ldi(item: local_decl_item) -> (ast_node: local_decl_item)
{
  prose_description = "renames the local storage elements appearing in the local declaration item {item}, yielding the local declaration item {ast_node}.",
  prose_transition = "renaming local storage elements in the local declaration item {item} yields",
  prose_application = "the result of renaming local storage elements in {item}",
} =
  case var {
    item =: LDI_Var(name);
    rename_locals_name(name) -> name';
    ast_node := LDI_Var(name');
    --
    ast_node;
  }

  case tuple {
    item =: LDI_Tuple(names);
    new_names := list_map(name, names, rename_locals_name(name));
    ast_node := LDI_Tuple(new_names);
    --
    ast_node;
  }
;

function rename_locals_constraint(c: int_constraint) -> (ast_node: int_constraint)
{
  prose_description = "renames the local storage elements appearing in the constraint {c}, yielding the constraint {ast_node}.",
  prose_transition = "renaming local storage elements in the constraint {c} yields",
  prose_application = "the result of renaming local storage elements in {c}",
} =
  case exact {
    c =: Constraint_Exact(subexpression);
    rename_locals_expr(subexpression) -> subexpression';
    ast_node := Constraint_Exact(subexpression');
    --
    ast_node;
  }

  case range {
    c =: Constraint_Range(start_expression, end_expression);
    rename_locals_expr(start_expression) -> start_expression';
    rename_locals_expr(end_expression) -> end_expression';
    ast_node := Constraint_Range(start_expression', end_expression');
    --
    ast_node;
  }
;

function rename_locals_slice(s: slice) -> (ast_node: slice)
{
  prose_description = "renames the local storage elements appearing in the slice {s}, yielding {ast_node}.",
  prose_transition = "renaming local storage elements in the slice {s} yields",
  prose_application = "the result of renaming local storage elements in {s}",
} =
  case single {
    s =: Slice_Single(expr);
    rename_locals_expr(expr) -> expr';
    ast_node := Slice_Single(expr');
    --
    ast_node;
  }

  case slice_length {
    s =: Slice_Length(expr1, expr2);
    rename_locals_expr(expr1) -> expr1';
    rename_locals_expr(expr2) -> expr2';
    ast_node := Slice_Length(expr1', expr2');
    --
    ast_node;
  }

  case slice_range {
    s =: Slice_Range(expr1, expr2);
    rename_locals_expr(expr1) -> expr1';
    rename_locals_expr(expr2) -> expr2';
    ast_node := Slice_Range(expr1', expr2');
    --
    ast_node;
  }

  case slice_star {
    s =: Slice_Star(expr1, expr2);
    rename_locals_expr(expr1) -> expr1';
    rename_locals_expr(expr2) -> expr2';
    ast_node := Slice_Star(expr1', expr2');
    --
    ast_node;
  }
;

function rename_locals_name(name: Identifier) -> (ast_node: Identifier)
{
  prose_description = "renames the local identifier {name}, yielding the identifier {ast_node}.",
  prose_transition = "renaming the local identifier {name} yields",
  prose_application = "the result of renaming the local identifier {name}",
} =
  ast_node := concat(stdlib_local_prefix, name);
  --
  ast_node;
;

function rename_locals_array_index(index: array_index) -> (ast_node: array_index)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the array index {index}, yielding the array index {ast_node}.",
  prose_transition = "renaming local storage elements in the array index {index} yields",
  prose_application = "the result of renaming local storage elements in {index}",
} =
  case enum {
    index =: ArrayLength_Enum(enumeration_name, enumeration_labels);
    ast_node := ArrayLength_Enum(enumeration_name, enumeration_labels);
    --
    ast_node;
  }

  case expr {
    index =: ArrayLength_Expr(length);
    rename_locals_expr(length) -> length';
    ast_node := ArrayLength_Expr(length');
    --
    ast_node;
  }
;

function rename_locals_pattern(pattern_desc: pattern) -> (ast_node: pattern)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the pattern {pattern_desc}, yielding the pattern {ast_node}.",
  prose_transition = "renaming local storage elements in the pattern {pattern_desc} yields",
  prose_application = "the result of renaming local storage elements in {pattern_desc}",
} =
  case all {
    pattern_desc = Pattern_All;
    --
    Pattern_All;
  }

  case mask {
    pattern_desc =: Pattern_Mask(mask);
    --
    Pattern_Mask(mask);
  }

  case pattern_any {
    pattern_desc =: Pattern_Any(patterns);
    new_patterns := list_map(pattern, patterns, rename_locals_pattern(pattern));
    ast_node := Pattern_Any(new_patterns);
    --
    ast_node;
  }

  case pattern_tuple {
    pattern_desc =: Pattern_Tuple(patterns);
    new_patterns := list_map(pattern, patterns, rename_locals_pattern(pattern));
    ast_node := Pattern_Tuple(match_non_empty_list(new_patterns));
    --
    ast_node;
  }

  case geq {
    pattern_desc =: Pattern_Geq(expr);
    rename_locals_expr(expr) -> expr';
    ast_node := Pattern_Geq(expr');
    --
    ast_node;
  }

  case leq {
    pattern_desc =: Pattern_Leq(expr);
    rename_locals_expr(expr) -> expr';
    ast_node := Pattern_Leq(expr');
    --
    ast_node;
  }

  case single {
    pattern_desc =: Pattern_Single(expr);
    rename_locals_expr(expr) -> expr';
    ast_node := Pattern_Single(expr');
    --
    ast_node;
  }

  case not_case {
    pattern_desc =: Pattern_Not(subpattern);
    rename_locals_pattern(subpattern) -> subpattern';
    ast_node := Pattern_Not(subpattern');
    --
    ast_node;
  }

  case range {
    pattern_desc =: Pattern_Range(start_expression, end_expression);
    rename_locals_expr(start_expression) -> start_expression';
    rename_locals_expr(end_expression) -> end_expression';
    ast_node := Pattern_Range(start_expression', end_expression');
    --
    ast_node;
  }
;

function rename_locals_catcher(catcher_desc: catcher) -> (ast_node: catcher)
{
  prose_description = "renames the identifiers corresponding to local storage elements that appear in the \catcherterm{} {catcher_desc}, yielding the \catcherterm{} {ast_node}.",
  prose_transition = "renaming local storage elements in the catcher {catcher_desc} yields",
  prose_application = "the result of renaming local storage elements in {catcher_desc}",
} =
  catcher_desc =: (variable, guard_type, execute);
  rename_locals_ty(guard_type) -> guard_type';
  rename_locals_stmt(execute) -> execute';

  case anonymous {
    variable = none;
    ast_node := (none, guard_type', execute');
    --
    ast_node;
  }

  case named {
    variable =: some(name);
    rename_locals_name(name) -> name';
    ast_node := (some(name'), guard_type', execute');
    --
    ast_node;
  }
;

function set_builtin(decl: decl) -> (ast_node: decl) | build_error
{
  prose_description = "sets the $\funcbuiltin$ flag of the top-level function declaration {decl}, which is used to identify standard library functions in \TypingRuleRef{InsertStdlibParam}. \ProseOtherwiseBuildError",
  prose_transition = "marking {decl} as builtin yields",
  prose_application = "the declaration obtained by marking {decl} as builtin",
} =
  be_check(ast_label(decl) = label_D_Func, BE_BuildBadDeclaration) -> True;
  decl =: D_Func(funcsig);
  funcsigp := funcsig(builtin: True);
  ast_node := D_Func(funcsigp);
  --
  ast_node;
;
