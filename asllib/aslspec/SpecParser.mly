%{
open AST
open AST.AttributeKey

(** Elements such as types, constants, and relations generate custom LaTeX macros.
    Since LaTeX macro names do not allow certain characters, such as numbers and apostrophes,
    we need to check that element-defining identifiers conform to those restrictions.
*)
let check_definition_name name =
   let () = assert (String.length name > 0) in
   let id_regexp = Str.regexp "^[A-Za-z_']+$" in
   if not (Str.string_match id_regexp name 0) then
     let msg = Format.sprintf "illegal element-defining identifier: %s" name in
     raise (SpecError msg)

let bool_of_string s =
  match String.lowercase_ascii s with
  | "true" -> true
  | "false" -> false
  | _ ->
      let msg = Format.sprintf "expected boolean string (true/false), got: %s" s in
      raise (SpecError msg)
%}

%type <AST.t> spec
%start spec

%token EOF

%token <string> IDENTIFIER LATEX_MACRO STRING

(* Keyword tokens *)
%token ASSOCIATIVE
%token AST
%token CASE
%token COLON_EQ
%token EQ_COLON
%token CONSTANT
%token CONSTANTS_SET
%token CUSTOM
%token FUN
%token FUNCTION
%token INDEX
%token LATEX
%token LIST0
%token LIST1
%token MATH_MACRO
%token MATH_LAYOUT
%token LHS_HYPERTARGETS
%token AUTO_NAME
%token OPTION
%token OPERATOR
%token PARTIAL
%token POWERSET
%token POWERSET_FINITE
%token PROSE_APPLICATION
%token PROSE_DESCRIPTION
%token RENDER
%token RELATION
%token RULE
%token SEMANTICS
%token SHORT_CIRCUIT_MACRO
%token TYPEDEF
%token TYPING
%token VARIADIC

%token IFF
%token LIST
%token SET
%token SIZE
%token SOME

(* Punctuation and operator tokens *)
%token ARROW
%token COMMA
%token COLON
%token DOT
%token EQ
%token IN
%token NOT_IN
%token SEMI
%token VDASH
%token LPAR
%token RPAR
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token MINUS
%token MINUS_MINUS

%token PLUS
%token TIMES
%token DIVIDE
%token EXPONENT
%token AND
%token OR
%token LE
%token LT
%token GE
%token GT
%token NEQ

%token IF THEN ELSE
%right AND
%right OR
%right ELSE

%nonassoc COLON_EQ
%nonassoc EQ_COLON
%nonassoc EQ
%nonassoc IN
%nonassoc NOT_IN
%nonassoc IFF
%nonassoc LE
%nonassoc LT
%nonassoc GE
%nonassoc GT
%nonassoc NEQ

%right MINUS
%right PLUS
%right TIMES
%right DIVIDE
%right EXPONENT

%left LPAR

%%

(* ------------------------------------------------------------------------
   Parameterized helper productions
  ------------------------------------------------------------------------- *)

(* Pair matching *)
let     pared(x) == delimited(    LPAR, x, RPAR    )
let    braced(x) == delimited(  LBRACE, x, RBRACE  )
let bracketed(x) == delimited(LBRACKET, x, RBRACKET)

(* Option handling *)
(* [some] returns an option, but ensures it is there. *)
let some(x) == ~ = x ; <Some>

(* We reverse the standard [terminated] to increase clarity on some complex
   rules. *)
let terminated_by(x, y) == terminated(y, x)

(* Position annotation *)
let annotated(x) == desc = x; { { desc; pos_start=$symbolstartpos; pos_end=$endpos } }

(* ------------------------------------------------------------------------- *)
(* Parameterized lists *)

(* A non-empty comma-separated list. *)
let clist1(x) :=
  | x=x; { [ x ] }
  | h=x; COMMA; t=clist1(x); { h :: t }

(* A comma separated list. *)
let clist0(x) := { [] } | clist1(x)

(* A comma separated list with at least 2 elements. *)
let clist2(x) := ~=x; COMMA; li=clist1(x); { x :: li }

(* A comma-separated trailing list. *)
let tclist0(x) := { [] } | tclist1(x)

(* A comma-separated non-empty trailing list. *)
let tclist1(x) :=
  | x=x; ioption(COMMA); { [ x ] }
  | h=x; COMMA; t=tclist1(x); { h :: t }

(* A parenthesised comma-separated list *)
let plist0(x) == pared(clist0(x))

(* A parenthesised comma-separated list with at least one element. *)
let plist1(x) == pared(clist1(x))

(* A parenthesised comma-separated list with at least 2 elements. *)
let plist2(x) == pared(clist2(x))

(* A parameterized list with at least 1 element *)
let list1(x) :=
  | ~=x; { [ x ] }
  | ~=x; l=list1(x); { x :: l }

(* ---------------------------------------- *)
(* Actual AST rules                         *)
(* ---------------------------------------- *)

let spec := elems=terminated(list(terminated(elem, SEMI)), EOF); { elems }

let elem :=
    | type_definition
    | relation_definition
    | operator_definition
    | constant_definition
    | render_types
    | render_rule

let type_kind := TYPEDEF; { Term.TypeKind_Generic }
    | AST; { Term.TypeKind_AST }

let type_definition :=
    | ~=type_kind; type_name=IDENTIFIER; ~=type_attributes; ~=type_variants_with_attributes;
    {   check_definition_name type_name;
        Elem_Type (Type.make type_kind type_name type_variants_with_attributes type_attributes) }
    | type_kind; type_name=IDENTIFIER; type_attributes; list1(type_variant_with_attributes);
    {   let msg = Format.sprintf "Definition of 'typedef %s' is missing '='" type_name in
        raise (SpecError msg) }

let relation_definition :=
    ~=relation_category; ~=relation_property; name=IDENTIFIER; input=plist0(opt_named_type_term); ARROW; output=type_variants;
    ~=relation_attributes; ~=opt_relation_rule;
    {   check_definition_name name;
        Elem_Relation (Relation.make name relation_property relation_category input output relation_attributes opt_relation_rule) }

let operator_definition :=
    ~=is_variadic; OPERATOR; name=IDENTIFIER; ~=parameters; input=plist0(opt_named_type_term); ARROW; output=type_term;
    ~=operator_attributes;
    {   check_definition_name name;
        Elem_Relation (Relation.make_operator name parameters input output is_variadic operator_attributes) }

let is_variadic :=
    | VARIADIC; { true }
    | { false }

let parameters :=
    | LBRACKET; params=tclist1(IDENTIFIER); RBRACKET; { params }
    | { [] }

let constant_definition := CONSTANT; name=IDENTIFIER; ~=opt_type; att=type_attributes; ~=opt_value_and_attributes;
    {   check_definition_name name;
        Elem_Constant (Constant.make name opt_type opt_value_and_attributes att) }

let opt_type :=
    | { None }
    | COLON; type_term=type_term; { Some type_term }

let opt_value_and_attributes :=
    | { None }
    | EQ; ~=expr; att=constant_value_attributes; { Some (expr, att) }

let constant_value_attributes ==
    | { [] }
    | LBRACE; attr=math_layout_attribute; RBRACE; { [attr] }

let relation_property :=
    | RELATION; { Relation.RelationProperty_Relation }
    | FUNCTION; { Relation.RelationProperty_Function }

let relation_category :=
    | { None }
    | TYPING; { Some Relation.RelationCategory_Typing }
    | SEMANTICS; { Some Relation.RelationCategory_Semantics }

let type_attributes ==
    | { [] }
    | LBRACE; pairs=tclist0(type_attribute); RBRACE; { pairs }

let type_attribute :=
    | prose_description_attribute
    | math_macro_attribute
    | math_layout_attribute
    | short_circuit_macro_attribute

let prose_description_attribute ==
    | PROSE_DESCRIPTION; EQ; template=STRING; { (Prose_Description, StringAttribute template) }
    | template=STRING; { (Prose_Description, StringAttribute template) }

let math_macro_attribute ==
    MATH_MACRO; EQ; macro=LATEX_MACRO; { (Math_Macro, MathMacroAttribute macro) }

let math_layout_attribute ==
    | MATH_LAYOUT; EQ; ~=math_layout; { (Math_Layout, MathLayoutAttribute math_layout) }
    | ~=math_layout; { (Math_Layout, MathLayoutAttribute math_layout) }

let short_circuit_macro_attribute ==
    SHORT_CIRCUIT_MACRO; EQ; macro=LATEX_MACRO; { (Short_Circuit_Macro, MathMacroAttribute macro) }

let relation_attributes ==
    LBRACE; pairs=tclist0(relation_attribute); RBRACE; { pairs }

let relation_attribute :=
    | prose_application_attribute
    | type_attribute

let operator_attributes ==
    LBRACE; pairs=tclist0(operator_attribute); RBRACE; { pairs }

let operator_attribute :=
    | prose_description_attribute
    | math_macro_attribute
    | operator_style_attribute
    | prose_application_attribute

let operator_style_attribute ==
    | ASSOCIATIVE; EQ; value=IDENTIFIER; { (Associative, BoolAttribute (bool_of_string value)) }
    | CUSTOM; EQ; value=IDENTIFIER; { (Custom, BoolAttribute (bool_of_string value)) }

let prose_application_attribute ==
    PROSE_APPLICATION; EQ; template=STRING; { (Prose_Application, StringAttribute template) }

let type_variants_with_attributes :=
    | { [] }
    | EQ; head=type_term_with_attributes; tail=list(type_variant_with_attributes); { head :: tail }
    | EQ;  VDASH; head=type_term_with_attributes; tail=list(type_variant_with_attributes); { head :: tail }
let type_variant_with_attributes :=
    | VDASH; term_with_attributes=type_term_with_attributes; { term_with_attributes }

let type_variants :=
    | head=type_term; tail=list(type_variant); { head :: tail }
    | VDASH; head=type_term; tail=list(type_variant); { head :: tail }
let type_variant := VDASH; term=type_term; { term }

let type_term_with_attributes := ~=type_term; ~=type_attributes;
    { TypeVariant.make TypeKind_Generic type_term type_attributes }

let type_term :=
    | name=IDENTIFIER; { check_definition_name name; Label name }
    | op=type_operator; LPAR; ~=opt_named_type_term; RPAR; { Term.make_type_operation op opt_named_type_term }
    | LPAR; args=tclist1(opt_named_type_term); RPAR; { Tuple {label_opt = None; args} }
    | label=IDENTIFIER; LPAR; args=tclist1(opt_named_type_term); RPAR;
    {   check_definition_name label;
        Tuple {label_opt = Some label; args} }
    | LBRACKET; fields=tclist1(record_field); RBRACKET; { Term.make_record fields }
    | label=IDENTIFIER; LBRACKET; fields=tclist1(record_field); RBRACKET;
    {   check_definition_name label;
        Term.make_labelled_record label fields }
    | CONSTANTS_SET; LPAR; constants=tclist1(IDENTIFIER); RPAR; { ConstantsSet constants }
    | FUN; from_type=opt_named_type_term; ARROW; to_type=opt_named_type_term; { Function {from_type; to_type; total = true}}
    | PARTIAL; from_type=opt_named_type_term; ARROW; to_type=opt_named_type_term; { Function {from_type; to_type; total = false}}

let type_operator :=
    | POWERSET; { Powerset }
    | POWERSET_FINITE; { Powerset_Finite }
    | LIST0; { List0 }
    | LIST1; { List1 }
    | OPTION; { Option }

let named_type_term ==
    name=IDENTIFIER; COLON; ~=type_term; { (name, type_term) }

let opt_named_type_term ==
    | name=IDENTIFIER; COLON; ~=type_term; { (Some name, type_term) }
    | ~=type_term; { (None, type_term) }

let record_field := ~=named_type_term; ~=type_attributes;
    { Term.make_record_field named_type_term type_attributes }

let math_layout :=
    | IDENTIFIER; { Unspecified }
    | LPAR; inner=clist0(math_layout); RPAR; { Horizontal inner }
    | IDENTIFIER; LPAR; inner=clist0(math_layout); RPAR; { Horizontal inner }
    | LBRACKET; inner=clist0(math_layout); RBRACKET; { Vertical inner }
    | IDENTIFIER; LBRACKET; inner=clist0(math_layout); RBRACKET; { Vertical inner }

let render_types :=
    RENDER; name=IDENTIFIER; ~=render_types_attributes; EQ; pointers=clist1(type_subset_pointer);
    {   check_definition_name name;
        Elem_RenderTypes (TypesRender.make name pointers render_types_attributes) }

let type_subset_pointer :=
    | type_name=IDENTIFIER; LPAR; MINUS; RPAR; { (type_name, []) }
    | type_name=IDENTIFIER; LPAR; variant_names=tclist1(IDENTIFIER); RPAR; { (type_name, variant_names) }

let render_types_attributes ==
    | { [] }
    | LBRACE; pairs=tclist0(render_types_attribute); RBRACE; { pairs }

let render_types_attribute :=
    | lhs_hypertargets

let lhs_hypertargets ==
    | LHS_HYPERTARGETS; EQ; value=IDENTIFIER;{ (LHS_Hypertargets, BoolAttribute (bool_of_string value)) }

let opt_relation_rule := { None }
    | EQ; ~=rule; { Some rule }

let rule := rule_elements=list1(rule_element); { rule_elements }

let rule_element :=
    | ~=judgment; SEMI; { Rule.Judgment judgment }
    | ~=rule_case; { Rule.Cases [ rule_case ] }

let rule_case := CASE; name=IDENTIFIER; LBRACE; elements=list(rule_element); RBRACE;
      { Rule.make_case name elements }

let judgment :=
    | ~=maybe_output_expr; ~=expr; ~=judgment_attributes;
      { Rule.make_judgement expr ~is_output:maybe_output_expr judgment_attributes  }
    | ~=judgment_expr; ~=judgment_attributes;
      { Rule.make_judgement judgment_expr ~is_output:false judgment_attributes }

let index_judgement :=
    | INDEX; LPAR; index=IDENTIFIER; COMMA; list_var=IDENTIFIER; COLON; body=transition_expr; RPAR;
      { Expr.Indexed { index; list_var; body } }

let var_expr := id=IDENTIFIER; { Expr.make_var id }

let expr :=
    | var_expr
    | args=plist1(expr);
      { Expr.make_tuple args }
    | lhs=expr; args=plist0(expr);
      { Expr.make_application lhs args }
    | var=IDENTIFIER; DOT; ~=field_path;
      { Expr.FieldAccess { var; fields = field_path} }
    | list_var=IDENTIFIER; LBRACKET; index=expr; RBRACKET;
      { Expr.make_list_index list_var index }
    | label_opt=ioption(IDENTIFIER); LBRACKET; fields=tclist1(field_and_value); RBRACKET;
      { Expr.make_record label_opt fields }
    | lhs=expr; ~=infix_expr_operator; rhs=expr;
      { Expr.make_operator_application infix_expr_operator [lhs; rhs] }
    | IF; cond=expr; THEN; then_branch=expr; ELSE; else_branch=expr;
      { Expr.make_operator_application "if_then_else" [cond; then_branch; else_branch] }

let maybe_output_expr ==
    | { false }
    | MINUS_MINUS; { true }

let judgment_expr :=
    | transition_expr
    | LPAR; ~=transition_expr; RPAR;
      { Expr.make_tuple [ transition_expr ] }
    | index_judgement
    | LPAR; ~=index_judgement; RPAR;
      { Expr.make_tuple [ index_judgement ] }

let transition_expr :=
    | lhs=expr; ARROW; rhs=transition_output_expr; ~=short_circuit;
      { Expr.Transition { lhs; rhs; short_circuit } }

let transition_output_expr :=
    | var_expr
    | args=plist1(transition_output_expr);
      { Expr.make_tuple args }
    | label=IDENTIFIER; args=plist1(transition_output_expr);
      { Expr.make_application (Expr.make_var label) args }
    | list_var=IDENTIFIER; LBRACKET; index=transition_output_expr; RBRACKET;
      { Expr.make_list_index list_var index }
    | lhs=transition_output_expr; ~=infix_expr_operator; rhs=transition_output_expr;
      { Expr.make_operator_application infix_expr_operator [lhs; rhs] }

let short_circuit :=
    | { None } (* Short-circuiting expressions will be inserted automatically. *)
    | VDASH; (* Explicitly no alternatives. *)
      { Some [] }
    | VDASH; alternatives=clist1(short_circuit_expr);
      { Some alternatives }

let short_circuit_expr :=
    | id=IDENTIFIER;
      { Expr.make_var id }
    | lhs=IDENTIFIER; args=plist0(IDENTIFIER);
      { Expr.make_application (Expr.make_var lhs) (List.map Expr.make_var args) }

let field_path :=
  | id=IDENTIFIER; { [ id ] }
  | id1=IDENTIFIER; DOT; fields=field_path; { id1 :: fields }

let infix_expr_operator ==
    | COLON_EQ; { "assign" }
    | EQ_COLON; { "reverse_assign" }
    | EQ; { "equal" }
    | MINUS; { "num_minus" }
    | PLUS;  { "num_plus" }
    | TIMES; { "num_times" }
    | DIVIDE; { "num_divide" }
    | EXPONENT; { "num_exponent" }
    | AND; { "and" }
    | OR; { "or" }
    | IN; { "member" }
    | NOT_IN; { "not_member" }
    | IFF; { "iff" }
    | LE; { "less_or_equal" }
    | LT; { "less_than" }
    | GE; { "greater_or_equal" }
    | GT; { "greater_than" }
    | NEQ; { "not_equal" }

let field_and_value == field=IDENTIFIER; COLON; value=expr; { (field, value) }

let judgment_attributes ==
    | { [] }
    | LBRACE; pairs=tclist0(judgment_attribute); RBRACE; { pairs }

let judgment_attribute :=
    | math_layout_attribute
    | auto_name_attribute

let auto_name_attribute := AUTO_NAME; EQ; value=IDENTIFIER; { (Auto_Name, BoolAttribute (bool_of_string value)) }

let render_rule :=
  | RENDER; RULE; name=IDENTIFIER; EQ; relation_name=IDENTIFIER; rule_name=pared(rule_name);
    { check_definition_name name; Elem_RenderRule (RuleRender.make ~name ~relation_name rule_name) }
  | RENDER; RULE; name=IDENTIFIER;
    { check_definition_name name; Elem_RenderRule (RuleRender.make ~name ~relation_name:name []) }
    (** Stands for the entire rule for the given relation. *)

let rule_name :=
  | ~=rule_path; { rule_path }
  | MINUS; { [ "" ] }
    (** The empty string is a prefix of every rule name. *)

let rule_path := name=IDENTIFIER; { [name] }
  | name=IDENTIFIER; DOT; rest=rule_path; { name :: rest }
