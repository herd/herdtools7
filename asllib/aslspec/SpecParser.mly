%{
open AST
open AST.AttributeKey

let check_definition_name name =
   let () = assert (String.length name > 0) in
   let id_regexp = Str.regexp "^[A-Za-z_']+$" in
   if not (Str.string_match id_regexp name 0) then
     let msg = Format.sprintf "illegal element-defining identifier: %s" name in
     raise (SpecError msg)
%}

%type <AST.t> spec
%start spec

%token EOF

%token <string> IDENTIFIER LATEX_MACRO STRING

(* Keyword tokens *)
%token AST
%token CONSTANT
%token CONSTANTS_SET
%token FUN
%token FUNCTION
%token PARTIAL
%token LIST0
%token LIST1
%token MATH_MACRO
%token MATH_LAYOUT
%token OPTION
%token POWERSET
%token POWERSET_FINITE
%token PROSE_APPLICATION
%token PROSE_DESCRIPTION
%token RENDER
%token RELATION
%token SEMANTICS
%token SHORT_CIRCUIT_MACRO
%token TYPEDEF
%token TYPING

(* Punctuation and operator tokens *)
%token ARROW
%token COMMA
%token COLON
%token EQ
%token SEMI
%token VDASH
%token LPAR
%token RPAR
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token MINUS

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

(* A parenthesised comma-separated list with at least 2 elements. *)
let plist2(x) == pared(clist2(x))

(* A parameterized list with at least 1 element *)
let list1(x) :=
  | ~=x; { [ x ] }
  | ~=x; l=list1(x); { x :: l }

(* ---------------------------------------- *)
(* Actual AST rules                         *)
(* ---------------------------------------- *)

let spec := elems=terminated(list(elem), EOF); { elems }

let elem :=
    | type_definition
    | relation_definition
    | constant_definition
    | render_definition

let type_kind := TYPEDEF; { TypeKind_Generic }
    | AST; { TypeKind_AST }

let type_definition :=
    | ~=type_kind; type_name=IDENTIFIER; ~=type_attributes; SEMI;
    {   check_definition_name type_name;
        Elem_Type (Type.make type_kind type_name [] type_attributes) }
    | ~=type_kind; type_name=IDENTIFIER; ~=type_attributes; EQ; ~=type_variants_with_attributes; SEMI;
    {   check_definition_name type_name;
        Elem_Type (Type.make type_kind type_name type_variants_with_attributes type_attributes) }
    | type_kind; type_name=IDENTIFIER; type_attributes; type_variants_with_attributes; SEMI;
    {   let msg = Format.sprintf "Definition of 'typedef %s' is missing '='" type_name in
        raise (SpecError msg) }

let relation_definition :=
    ~=relation_category; ~=relation_property; name=IDENTIFIER; input=plist0(opt_named_type_term); ARROW; output=type_variants;
    attributes=relation_attributes; SEMI;
    {   check_definition_name name;
        Elem_Relation (Relation.make name relation_property relation_category input output attributes) }

let constant_definition := CONSTANT; name=IDENTIFIER; att=type_attributes; SEMI;
    {   check_definition_name name;
        Elem_Constant (Constant.make name att) }

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
    | PROSE_DESCRIPTION; EQ; template=STRING; { (Prose_Description, StringAttribute template) }
    | template=STRING; { (Prose_Description, StringAttribute template) }
    | MATH_MACRO; EQ; macro=LATEX_MACRO; { (Math_Macro, MathMacroAttribute macro) }
    | MATH_LAYOUT; EQ; ~=math_layout; { (Math_Layout, MathLayoutAttribute math_layout) }
    | SHORT_CIRCUIT_MACRO; EQ; macro=LATEX_MACRO; { (Short_Circuit_Macro, MathMacroAttribute macro) }

let relation_attributes ==
    LBRACE; pairs=tclist0(relation_attribute); RBRACE; { pairs }

let relation_attribute :=
    | PROSE_DESCRIPTION; EQ; template=STRING; { (Prose_Description, StringAttribute template) }
    | template=STRING; { (Prose_Description, StringAttribute template) }
    | PROSE_APPLICATION; EQ; template=STRING; { (Prose_Application, StringAttribute template) }
    | MATH_MACRO; EQ; macro=LATEX_MACRO; { (Math_Macro, MathMacroAttribute macro) }
    | MATH_LAYOUT; EQ; ~=math_layout; { (Math_Layout, MathLayoutAttribute math_layout) }

let type_variants_with_attributes :=
    | head=type_term_with_attributes; tail=list(type_variant_with_attributes); { head :: tail }
    | VDASH; head=type_term_with_attributes; tail=list(type_variant_with_attributes); { head :: tail }
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
    | op=operator; LPAR; ~=opt_named_type_term; RPAR; { make_operator op opt_named_type_term }
    | LPAR; components=tclist1(opt_named_type_term); RPAR; { LabelledTuple {label_opt = None; components} }
    | label=IDENTIFIER; LPAR; components=tclist1(opt_named_type_term); RPAR;
    {   check_definition_name label;
        LabelledTuple {label_opt = Some label; components} }
    | LBRACKET; fields=tclist1(named_type_term); RBRACKET; { make_record fields }
    | label=IDENTIFIER; LBRACKET; fields=tclist1(named_type_term); RBRACKET;
    {   check_definition_name label;
        make_labelled_record label fields }
    | CONSTANTS_SET; LPAR; constants=tclist1(IDENTIFIER); RPAR; { ConstantsSet constants }
    | FUN; from_type=opt_named_type_term; ARROW; to_type=opt_named_type_term; { Function {from_type; to_type; total = true}}
    | PARTIAL; from_type=opt_named_type_term; ARROW; to_type=opt_named_type_term; { Function {from_type; to_type; total = false}}

let operator :=
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

let math_layout :=
    | IDENTIFIER; { Unspecified }
    | LPAR; inner=clist0(math_layout); RPAR; { Horizontal inner }
    | IDENTIFIER; LPAR; inner=clist0(math_layout); RPAR; { Horizontal inner }
    | LBRACKET; inner=clist0(math_layout); RBRACKET; { Vertical inner }
    | IDENTIFIER; LBRACKET; inner=clist0(math_layout); RBRACKET; { Vertical inner }

let render_definition :=
    RENDER; name=IDENTIFIER; EQ; pointers=clist1(type_subset_pointer); SEMI;
    {   check_definition_name name;
        Elem_Render (TypesRender.make name pointers) }

let type_subset_pointer :=
    | type_name=IDENTIFIER; LPAR; MINUS; RPAR; { (type_name, []) }
    | type_name=IDENTIFIER; LPAR; variant_names=tclist1(IDENTIFIER); RPAR; { (type_name, variant_names) }
