
%{

  let build_expr_conds =
    let open AST in
    let make_cond { desc = (c, e_then); _ } e_else = AST.E_Cond (c, e_then, e_else) in
    fun (elseifs, e) -> List.fold_right (ASTUtils.map2_desc make_cond) elseifs e

  let tr_get_fields (e, fields) =
    let open AST in
    let one_field f = E_GetField (e, f, TA_None) |> ASTUtils.add_dummy_pos in
    E_Concat (List.map one_field fields)

  let build_stmt_conds (s_elsifs, s_else) =
    let open AST in
    let s_else = match s_else with
    | Some s -> s
    | None -> ASTUtils.s_pass
    in
    let folder { desc = (c, s_then); _ } s_else = AST.S_Cond (c, s_then, s_else) in
    List.fold_right (ASTUtils.map2_desc folder) s_elsifs s_else

%}

%token <string> IDENTIFIER STRING_LIT MASK_LIT
%token <Bitvector.t> BITS_LIT
%token <int> INT_LIT
%token <float> REAL_LIT
%token <string> QUALIFIER

%token AMP
%token AMP_AMP
%token AND
%token ARRAY
%token ASSERT
%token BANG
%token BAR_BAR
%token BITS
%token UU_ARRAY
%token UU_BUILTIN
%token UU_CONDITIONAL
%token UU_CONFIG
%token UU_DECODE
%token UU_ENCODING
%token UU_EXCEPTIONTAKEN
%token UU_EXECUTE
%token UU_EVENT
%token UU_FIELD
%token UU_FUNCTION
%token UU_GUARD
%token UU_INSTRUCTION
%token UU_INSTRUCTION_SET
%token UU_MAP
%token UU_NOP
%token UU_NEWEVENT
%token UU_NEWMAP
%token UU_OPCODE
%token UU_OPERATOR_ONE
%token UU_OPERATOR_TWO
%token UU_POSTDECODE
%token UU_READWRITE
%token UU_REGISTER
%token UU_UNALLOCATED
%token UU_UNPREDICTABLE_UNLESS
%token UU_UNPREDICTABLE
%token UU_WRITE
%token CARET
%token CASE
%token CATCH
%token COLON
%token COMMA
%token CONSTANT
%token CONSTRAINED_UNPRED
%token DEDENT
%token DIV
%token DO
%token DOT
%token DOT_DOT
%token DOWNTO
%token ELSE
%token ELSIF
%token ENUMERATION
%token EOF
%token EOL
%token EOR
%token EQ
%token EQ_EQ
%token EQ_GT
%token FOR
%token GT
%token GT_EQ
%token GT_GT
%token IF
%token IMPLEM_DEFINED
%token IN
%token IFF
%token IMPLIES
%token INDENT
%token IS
%token LBRACE
%token LBRACE_LBRACE
%token LBRACK
%token LPAREN
%token LT
%token LT_EQ
%token LT_LT
%token MINUS
%token MOD
%token BANG_EQ
%token NOT
%token OF
%token OR
%token OTHERWISE
%token PLUS
%token PLUS_PLUS
%token PLUS_COLON
%token QUOT
%token RBRACE
%token RBRACE_RBRACE
%token RBRACK
%token RECORD
%token REM
%token REPEAT
%token RETURN
%token RPAREN
%token SEE
%token SEMICOLON
%token SLASH
%token STAR
%token THEN
%token THROW
%token TO
%token TRY
%token TYPE
%token TYPEOF
%token UNDEFINED
%token UNKNOWN
%token UNPREDICTABLE
%token UNTIL
%token WHEN
%token WHILE

%type <AST.stmt> simple_stmts
%type <AST.stmt> simple_stmt_list
%type <AST.stmt> stmts
%type <AST.t> ast
%start ast

%nonassoc ELSE
%left COLON
%left AMP_AMP BAR_BAR
%left EQ_EQ BANG_EQ
%nonassoc GT_EQ LT_EQ LT GT
%left PLUS MINUS EOR AND OR
%left STAR SLASH MOD LT_LT GT_GT DIV
%left CARET
%nonassoc BANG NOT
%nonassoc IN
%left DOT LBRACK

%%

let ast := list(EOL); terminated (list(decl), EOF)

let decl ==
  | variable_decl
  | function_decl
  | procedure_decl
  | getter_decl
  | setter_decl
  | type_decl

let annotated(x) == desc = x; { AST.{ desc; pos_start=$symbolstartpos; pos_end=$endpos }}

let unimplemented_decl(x) ==
  x=annotated(x) ; { AST.(D_GlobalConst ("-", ASTUtils.add_pos_from x @@ T_Int None, ASTUtils.add_pos_from x @@ E_Literal (V_Int 0))) }

let type_decl ==
  | terminated_by(SEMICOLON; EOL,
    | TYPE; ~=tidentdecl; EQ; ~=ty; < AST.D_TypeDecl >
    | RECORD; x=tidentdecl; fields=annotated(braced(nlist(field)));
      { AST.(D_TypeDecl (x, ASTUtils.add_pos_from fields (T_Record fields.desc))) }
    | ENUMERATION; x=tidentdecl; li=annotated(braced(clist(ident)));
      { AST.(D_TypeDecl (x, ASTUtils.add_pos_from li (T_Enum li.desc))) }

    | unimplemented_decl(
      | TYPE; tidentdecl; <>
    )
  )

  | TYPE; x=tidentdecl; IS; li=annotated(pared(nclist(field_ns))); EOL;
    { AST.(D_TypeDecl (x, ASTUtils.add_pos_from li (T_Record li.desc))) }

let tidentdecl ==
  | ident
  | QUALIFIER; DOT; ident

let field == terminated(field_ns, SEMICOLON)
let field_ns == t=ty; x=ident; { (x, t) }

let bracketed(x) == delimited(LBRACK, x, RBRACK)
let braced(x) == delimited(LBRACE, x, RBRACE)
let pared(x) == delimited(LPAREN, x, RPAREN)
let clist(x) == { [] } | nclist(x)
let nlist(x) == nonempty_list(x)
let nclist(x) == separated_nonempty_list(COMMA, x)
let nnclist(x) == h=x; COMMA; t=nclist(x); { h::t }
let terminated_by (y, x) == terminated(x, y)
let some(x) == ~=x; < Some >

let unimplemented_ty(x) == x; { AST.T_Named "Unimplemented" }

let ty :=
  annotated (
    | ty_non_tuple
    | ~=pared(nnclist(ty)); < AST.T_Tuple >
  )

let ty_non_tuple ==
  | ~=tident; < AST.T_Named >
  | BITS; e=pared(expr); { AST.(T_Bits (AST.BitWidth_Determined e, None)) }
  (* | tident; pared(clist(expr)); <> *)
  | unimplemented_ty (
    | TYPEOF; pared(expr); <>
    | ARRAY; bracketed(ixtype); OF; ty; <>
  )

let ixtype ==
  | tident; <>
  | expr; DOT_DOT; expr; <>

let tident ==
  | typeident
  | QUALIFIER; DOT; typeident

let typeident == typeid
let typeid == ident
let ident == IDENTIFIER
let qualident == ioption(QUALIFIER; DOT); ident

let unimplemented_expr(x) == x; { AST.(E_Literal (V_Bool true)) }
let without_ta == { AST.TA_None }

let expr := binop_expr(expr, binop_or_concat)
let sexpr := binop_expr(sexpr, abinop)

let binop_expr(e, b) ==
  | pared(expr)
  | annotated (
      | ~=literal_expression;                 < AST.E_Literal   >
      | ~=qualident;                          < AST.E_Var       >
      | ~=qualident; ~=pared(clist(expr));    < AST.E_Call      >
      | ~=unop; ~=e;                          < AST.E_Unop      >
      | e1=e; op=b; e2=e;                     { AST.E_Binop (op, e1, e2) }
      | ~=pared(nnclist(expr));               < AST.E_Tuple     >
      | IF; c=expr; THEN; e=expr; ~=e_else;   < AST.E_Cond      >
      | ~=e; DOT; ~=ident; ~=without_ta;      < AST.E_GetField  >
      | ~=e; DOT; ~=bracketed(clist(ident));  < tr_get_fields   >
      | ~=e; ~=bracketed(clist(slice));       < AST.E_Slice     >
      (*
      | ~=e; LT; ~=clist(slice); GT;          < AST.E_Slice     >
      *)

      | unimplemented_expr(
        | ty_non_tuple; UNKNOWN; <>
        | ty_non_tuple; IMPLEM_DEFINED; ioption(STRING_LIT); <>
        | e; IN; pattern; <>
      )
    )

let e_elseif == annotated ( ELSIF; c=expr; THEN; e=expr; <> )
let e_else == ~=list(e_elseif); ELSE; ~=expr; < build_expr_conds >

let slice ==
  | ~=sexpr;                        < AST.Slice_Single >
  | e1=sexpr; COLON; e2=sexpr;      < AST.Slice_Range  >
  | e1=sexpr; PLUS_COLON; e2=sexpr; < AST.Slice_Length >

let unimplemented_literal_expression(x) == x; { AST.V_Bool false }

let literal_expression ==
  | ~=INT_LIT;       < AST.V_Int        >
  | ~=REAL_LIT;      < AST.V_Real       >
  | ~=BITS_LIT;      < AST.V_BitVector  >

  | unimplemented_literal_expression(
    | STRING_LIT
  )

let variable_decl ==
  terminated_by (SEMICOLON; EOL,
    | CONSTANT; t=ty; x=qualident; EQ; e=expr; { AST.D_GlobalConst (x, t, e) }

    | unimplemented_decl (
      | ty; qualident; <>
      | ARRAY; ty; qualident; bracketed(ixtype); <>
    )
  )

let function_decl ==
  return_type=some(ty); name=qualident; args=pared(clist(formal)); body=func_body;
    { AST.(D_Func { name; args; return_type; body }) }

let getter_decl ==
  | ~=ty; name=qualident; body=opt_indented_block; SEMICOLON; EOL;
    {
      let return_type = Some(ty)
      and args = [] in
      AST.(D_Func { name; args; return_type; body })
    }
  | return_type=some(ty); name=qualident; args=bracketed(clist(formal)); body=func_body;
    { AST.(D_Func { name; args; return_type; body })}

let setter_decl ==
  name=qualident; args=loption(bracketed(clist(sformal))); EQ; ~=ty; ~=ident; body=func_body;
    { AST.(D_Func { name; body; return_type=None; args=(ident, ty) :: args }) }

let procedure_decl ==
  name=qualident; args=pared(clist(formal)); body=func_body;
    { AST.(D_Func { name; args; body; return_type=None }) }

let sformal == t=ty; ioption(AMP); x=ident; { (x, t) }
let formal == t=ty; x=ident; { (x, t) }

let s_eol == EOL; { ASTUtils.s_pass }

let func_body ==
  | indented_block
  | ioption(SEMICOLON); s_eol

let opt_indented_block ==
  | indented_block
  | s_eol

let indented_block ==
  | EOL; INDENT; ~=nlist(stmts); DEDENT; < ASTUtils.stmt_from_list >

(* Always terminated by EOL or indented block. *)
let possibly_empty_block ==
  | indented_block
  | simple_stmts
  | s_eol

let unimplemented_stmts(x) == x; { AST.S_Pass }

let stmts ==
  | simple_stmts
  | compound_stmt

(* Always terminated by EOL *)
let simple_stmts ==
  | annotated (
    ~=simple_stmt_list; ~=simple_if_stmt; < AST.S_Then >
  )
  | terminated(simple_stmt_list, EOL)

let simple_stmt_list == ~=nlist(simple_stmt); < ASTUtils.stmt_from_list >

let simple_stmt ==
  | assignment_stmt
  | annotated ( terminated_by (SEMICOLON,
    | ~=qualident; ~=pared(clist(expr));  < AST.S_Call >
    | RETURN; ~=ioption(expr);            < AST.S_Return >
    | ASSERT; ~=expr;                     < AST.S_Assert >

    | unimplemented_stmts (
      | UNPREDICTABLE; ioption(pared(<>)); <>
      | CONSTRAINED_UNPRED; <>
      | IMPLEM_DEFINED; pared(ident); <>
      | UNDEFINED; ioption(pared(<>)); <>
      | IMPLEM_DEFINED; ioption(STRING_LIT); <>
      | SEE; pared(expr); <>
      | SEE; STRING_LIT; <>
      | SEE; ident; <>
      | THROW; ident; <>
    )
  ))

let assignment_stmt ==
  annotated (
    terminated_by(SEMICOLON,
      |               ~=           lexpr ; EQ; ~=expr; < AST.S_Assign >
      | ty_non_tuple; ~=annotated(le_var); EQ; ~=expr; < AST.S_Assign >
      | CONSTANT; ty; ~=annotated(le_var); EQ; ~=expr; < AST.S_Assign >

      | unimplemented_stmts (
        | ty_non_tuple; nclist(ident); <>
      )
    )
  )

let le_var == ~=qualident; < AST.LE_Var >
let lexpr_ignore == { AST.LE_Ignore }
let unimplemented_lexpr(x) == x; lexpr_ignore

let lexpr :=
  annotated (
    | MINUS; lexpr_ignore
    | le_var
    | ~=lexpr; DOT; ~=ident; ~=without_ta;  < AST.LE_SetField >
    | ~=lexpr; ~=bracketed(clist(slice));   < AST.LE_Slice    >
    | ~=lexpr; LT; ~=clist(slice); GT;      < AST.LE_Slice    >
    | ~=pared(nclist(lexpr));               < AST.LE_TupleUnpack >

    | unimplemented_lexpr (
      | lexpr; DOT; bracketed(clist(ident)); <>
      | bracketed(nclist(lexpr)); <>
    )
  )

let simple_if_stmt ==
    annotated (
      IF; ~=expr; THEN; ~=simple_stmt_list; ~=simple_else_opt; EOL; < AST.S_Cond >
    )

let simple_else_opt == ~=list(simple_elsif); ~=ioption(ELSE; simple_stmt_list); < build_stmt_conds >
let simple_else     == ~=list(simple_elsif); ~=   some(ELSE; simple_stmt_list); < build_stmt_conds >
let simple_elsif == annotated ( ELSIF; ~=expr; THEN; ~=simple_stmt_list; <> )

let compound_stmt ==
  annotated (
    | conditional_stmt
    |  unimplemented_stmts (
      | repetitive_stmt; <>
      | catch_stmt; <>
    )
  )

let conditional_stmt ==
  (* The first two cases of asl.ott are united in this simpler rule. *)
  | IF; ~=expr; THEN; ~=possibly_empty_block; ~=s_else;       < AST.S_Cond >
  | IF; ~=expr; THEN; ~=simple_stmt_list; ~=simple_else; EOL; < AST.S_Cond >
  | CASE; ~=expr; OF; EOL; INDENT; ~=alt_otherwise; DEDENT;   < AST.S_Case >

let s_elsif == annotated ( ELSIF; ~=expr; THEN; ~=possibly_empty_block; <> )
let s_else == ~=list(s_elsif); ~=ioption(ELSE; possibly_empty_block); < build_stmt_conds >

let alt ==
  annotated (
    | WHEN; ~=nclist(pattern); opt_altcond; ~=possibly_empty_block; <>
    | WHEN; ~=nclist(pattern); opt_altcond; ~=simple_if_stmt; <>
  )

let alt_otherwise ==
  | list(alt)
  | li=list(alt); ~=otherwise; { li @ [ ASTUtils.add_pos_from otherwise ([], otherwise.desc) ] }

let otherwise == annotated (OTHERWISE; possibly_empty_block)

let opt_altcond ==
  | <>
  | EQ_GT; <>
  | AND; expr; EQ_GT; <>

let pattern ==
  annotated (
    | ~=literal_expression; < AST.E_Literal >
    | ~=qualident;          < AST.E_Var     >

    | unimplemented_expr (
      | MASK_LIT; <>
      | MINUS; <>
      | pared(nclist(pattern)); <>
      | braced(nclist(apattern)); <>
    )
  )

let apattern == expr; <> | expr; DOT_DOT; expr; <>


let repetitive_stmt ==
  | FOR; ident; EQ; expr; direction; expr; indented_block; <>
  | WHILE; expr; DO; indented_block; <>
  | REPEAT; indented_block; UNTIL; expr; SEMICOLON; EOL; <>

let direction == TO | DOWNTO

let catch_stmt ==
  | TRY; indented_block; CATCH; ident; EOL; INDENT; nlist(catcher); ioption(otherwise); DEDENT; <>

let catcher == WHEN; expr; opt_indented_block; <>

let unop ==
  | BANG ;  { AST.BNOT }
  | MINUS ; { AST.NEG }
  | NOT ;   { AST.NOT }

let unimplemented_binop(x) == x ; { AST.PLUS }

let abinop ==
  | AND ;   { AST.AND }
  | AMP_AMP ;  { AST.BAND }
  | BAR_BAR ;   { AST.BOR }
  | DIV ;   { AST.DIV }
  | EOR ;   { AST.EOR }
  | EQ_EQ ; { AST.EQ_OP }
  | BANG_EQ ;   { AST.NEQ }
  | GT_EQ ;   { AST.GEQ }
  | LT_EQ ;   { AST.LEQ }
  | PLUS ;  { AST.PLUS }
  | MINUS ; { AST.MINUS }
  | MOD ;   { AST.MOD }
  | STAR ;  { AST.MUL }
  | OR ;    { AST.OR }
  | SLASH ;  { AST.RDIV }
  | LT_LT ;   { AST.SHL }
  | GT_GT ;   { AST.SHR }

  | unimplemented_binop(
    | CARET
  )

let binop ==
  | abinop
  | LT ;    { AST.LT }
  | GT ;    { AST.GT }

let binop_or_concat ==
  | binop
  | unimplemented_binop (
    | COLON
  )
