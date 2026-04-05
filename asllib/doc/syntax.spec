////////////////////////////////////////////////////////////////////////////////
// ASL parse tree definitions
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Tokens
////////////////////////////////////////////////////////////////////////////////

ast token
{
  "token",
  math_macro = \Token,
} =
    | Tok_func
    { "the token $\Tfunc$", math_macro = \Tfunc }
    | Tok_identifier(Identifier)
    { "the token $\Tidentifier$", math_macro = \Tidentifier }
    | Tok_accessor
    { "the token $\Taccessor$", math_macro = \Taccessor }
    | Tok_beq
    { "the token $\Tbeq$", math_macro = \Tbeq }
    | Tok_type
    { "the token $\Ttype$", math_macro = \Ttype }
    | Tok_of
    { "the token $\Tof$", math_macro = \Tof }
    | Tok_semicolon
    { "the token $\Tsemicolon$", math_macro = \Tsemicolon }
    | Tok_colon
    { "the token $\Tcolon$", math_macro = \Tcolon }
    | Tok_eq
    { "the token $\Teq$", math_macro = \Teq }
    | Tok_config
    { "the token $\Tconfig$", math_macro = \Tconfig }
    | Tok_var
    { "the token $\Tvar$", math_macro = \Tvar }
    | Tok_pragma
    { "the token $\Tpragma$", math_macro = \Tpragma }
    | Tok_recurselimit
    { "the token $\Trecurselimit$", math_macro = \Trecurselimit }
    | Tok_subtypes
    { "the token $\Tsubtypes$", math_macro = \Tsubtypes }
    | Tok_with
    { "the token $\Twith$", math_macro = \Twith }
    | Tok_arrow
    { "the token $\Tarrow$", math_macro = \Tarrow }
    | Tok_lbrace
    { "the token $\Tlbrace$", math_macro = \Tlbrace }
    | Tok_rbrace
    { "the token $\Trbrace$", math_macro = \Trbrace }
    | Tok_comma
    { "the token $\Tcomma$", math_macro = \Tcomma }
    | Tok_begin
    { "the token $\Tbegin$", math_macro = \Tbegin }
    | Tok_end
    { "the token $\Tend$", math_macro = \Tend }
    | Tok_minus
    { "the token $\Tminus$", math_macro = \Tminus }
    | Tok_getter
    { "the token $\Tgetter$", math_macro = \Tgetter }
    | Tok_setter
    { "the token $\Tsetter$", math_macro = \Tsetter }
    | Tok_pure
    { "the token $\Tpure$", math_macro = \Tpure }
    | Tok_readonly
    { "the token $\Treadonly$", math_macro = \Treadonly }
    | Tok_noreturn
    { "the token $\Tnoreturn$", math_macro = \Tnoreturn }
    | Tok_impdef
    { "the token $\Timpdef$", math_macro = \Timpdef }
    | Tok_implementation
    { "the token $\Timplementation$", math_macro = \Timplementation }
    | Tok_let
    { "the token $\Tlet$", math_macro = \Tlet }
    | Tok_constant
    { "the token $\Tconstant$", math_macro = \Tconstant }
    | Tok_to
    { "the token $\Tto$", math_macro = \Tto }
    | Tok_downto
    { "the token $\Tdownto$", math_macro = \Tdownto }
    | Tok_when
    { "the token $\Twhen$", math_macro = \Twhen }
    | Tok_where
    { "the token $\Twhere$", math_macro = \Twhere }
    | Tok_otherwise
    { "the token $\Totherwise$", math_macro = \Totherwise }
    | Tok_looplimit
    { "the token $\Tlooplimit$", math_macro = \Tlooplimit }
    | Tok_if
    { "the token $\Tif$", math_macro = \Tif }
    | Tok_then
    { "the token $\Tthen$", math_macro = \Tthen }
    | Tok_case
    { "the token $\Tcase$", math_macro = \Tcase }
    | Tok_while
    { "the token $\Twhile$", math_macro = \Twhile }
    | Tok_do
    { "the token $\Tdo$", math_macro = \Tdo }
    | Tok_for
    { "the token $\Tfor$", math_macro = \Tfor }
    | Tok_try
    { "the token $\Ttry$", math_macro = \Ttry }
    | Tok_catch
    { "the token $\Tcatch$", math_macro = \Tcatch }
    | Tok_pass
    { "the token $\Tpass$", math_macro = \Tpass }
    | Tok_return
    { "the token $\Treturn$", math_macro = \Treturn }
    | Tok_assert
    { "the token $\Tassert$", math_macro = \Tassert }
    | Tok_dot
    { "the token $\Tdot$", math_macro = \Tdot }
    | Tok_lbracket
    { "the token $\Tlbracket$", math_macro = \Tlbracket }
    | Tok_rbracket
    { "the token $\Trbracket$", math_macro = \Trbracket }
    | Tok_print
    { "the token $\Tprint$", math_macro = \Tprint }
    | Tok_println
    { "the token $\Tprintln$", math_macro = \Tprintln }
    | Tok_unreachable
    { "the token $\Tunreachable$", math_macro = \Tunreachable }
    | Tok_repeat
    { "the token $\Trepeat$", math_macro = \Trepeat }
    | Tok_until
    { "the token $\Tuntil$", math_macro = \Tuntil }
    | Tok_throw
    { "the token $\Tthrow$", math_macro = \Tthrow }
    | Tok_elseif
    { "the token $\Telseif$", math_macro = \Telseif }
    | Tok_else
    { "the token $\Telse$", math_macro = \Telse }
    | Tok_lpar
    { "the token $\Tlpar$", math_macro = \Tlpar }
    | Tok_rpar
    { "the token $\Trpar$", math_macro = \Trpar }
    | Tok_llbracket
    { "the token $\Tllbracket$", math_macro = \Tllbracket }
    | Tok_rrbracket
    { "the token $\Trrbracket$", math_macro = \Trrbracket }
    | Tok_slicing
    { "the token $\Tslicing$", math_macro = \Tslicing }
    | Tok_as
    { "the token $\Tas$", math_macro = \Tas }
    | Tok_in
    { "the token $\Tin$", math_macro = \Tin }
    | Tok_eqop
    { "the token $\Teqop$", math_macro = \Teqop }
    | Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit)))
    { "the token $\Tmasklit$", math_macro = \Tmasklit }
    | Tok_neq
    { "the token $\Tneq$", math_macro = \Tneq }
    | Tok_arbitrary
    { "the token $\Tarbitrary$", math_macro = \Tarbitrary }
    | Tok_bnot
    { "the token $\Tbnot$", math_macro = \Tbnot }
    | Tok_leq
    { "the token $\Tleq$", math_macro = \Tleq }
    | Tok_geq
    { "the token $\Tgeq$", math_macro = \Tgeq }
    | Tok_pluscolon
    { "the token $\Tpluscolon$", math_macro = \Tpluscolon }
    | Tok_starcolon
    { "the token $\Tstarcolon$", math_macro = \Tstarcolon }
    | Tok_integer
    { "the token $\Tinteger$", math_macro = \Tinteger }
    | Tok_real
    { "the token $\Treal$", math_macro = \Treal }
    | Tok_string
    { "the token $\Tstring$", math_macro = \Tstring }
    | Tok_boolean
    { "the token $\Tboolean$", math_macro = \Tboolean }
    | Tok_bit
    { "the token $\Tbit$", math_macro = \Tbit }
    | Tok_bits
    { "the token $\Tbits$", math_macro = \Tbits }
    | Tok_array
    { "the token $\Tarray$", math_macro = \Tarray }
    | Tok_enumeration
    { "the token $\Tenumeration$", math_macro = \Tenumeration }
    | Tok_record
    { "the token $\Trecord$", math_macro = \Trecord }
    | Tok_exception
    { "the token $\Texception$", math_macro = \Texception }
    | Tok_collection
    { "the token $\Tcollection$", math_macro = \Tcollection }
    | Tok_intlit(Z)
    { "the token $\Tintlit$", math_macro = \Tintlit }
    | Tok_boollit(Bool)
    { "the token $\Tboollit$", math_macro = \Tboollit }
    | Tok_reallit(Q)
    { "the token $\Treallit$", math_macro = \Treallit }
    | Tok_bitvectorlit(list0(Bit))
    { "the token $\Tbitvectorlit$", math_macro = \Tbitvectorlit }
    | Tok_stringlit(Strings)
    { "the token $\Tstringlit$", math_macro = \Tstringlit }
    | Tok_not
    { "the token $\Tnot$", math_macro = \Tnot }
    | Tok_and
    { "the token $\Tand$", math_macro = \Tand }
    | Tok_band
    { "the token $\Tband$", math_macro = \Tband }
    | Tok_bor
    { "the token $\Tbor$", math_macro = \Tbor }
    | Tok_div
    { "the token $\Tdiv$", math_macro = \Tdiv }
    | Tok_divrm
    { "the token $\Tdivrm$", math_macro = \Tdivrm }
    | Tok_xor
    { "the token $\Txor$", math_macro = \Txor }
    | Tok_gt
    { "the token $\Tgt$", math_macro = \Tgt }
    | Tok_bimpl
    { "the token $\Tbimpl$", math_macro = \Tbimpl }
    | Tok_lt
    { "the token $\Tlt$", math_macro = \Tlt }
    | Tok_plus
    { "the token $\Tplus$", math_macro = \Tplus }
    | Tok_mod
    { "the token $\Tmod$", math_macro = \Tmod }
    | Tok_mul
    { "the token $\Tmul$", math_macro = \Tmul }
    | Tok_or
    { "the token $\Tor$", math_macro = \Tor }
    | Tok_rdiv
    { "the token $\Trdiv$", math_macro = \Trdiv }
    | Tok_shl
    { "the token $\Tshl$", math_macro = \Tshl }
    | Tok_shr
    { "the token $\Tshr$", math_macro = \Tshr }
    | Tok_pow
    { "the token $\Tpow$", math_macro = \Tpow }
    | Tok_coloncolon
    { "the token $\Tcoloncolon$", math_macro = \Tcoloncolon }
    | Tok_plusplus
    { "the token $\Tplusplus$", math_macro = \Tplusplus }
;

typedef epsilon
{
  "epsilon",
  math_macro = \emptysentence,
};

////////////////////////////////////////////////////////////////////////////////
// Non-terminals
////////////////////////////////////////////////////////////////////////////////

ast N_spec
{
  "non-terminal $\Nspec$",
  math_macro = \Nspec,
} =
    ND_Spec_One(/*maybeemptylist*/ list0(N_decl))
    { math_macro = \dummyrulelabel }
;

ast N_decl
{
  "non-terminal $\Ndecl$",
  math_macro = \Ndecl,
} =
    | ND_Decl_One(N_purity_keyword, N_override, Tok_func, Tok_identifier(Identifier), N_params_opt, N_func_args, N_return_type, N_recurse_limit, N_func_body)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Two(N_qualifier, N_override, Tok_func, Tok_identifier(Identifier), N_params_opt, N_func_args, N_recurse_limit, N_func_body)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Three(N_override, Tok_accessor, Tok_identifier(Identifier), N_params_opt, N_func_args, Tok_beq, Tok_identifier(Identifier), N_as_ty, N_accessor_body)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Four(Tok_type, Tok_identifier(Identifier), Tok_of, N_ty_decl, N_subtype_opt, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Five(Tok_type, Tok_identifier(Identifier), N_subtype, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Six(N_global_decl_keyword_non_config, Tok_identifier(Identifier), /*option*/ option((Tok_colon, N_ty)), Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Seven(Tok_config, Tok_identifier(Identifier), Tok_colon, N_ty, Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Eight(Tok_var, Tok_identifier(Identifier), Tok_colon, N_ty_or_collection, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Nine(Tok_var, /*clist2*/ list1(Tok_identifier(Identifier)), N_as_ty, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Ten(N_global_decl_keyword, Tok_identifier(Identifier), Tok_colon, N_ty, Tok_eq, N_elided_param_call, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Decl_Eleven(Tok_pragma, Tok_identifier(Identifier), /*clist0*/ list0(N_expr), Tok_semicolon)
    { math_macro = \dummyrulelabel }
;

ast N_recurse_limit
{
  "non-terminal $\Nrecurselimit$",
  math_macro = \Nrecurselimit,
} =
    | ND_Recurse_Limit_One(Tok_recurselimit, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Recurse_Limit_Two(epsilon)
    { math_macro = \dummyrulelabel }
;

ast N_subtype
{
  "non-terminal $\Nsubtype$",
  math_macro = \Nsubtype,
} =
    | ND_Subtype_One(Tok_subtypes, Tok_identifier(Identifier), Tok_with, N_fields)
    { math_macro = \dummyrulelabel }
    | ND_Subtype_Two(Tok_subtypes, Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
;

ast N_subtype_opt
{
  "non-terminal $\Nsubtypeopt$",
  math_macro = \Nsubtypeopt,
} =
    ND_Subtype_Opt_One(/*option*/ option(N_subtype))
    { math_macro = \dummyrulelabel }
;

ast N_typed_identifier
{
  "non-terminal $\Ntypedidentifier$",
  math_macro = \Ntypedidentifier,
} =
    ND_Typed_Identifier_One(Tok_identifier(Identifier), N_as_ty)
    { math_macro = \dummyrulelabel }
;

ast N_opt_typed_identifier
{
  "non-terminal $\Nopttypedidentifier$",
  math_macro = \Nopttypedidentifier,
} =
    ND_Opt_Typed_Identifier_One(Tok_identifier(Identifier), /*option*/ option(N_as_ty))
    { math_macro = \dummyrulelabel }
;

ast N_as_ty
{
  "non-terminal $\Nasty$",
  math_macro = \Nasty,
} =
    ND_As_Ty_One(Tok_colon, N_ty)
    { math_macro = \dummyrulelabel }
;

ast N_return_type
{
  "non-terminal $\Nreturntype$",
  math_macro = \Nreturntype,
} =
    ND_Return_Type_One(Tok_arrow, N_ty)
    { math_macro = \dummyrulelabel }
;

ast N_params_opt
{
  "non-terminal $\Nparamsopt$",
  math_macro = \Nparamsopt,
} =
    | ND_Params_Opt_One(epsilon)
    { math_macro = \dummyrulelabel }
    | ND_Params_Opt_Two(Tok_lbrace, /*clist0*/ list0(N_opt_typed_identifier), Tok_rbrace)
    { math_macro = \dummyrulelabel }
;

ast N_call
{
  "non-terminal $\Ncall$",
  math_macro = \Ncall,
} =
    | ND_Call_One(Tok_identifier(Identifier), /*plist0*/ list0(N_expr))
    { math_macro = \dummyrulelabel }
    | ND_Call_Two(Tok_identifier(Identifier), Tok_lbrace, /*clist1*/ list1(N_expr), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Call_Three(Tok_identifier(Identifier), Tok_lbrace, /*clist1*/ list1(N_expr), Tok_rbrace, /*plist0*/ list0(N_expr))
    { math_macro = \dummyrulelabel }
;

ast N_elided_param_call
{
  "non-terminal $\Nelidedparamcall$",
  math_macro = \Nelidedparamcall,
} =
    | ND_Elided_Param_Call_One(Tok_identifier(Identifier), Tok_lbrace, Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Elided_Param_Call_Two(Tok_identifier(Identifier), Tok_lbrace, Tok_rbrace, /*plist0*/ list0(N_expr))
    { math_macro = \dummyrulelabel }
    | ND_Elided_Param_Call_Three(Tok_identifier(Identifier), Tok_lbrace, Tok_comma, /*tclist1*/ list1(N_expr), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Elided_Param_Call_Four(Tok_identifier(Identifier), Tok_lbrace, Tok_comma, /*tclist1*/ list1(N_expr), Tok_rbrace, /*plist0*/ list0(N_expr))
    { math_macro = \dummyrulelabel }
;

ast N_func_args
{
  "non-terminal $\Nfuncargs$",
  math_macro = \Nfuncargs,
} =
    ND_Func_Args_One(/*plist0*/ list0(N_typed_identifier))
    { math_macro = \dummyrulelabel }
;

ast N_func_body
{
  "non-terminal $\Nfuncbody$",
  math_macro = \Nfuncbody,
} =
    ND_Func_Body_One(Tok_begin, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
;

ast N_ignored_or_identifier
{
  "non-terminal $\Nignoredoridentifier$",
  math_macro = \Nignoredoridentifier,
} =
    | (Tok_minus)
    | (Tok_identifier(Identifier))
;

ast N_accessor_body
{
  "non-terminal $\Naccessorbody$",
  math_macro = \Naccessorbody,
} =
    ND_Accessor_Body_One(Tok_begin, N_accessors, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
;

ast N_accessors
{
  "non-terminal $\Naccessors$",
  math_macro = \Naccessors,
} =
    | ND_Accessors_One(N_is_readonly, Tok_getter, N_stmt_list, Tok_end, Tok_semicolon, Tok_setter, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Accessors_Two(Tok_setter, N_stmt_list, Tok_end, Tok_semicolon, N_is_readonly, Tok_getter, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
;

ast N_qualifier
{
  "non-terminal $\Nqualifier$",
  math_macro = \Nqualifier,
} =
    | (epsilon)
    | (Tok_pure)
    | (Tok_readonly)
    | (Tok_noreturn)
;

ast N_purity_keyword
{
  "non-terminal $\Npuritykeyword$",
  math_macro = \Npuritykeyword,
} =
    | (epsilon)
    | (Tok_pure)
    | (Tok_readonly)
;

ast N_is_readonly
{
  "non-terminal $\Nisreadonly$",
  math_macro = \Nisreadonly,
} =
    | (epsilon)
    | (Tok_readonly)
;

ast N_override
{
  "non-terminal $\Noverride$",
  math_macro = \Noverride,
} =
    | (epsilon)
    | (Tok_impdef)
    | (Tok_implementation)
;

ast N_local_decl_keyword
{
  "non-terminal $\Nlocaldeclkeyword$",
  math_macro = \Nlocaldeclkeyword,
} =
    | (Tok_let)
    | (Tok_constant)
    | (Tok_var)
;

ast N_global_decl_keyword_non_config
{
  "non-terminal $\Nglobaldeclkeywordnonconfig$",
  math_macro = \Nglobaldeclkeywordnonconfig,
} =
    | (Tok_let)
    | (Tok_constant)
    | (Tok_var)
;

ast N_global_decl_keyword
{
  "non-terminal $\Nglobaldeclkeyword$",
  math_macro = \Nglobaldeclkeyword,
} =
    | ND_Global_Decl_Keyword_One(N_global_decl_keyword_non_config)
    { math_macro = \dummyrulelabel }
    | ND_Global_Decl_Keyword_Two(Tok_config)
    { math_macro = \dummyrulelabel }
;

ast N_direction
{
  "non-terminal $\Ndirection$",
  math_macro = \Ndirection,
} =
    | (Tok_to)
    | (Tok_downto)
;

ast N_case_alt_list
{
  "non-terminal $\Ncasealtlist$",
  math_macro = \Ncasealtlist,
} =
    ND_Case_Alt_List_One(list1(N_case_alt))
    { math_macro = \dummyrulelabel }
;

ast N_case_alt
{
  "non-terminal $\Ncasealt$",
  math_macro = \Ncasealt,
} =
    ND_Case_Alt_One(Tok_when, N_pattern_list, /*option*/ option((Tok_where, N_expr)), Tok_arrow, N_stmt_list)
    { math_macro = \dummyrulelabel }
;

ast N_otherwise_opt
{
  "non-terminal $\Notherwiseopt$",
  math_macro = \Notherwiseopt,
} =
    | ND_Otherwise_Opt_One(Tok_otherwise, Tok_arrow, N_stmt_list)
    { math_macro = \dummyrulelabel }
    | ND_Otherwise_Opt_Two(epsilon)
    { math_macro = \dummyrulelabel }
;

ast N_catcher
{
  "non-terminal $\Ncatcher$",
  math_macro = \Ncatcher,
} =
    | ND_Catcher_One(Tok_when, Tok_identifier(Identifier), Tok_colon, N_ty, Tok_arrow, N_stmt_list)
    { math_macro = \dummyrulelabel }
    | ND_Catcher_Two(Tok_when, N_ty, Tok_arrow, N_stmt_list)
    { math_macro = \dummyrulelabel }
;

ast N_loop_limit
{
  "non-terminal $\Nlooplimit$",
  math_macro = \Nlooplimit,
} =
    | ND_Loop_Limit_One(Tok_looplimit, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Loop_Limit_Two(epsilon)
    { math_macro = \dummyrulelabel }
;

ast N_stmt
{
  "non-terminal $\Nstmt$",
  math_macro = \Nstmt,
} =
    | ND_Stmt_One(Tok_if, N_expr, Tok_then, N_stmt_list, N_s_else, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Two(Tok_case, N_expr, Tok_of, N_case_alt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Three(Tok_case, N_expr, Tok_of, N_case_alt_list, Tok_otherwise, Tok_arrow, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Four(Tok_while, N_expr, N_loop_limit, Tok_do, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Five(Tok_for, Tok_identifier(Identifier), Tok_eq, N_expr, N_direction, N_expr, N_loop_limit, Tok_do, N_stmt_list, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Six(Tok_try, N_stmt_list, Tok_catch, /*list1*/ list1(N_catcher), N_otherwise_opt, Tok_end, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Seven(Tok_pass, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Eight(Tok_return, /*option*/ option(N_expr), Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Nine(N_call, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Ten(Tok_assert, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Eleven(N_local_decl_keyword, N_decl_item, /*option*/ option(N_as_ty), Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Twelve(N_lexpr, Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Thirteen(N_call, N_setter_access, Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Fourteen(N_call, N_setter_access, N_slices, Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Fifteen(N_call, Tok_dot, Tok_lbracket, /*clist1*/ list1(Tok_identifier(Identifier)), Tok_rbracket, Tok_eq, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Sixteen(N_local_decl_keyword, N_decl_item, N_as_ty, Tok_eq, N_elided_param_call, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Seventeen(Tok_var, N_decl_item, N_as_ty, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Eighteen(Tok_var, /*clist2*/ list1(Tok_identifier(Identifier)), N_as_ty, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Nineteen(Tok_print, /*plist0*/ list0(N_expr), Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_Twenty(Tok_println, /*plist0*/ list0(N_expr), Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_TwentyOne(Tok_unreachable, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_TwentyTwo(Tok_repeat, N_stmt_list, Tok_until, N_expr, N_loop_limit, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_TwentyThree(Tok_throw, N_expr, Tok_semicolon)
    { math_macro = \dummyrulelabel }
    | ND_Stmt_TwentyFour(Tok_pragma, Tok_identifier(Identifier), list0(N_expr), Tok_semicolon)
    { math_macro = \dummyrulelabel }
;

ast N_stmt_list
{
  "non-terminal $\Nstmtlist$",
  math_macro = \Nstmtlist,
} =
    ND_Stmt_List_One(list1(N_stmt))
    { math_macro = \dummyrulelabel }
;

ast N_s_else
{
  "non-terminal $\Nselse$",
  math_macro = \Nselse,
} =
    | ND_S_Else_One(Tok_elseif, N_expr, Tok_then, N_stmt_list, N_s_else)
    { math_macro = \dummyrulelabel }
    | ND_S_Else_Two(Tok_else, N_stmt_list)
    { math_macro = \dummyrulelabel }
    | ND_S_Else_Three(epsilon)
    { math_macro = \dummyrulelabel }
;

ast N_lexpr
{
  "non-terminal $\Nlexpr$",
  math_macro = \Nlexpr,
} =
    | ND_Lexpr_One(Tok_minus)
    { math_macro = \dummyrulelabel }
    | ND_Lexpr_Two(N_basic_lexpr)
    { math_macro = \dummyrulelabel }
    | ND_Lexpr_Three(Tok_lpar, /*plist2*/ list1(N_discard_or_basic_lexpr), Tok_rpar)
    { math_macro = \dummyrulelabel }
    | ND_Lexpr_Four(Tok_identifier(Identifier), Tok_dot, Tok_lbracket, /*clist1*/ list1(Tok_identifier(Identifier)), Tok_rbracket)
    { math_macro = \dummyrulelabel }
    | ND_Lexpr_Five(Tok_identifier(Identifier), Tok_dot, Tok_lpar, /*plist2*/ list1(N_discard_or_identifier), Tok_rpar)
    { math_macro = \dummyrulelabel }
;

ast N_basic_lexpr
{
  "non-terminal $\Nbasiclexpr$",
  math_macro = \Nbasiclexpr,
} =
    | ND_Basic_Lexpr_One(Tok_identifier(Identifier), N_access)
    { math_macro = \dummyrulelabel }
    | ND_Basic_Lexpr_Two(Tok_identifier(Identifier), N_access, N_slices)
    { math_macro = \dummyrulelabel }
;

ast N_access
{
  "non-terminal $\Naccess$",
  math_macro = \Naccess,
} =
    | ND_Access_One(epsilon)
    { math_macro = \dummyrulelabel }
    | ND_Access_Two(Tok_dot, Tok_identifier(Identifier), N_access)
    { math_macro = \dummyrulelabel }
    | ND_Access_Three(Tok_llbracket, N_expr, Tok_rrbracket, N_access)
    { math_macro = \dummyrulelabel }
;

ast N_discard_or_basic_lexpr
{
  "non-terminal $\Ndiscardorbasiclexpr$",
  math_macro = \Ndiscardorbasiclexpr,
} =
    | ND_Discard_Or_Basic_Lexpr_One(Tok_minus)
    { math_macro = \dummyrulelabel }
    | ND_Discard_Or_Basic_Lexpr_Two(N_basic_lexpr)
    { math_macro = \dummyrulelabel }
;

ast N_discard_or_identifier
{
  "non-terminal $\Ndiscardoridentifier$",
  math_macro = \Ndiscardoridentifier,
} =
    | (Tok_minus)
    | (Tok_identifier(Identifier))
;

ast N_setter_access
{
  "non-terminal $\Nsetteraccess$",
  math_macro = \Nsetteraccess,
} =
    | ND_Setter_Access_One(epsilon)
    { math_macro = \dummyrulelabel }
    | ND_Setter_Access_Two(Tok_dot, Tok_identifier(Identifier), N_setter_access)
    { math_macro = \dummyrulelabel }
;

ast N_decl_item
{
  "non-terminal $\Ndeclitem$",
  math_macro = \Ndeclitem,
} =
    | ND_Decl_Item_One(Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Decl_Item_Two(/*clist2*/ list1(N_ignored_or_identifier))
    { math_macro = \dummyrulelabel }
;

ast N_constraint_kind_opt
{
  "non-terminal $\Nconstraintkindopt$",
  math_macro = \Nconstraintkindopt,
} =
    | ND_Constraint_Kind_Opt_One(N_constraint_kind)
    { math_macro = \dummyrulelabel }
    | ND_Constraint_Kind_Opt_Two(epsilon)
    { math_macro = \dummyrulelabel }
;

ast N_constraint_kind
{
  "non-terminal $\Nconstraintkind$",
  math_macro = \Nconstraintkind,
} =
    | ND_Constraint_Kind_One(Tok_lbrace, /*clist1*/ list1(N_int_constraint), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Constraint_Kind_Two(Tok_lbrace, Tok_rbrace)
    { math_macro = \dummyrulelabel }
;

ast N_int_constraint
{
  "non-terminal $\Nintconstraint$",
  math_macro = \Nintconstraint,
} =
    | ND_Int_Constraint_One(N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Int_Constraint_Two(N_expr, Tok_slicing, N_expr)
    { math_macro = \dummyrulelabel }
;

ast N_expr_pattern
{
  "non-terminal $\Nexprpattern$",
  math_macro = \Nexprpattern,
} =
    | ND_Expr_Pattern_One(N_value)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Two(Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Three(N_expr_pattern, N_binop, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Four(N_unop, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Five(Tok_if, N_expr, Tok_then, N_expr, Tok_else, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Six(N_call)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Seven(N_expr_pattern, N_slices)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Eight(N_expr_pattern, Tok_llbracket, N_expr, Tok_rrbracket)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Nine(N_expr_pattern, Tok_dot, Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Ten(N_expr_pattern, Tok_dot, Tok_lbracket, /*clist1*/ list1(Tok_identifier(Identifier)), Tok_rbracket)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Eleven(N_expr_pattern, Tok_as, N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Twelve(N_expr_pattern, Tok_as, N_constraint_kind)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Thirteen(N_expr, Tok_in, N_pattern_set)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Fourteen(N_expr, Tok_eqop, Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Fifteen(N_expr, Tok_neq, Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Sixteen(Tok_arbitrary, Tok_colon, N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Seventeen(Tok_identifier(Identifier), Tok_lbrace, Tok_minus, Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Eighteen(Tok_identifier(Identifier), Tok_lbrace, /*clist1*/ list1(N_field_assign), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Pattern_Nineteen(Tok_lpar, N_expr_pattern, Tok_rpar)
    { math_macro = \dummyrulelabel }
;

ast N_pattern_set
{
  "non-terminal $\Npatternset$",
  math_macro = \Npatternset,
} =
    | ND_Pattern_Set_One(Tok_bnot, Tok_lbrace, N_pattern_list, Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Set_Two(Tok_lbrace, N_pattern_list, Tok_rbrace)
    { math_macro = \dummyrulelabel }
;

ast N_pattern_list
{
  "non-terminal $\Npatternlist$",
  math_macro = \Npatternlist,
} =
    ND_Pattern_List_One(/*plist2*/ list1(N_pattern))
    { math_macro = \dummyrulelabel }
;

ast N_pattern
{
  "non-terminal $\Npattern$",
  math_macro = \Npattern,
} =
    | ND_Pattern_One(N_expr_pattern)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Two(N_expr_pattern, Tok_slicing, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Three(Tok_minus)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Four(Tok_leq, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Five(Tok_geq, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Six(Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Seven(/*plist2*/ list1(N_pattern))
    { math_macro = \dummyrulelabel }
    | ND_Pattern_Eight(N_pattern_set)
    { math_macro = \dummyrulelabel }
;

ast N_fields
{
  "non-terminal $\Nfields$",
  math_macro = \Nfields,
} =
    | ND_Fields_One(Tok_lbrace, Tok_minus, Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Fields_Two(Tok_lbrace, /*tclist1*/ list1(N_typed_identifier), Tok_rbrace)
    { math_macro = \dummyrulelabel }
;

ast N_slices
{
  "non-terminal $\Nslices$",
  math_macro = \Nslices,
} =
    ND_Slices_One(Tok_lbracket, /*clist1*/ list1(N_slice), Tok_rbracket)
    { math_macro = \dummyrulelabel }
;

ast N_slice
{
  "non-terminal $\Nslice$",
  math_macro = \Nslice,
} =
    | ND_Slice_One(N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Slice_Two(N_expr, Tok_colon, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Slice_Three(N_expr, Tok_pluscolon, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Slice_Four(N_expr, Tok_starcolon, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Slice_Five(Tok_colon, N_expr)
    { math_macro = \dummyrulelabel }
;

ast N_bitfields
{
  "non-terminal $\Nbitfields$",
  math_macro = \Nbitfields,
} =
    ND_Bitfields_One(Tok_lbrace, /*tclist0*/ list0(N_bitfield), Tok_rbrace)
    { math_macro = \dummyrulelabel }
;

ast N_bitfield
{
  "non-terminal $\Nbitfield$",
  math_macro = \Nbitfield,
} =
    | ND_Bitfield_One(N_slices, Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Bitfield_Two(N_slices, Tok_identifier(Identifier), N_bitfields)
    { math_macro = \dummyrulelabel }
    | ND_Bitfield_Three(N_slices, Tok_identifier(Identifier), Tok_colon, N_ty)
    { math_macro = \dummyrulelabel }
;

ast N_ty
{
  "non-terminal $\Nty$",
  math_macro = \Nty,
} =
    | ND_Ty_One(Tok_integer, N_constraint_kind_opt)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Two(Tok_real)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Three(Tok_string)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Four(Tok_boolean)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Five(Tok_bit)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Six(Tok_bits, Tok_lpar, N_expr, Tok_rpar, /*option*/ option(N_bitfields))
    { math_macro = \dummyrulelabel }
    | ND_Ty_Seven(Tok_lpar, N_ty, Tok_rpar)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Eight(/*plist2*/ list1(N_ty))
    { math_macro = \dummyrulelabel }
    | ND_Ty_Nine(Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Ty_Ten(Tok_array, Tok_llbracket, N_expr, Tok_rrbracket, Tok_of, N_ty)
    { math_macro = \dummyrulelabel }
;

ast N_ty_decl
{
  "non-terminal $\Ntydecl$",
  math_macro = \Ntydecl,
} =
    | ND_Ty_Decl_One(N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Decl_Two(Tok_enumeration, Tok_lbrace, /*clist1*/ list1(Tok_identifier(Identifier)), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Decl_Three(Tok_record, N_fields)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Decl_Four(Tok_exception, N_fields)
    { math_macro = \dummyrulelabel }
;

ast N_field_assign
{
  "non-terminal $\Nfieldassign$",
  math_macro = \Nfieldassign,
} =
    ND_Field_Assign_One(Tok_identifier(Identifier), Tok_eq, N_expr)
    { math_macro = \dummyrulelabel }
;

ast N_ty_or_collection
{
  "non-terminal $\Ntyorcollection$",
  math_macro = \Ntyorcollection,
} =
    | ND_Ty_Or_Collection_One(N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Ty_Or_Collection_Two(Tok_collection, N_fields)
    { math_macro = \dummyrulelabel }
;

ast N_expr
{
  "non-terminal $\Nexpr$",
  math_macro = \Nexpr,
} =
    | ND_Expr_One(N_value)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Two(Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Three(N_expr, N_binop, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Four(N_unop, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Five(Tok_if, N_expr, Tok_then, N_expr, Tok_else, N_expr)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Six(N_call)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Seven(N_expr, N_slices)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Eight(N_expr, Tok_llbracket, N_expr, Tok_rrbracket)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Nine(N_expr, Tok_dot, Tok_identifier(Identifier))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Ten(N_expr, Tok_dot, Tok_lbracket, /*clist1*/ list1(Tok_identifier(Identifier)), Tok_rbracket)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Eleven(N_expr, Tok_as, N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Twelve(N_expr, Tok_as, N_constraint_kind)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Thirteen(N_expr, Tok_in, N_pattern_set)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Fourteen(N_expr, Tok_eqop, Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Fifteen(N_expr, Tok_neq, Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    { math_macro = \dummyrulelabel }
    | ND_Expr_Sixteen(Tok_arbitrary, Tok_colon, N_ty)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Seventeen(Tok_identifier(Identifier), Tok_lbrace, Tok_minus, Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Eighteen(Tok_identifier(Identifier), Tok_lbrace, /*clist1*/ list1(N_field_assign), Tok_rbrace)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Nineteen(Tok_lpar, N_expr, Tok_rpar)
    { math_macro = \dummyrulelabel }
    | ND_Expr_Twenty(/*plist2*/ list1(N_expr))
    { math_macro = \dummyrulelabel }
;

ast N_value
{
  "non-terminal $\Nvalue$",
  math_macro = \Nvalue,
} =
    | (Tok_intlit(Z))
    | (Tok_boollit(Bool))
    | (Tok_reallit(Q))
    | (Tok_bitvectorlit(list0(Bit)))
    | (Tok_stringlit(Strings))
;

ast N_unop
{
  "non-terminal $\Nunop$",
  math_macro = \Nunop,
} =
    | (Tok_bnot)
    | (Tok_minus)
    | (Tok_not)
;

ast N_binop
{
  "non-terminal $\Nbinop$",
  math_macro = \Nbinop,
} =
    | (Tok_and)
    | (Tok_band)
    | (Tok_bor)
    | (Tok_beq)
    | (Tok_div)
    | (Tok_divrm)
    | (Tok_xor)
    | (Tok_eqop)
    | (Tok_neq)
    | (Tok_gt)
    | (Tok_geq)
    | (Tok_bimpl)
    | (Tok_lt)
    | (Tok_leq)
    | (Tok_plus)
    | (Tok_minus)
    | (Tok_mod)
    | (Tok_mul)
    | (Tok_or)
    | (Tok_rdiv)
    | (Tok_shl)
    | (Tok_shr)
    | (Tok_pow)
    | (Tok_coloncolon)
    | (Tok_plusplus)
;
