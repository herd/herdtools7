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
    | Tok_identifier(id: Identifier)
    { "the token $\Tidentifier$ for identifier {id}", math_macro = \Tidentifier }
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

constant epsilon_sentence : epsilon
{
  "epsilon",
  math_macro = \emptysentence,
};

////////////////////////////////////////////////////////////////////////////////
// Parameterized nonterminals
////////////////////////////////////////////////////////////////////////////////

ast p_option[[x]]
  {
    "an optional symbol",
    math_macro = \option,
  } =
  | (epsilon)
  | x
;

ast clistone[[x]]
  { "a non-empty comma-separated list",
  } =
  | x
  | (x, Tok_comma, clistone[[x]])
;

ast clistzero[[x]]
  { "a possibly empty comma-separated list" }=
  | (epsilon)
  | clistone[[x]]
;

ast clisttwo[[x]]
  { "a comma-separated list with at least two elements" } =
  (x, Tok_comma, clistone[[x]]);

ast tclistone[[x]]
  { "a non-empty trailing comma-separated list" } =
  | x
  | (x, p_option[[Tok_comma]], clistone[[x]])
;

ast tclistzero[[x]]
  { "a possibly empty trailing comma-separated list" } =
  | (epsilon)
  | tclistone[[x]]
;

ast plistzero[[x]]
  { "a possibly empty parenthesized list" } =
  (Tok_lpar, clistzero[[x]], Tok_rpar);

ast plisttwo[[x]]
  { "a parenthesized list with at least two elements" } =
  (Tok_lpar, clisttwo[[x]], Tok_rpar);

////////////////////////////////////////////////////////////////////////////////
// Non-terminals
////////////////////////////////////////////////////////////////////////////////

ast N_spec
{
  "non-terminal $\Nspec$",
  math_macro = \Nspec,
} =
  (decls: list0(N_decl))
  {
    "the specification parse node with subnodes {decls}"
  }
;

ast N_decl
{
  "non-terminal $\Ndecl$",
  math_macro = \Ndecl,
} =
    | ND_Decl_One(subnode_one: N_purity_keyword, subnode_two: N_override, subnode_three: Tok_func, subnode_four: Tok_identifier(Identifier), subnode_five: N_params_opt, subnode_six: N_func_args, subnode_seven: N_return_type, subnode_eight: N_recurse_limit, subnode_nine: N_func_body)
    {
      "the parse node with the following subnodes: the $\Npuritykeyword$ non-terminal {subnode_one}, the $\Noverride$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, the $\Nparamsopt$ non-terminal {subnode_five}, the $\Nfuncargs$ non-terminal {subnode_six}, the $\Nreturntype$ non-terminal {subnode_seven}, the $\Nrecurselimit$ non-terminal {subnode_eight}, and the $\Nfuncbody$ non-terminal {subnode_nine}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Two(subnode_one: N_qualifier, subnode_two: N_override, subnode_three: Tok_func, subnode_four: Tok_identifier(Identifier), subnode_five: N_params_opt, subnode_six: N_func_args, subnode_seven: N_recurse_limit, subnode_eight: N_func_body)
    {
      "the parse node with the following subnodes: the $\Nqualifier$ non-terminal {subnode_one}, the $\Noverride$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, the $\Nparamsopt$ non-terminal {subnode_five}, the $\Nfuncargs$ non-terminal {subnode_six}, the $\Nrecurselimit$ non-terminal {subnode_seven}, and the $\Nfuncbody$ non-terminal {subnode_eight}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Three(subnode_one: N_override, subnode_two: Tok_accessor, subnode_three: Tok_identifier(Identifier), subnode_four: N_params_opt, subnode_five: N_func_args, subnode_six: Tok_beq, subnode_seven: Tok_identifier(Identifier), subnode_eight: N_as_ty, subnode_nine: N_accessor_body)
    {
      "the parse node with the following subnodes: the $\Noverride$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, the $\Nparamsopt$ non-terminal {subnode_four}, the $\Nfuncargs$ non-terminal {subnode_five}, {subnode_six}, {subnode_seven}, the $\Nasty$ non-terminal {subnode_eight}, and the $\Naccessorbody$ non-terminal {subnode_nine}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Four(subnode_one: Tok_type, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_of, subnode_four: N_ty_decl, subnode_five: N_subtype_opt, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, the $\Ntydecl$ non-terminal {subnode_four}, the $\Nsubtypeopt$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Five(subnode_one: Tok_type, subnode_two: Tok_identifier(Identifier), subnode_three: N_subtype, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Nsubtype$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Six(subnode_one: N_global_decl_keyword_non_config, subnode_two: N_ignored_or_identifier, subnode_three: p_option[[N_as_ty]], subnode_four: Tok_eq, subnode_five: N_expr, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nglobaldeclkeywordnonconfig$ non-terminal {subnode_one}, the $\Nignoredoridentifier$ non-terminal {subnode_two}, the $\Nasty$ non-terminal {subnode_three}, {subnode_four}, the $\Nexpr$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Seven(subnode_one: Tok_config, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_colon, subnode_four: N_ty, subnode_five: Tok_eq, subnode_six: N_expr, subnode_seven: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, the $\Nty$ non-terminal {subnode_four}, {subnode_five}, the $\Nexpr$ non-terminal {subnode_six}, and {subnode_seven}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Eight(subnode_one: Tok_var, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_colon, subnode_four: N_ty_or_collection, subnode_five: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, the $\Ntyorcollection$ non-terminal {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Nine(subnode_one: Tok_var, subnode_two: clisttwo[[Tok_identifier(Identifier)]], subnode_three: N_as_ty, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Nasty$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Ten(subnode_one: N_global_decl_keyword, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_colon, subnode_four: N_ty, subnode_five: Tok_eq, subnode_six: N_elided_param_call, subnode_seven: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nglobaldeclkeyword$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, the $\Nty$ non-terminal {subnode_four}, {subnode_five}, the $\Nelidedparamcall$ non-terminal {subnode_six}, and {subnode_seven}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Eleven(subnode_one: Tok_pragma, subnode_two: Tok_identifier(Identifier), subnode_three: clistzero[[N_expr]], subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
;

ast N_recurse_limit
{
  "non-terminal $\Nrecurselimit$",
  math_macro = \Nrecurselimit,
} =
    | ND_Recurse_Limit_One(subnode_one: Tok_recurselimit, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Recurse_Limit_Two(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_subtype
{
  "non-terminal $\Nsubtype$",
  math_macro = \Nsubtype,
} =
    | ND_Subtype_One(tok_one: Tok_subtypes, tok_two: Tok_identifier(Identifier), tok_three: Tok_with, n_four: N_fields)
    {
      "the parse node with the following subnodes: {tok_one}, {tok_two}, {tok_three}, and the $\Nfields$ non-terminal {n_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Subtype_Two(tok_one: Tok_subtypes, tok_two: Tok_identifier(Identifier))
    {
      "the parse node with the following subnodes: {tok_one} and {tok_two}",
      math_macro = \dummyrulelabel
    }
;

ast N_subtype_opt
{
  "non-terminal $\Nsubtypeopt$",
  math_macro = \Nsubtypeopt,
} =
    (subtype_opt: option(N_subtype))
    {
      "the optional subtyping information parse node with subnode {subtype_opt}"
    }
;

ast N_typed_identifier
{
  "non-terminal $\Ntypedidentifier$",
  math_macro = \Ntypedidentifier,
} =
    (subnode_one: Tok_identifier(Identifier), subnode_two: N_as_ty)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nasty$ non-terminal {subnode_two}"
    }
;

ast N_opt_typed_identifier
{
  "non-terminal $\Nopttypedidentifier$",
  math_macro = \Nopttypedidentifier,
} =
    (subnode_one: Tok_identifier(Identifier), subnode_two: p_option[[N_as_ty]])
    {
      "the parse node with the following subnodes: {subnode_one} and {subnode_two}"
    }
;

ast N_as_ty
{
  "non-terminal $\Nasty$",
  math_macro = \Nasty,
} =
    (subnode_one: Tok_colon, subnode_two: N_ty)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nty$ non-terminal {subnode_two}"
    }
;

ast N_return_type
{
  "non-terminal $\Nreturntype$",
  math_macro = \Nreturntype,
} =
    (subnode_one: Tok_arrow, subnode_two: N_ty)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nty$ non-terminal {subnode_two}"
    }
;

ast N_params_opt
{
  "non-terminal $\Nparamsopt$",
  math_macro = \Nparamsopt,
} =
    | ND_Params_Opt_One(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Params_Opt_Two(subnode_one: Tok_lbrace, subnode_two: clistzero[[N_opt_typed_identifier]], subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_call
{
  "non-terminal $\Ncall$",
  math_macro = \Ncall,
} =
    | ND_Call_One(subnode_one: Tok_identifier(Identifier), subnode_two: plistzero[[N_expr]])
    {
      "the parse node with the following subnodes: {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Call_Two(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: clistone[[N_expr]], subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Call_Three(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: clistone[[N_expr]], subnode_four: Tok_rbrace, subnode_five: plistzero[[N_expr]])
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
;

ast N_elided_param_call
{
  "non-terminal $\Nelidedparamcall$",
  math_macro = \Nelidedparamcall,
} =
    | ND_Elided_Param_Call_One(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Elided_Param_Call_Two(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_rbrace, subnode_four: plistzero[[N_expr]])
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Elided_Param_Call_Three(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_comma, subnode_four: tclistone[[N_expr]], subnode_five: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Elided_Param_Call_Four(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_comma, subnode_four: tclistone[[N_expr]], subnode_five: Tok_rbrace, subnode_six: plistzero[[N_expr]])
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
;

ast N_func_args
{
  "non-terminal $\Nfuncargs$",
  math_macro = \Nfuncargs,
} =
    ND_Func_Args_One(args: plistzero[[N_typed_identifier]])
    {
      "the function arguments parse node with subnodes {args}",
      math_macro = \dummyrulelabel
    }
;

ast N_func_body
{
  "non-terminal $\Nfuncbody$",
  math_macro = \Nfuncbody,
} =
    (subnode_one: Tok_begin, subnode_two: N_stmt_list, subnode_three: Tok_end, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nstmtlist$ non-terminal {subnode_two}, {subnode_three}, and {subnode_four}"
    }
;

ast N_ignored_or_identifier
{
  "non-terminal $\Nignoredoridentifier$",
  math_macro = \Nignoredoridentifier,
} =
    | (subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_accessor_body
{
  "non-terminal $\Naccessorbody$",
  math_macro = \Naccessorbody,
} =
    (subnode_one: Tok_begin, subnode_two: N_accessors, subnode_three: Tok_end, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Naccessors$ non-terminal {subnode_two}, {subnode_three}, and {subnode_four}"
    }
;

ast N_accessors
{
  "non-terminal $\Naccessors$",
  math_macro = \Naccessors,
} =
    | ND_Accessors_One(subnode_one: N_is_readonly, subnode_two: Tok_getter, subnode_three: N_stmt_list, subnode_four: Tok_end, subnode_five: Tok_semicolon, subnode_six: Tok_setter, subnode_seven: N_stmt_list, subnode_eight: Tok_end, subnode_nine: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nisreadonly$ non-terminal {subnode_one}, {subnode_two}, the $\Nstmtlist$ non-terminal {subnode_three}, {subnode_four}, {subnode_five}, {subnode_six}, the $\Nstmtlist$ non-terminal {subnode_seven}, {subnode_eight}, and {subnode_nine}",
      math_macro = \dummyrulelabel
    }
    | ND_Accessors_Two(subnode_one: Tok_setter, subnode_two: N_stmt_list, subnode_three: Tok_end, subnode_four: Tok_semicolon, subnode_five: N_is_readonly, subnode_six: Tok_getter, subnode_seven: N_stmt_list, subnode_eight: Tok_end, subnode_nine: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nstmtlist$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, the $\Nisreadonly$ non-terminal {subnode_five}, {subnode_six}, the $\Nstmtlist$ non-terminal {subnode_seven}, {subnode_eight}, and {subnode_nine}",
      math_macro = \dummyrulelabel
    }
;

ast N_qualifier
{
  "non-terminal $\Nqualifier$",
  math_macro = \Nqualifier,
} =
    | (subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_pure)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_readonly)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_noreturn)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_purity_keyword
{
  "non-terminal $\Npuritykeyword$",
  math_macro = \Npuritykeyword,
} =
    | (subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_pure)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_readonly)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_is_readonly
{
  "non-terminal $\Nisreadonly$",
  math_macro = \Nisreadonly,
} =
    | (subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_readonly)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_override
{
  "non-terminal $\Noverride$",
  math_macro = \Noverride,
} =
    | (subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_impdef)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_implementation)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_local_decl_keyword
{
  "non-terminal $\Nlocaldeclkeyword$",
  math_macro = \Nlocaldeclkeyword,
} =
    | (subnode_one: Tok_let)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_constant)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_var)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_global_decl_keyword_non_config
{
  "non-terminal $\Nglobaldeclkeywordnonconfig$",
  math_macro = \Nglobaldeclkeywordnonconfig,
} =
    | (subnode_one: Tok_let)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_constant)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_var)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_global_decl_keyword
{
  "non-terminal $\Nglobaldeclkeyword$",
  math_macro = \Nglobaldeclkeyword,
} =
    | ND_Global_Decl_Keyword_One(subnode_one: N_global_decl_keyword_non_config)
    {
      "the parse node with the following subnode: the $\Nglobaldeclkeywordnonconfig$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Global_Decl_Keyword_Two(subnode_one: Tok_config)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_direction
{
  "non-terminal $\Ndirection$",
  math_macro = \Ndirection,
} =
    | (subnode_one: Tok_to)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_downto)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_case_alt_list
{
  "non-terminal $\Ncasealtlist$",
  math_macro = \Ncasealtlist,
} =
    ND_Case_Alt_List_One(cases: list1(N_case_alt))
    {
      "the list of cases parse node with subnodes {cases}",
      math_macro = \dummyrulelabel
    }
;

ast N_case_alt
{
  "non-terminal $\Ncasealt$",
  math_macro = \Ncasealt,
} =
    (subnode_one: Tok_when, subnode_two: N_pattern_list, subnode_three: p_option[[(Tok_where, N_expr)]], subnode_four: Tok_arrow, subnode_five: N_stmt_list)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Npatternlist$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, and the $\Nstmtlist$ non-terminal {subnode_five}"
    }
;

ast N_otherwise_opt
{
  "non-terminal $\Notherwiseopt$",
  math_macro = \Notherwiseopt,
} =
    | ND_Otherwise_Opt_One(subnode_one: Tok_otherwise, subnode_two: Tok_arrow, subnode_three: N_stmt_list)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Nstmtlist$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Otherwise_Opt_Two(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_catcher
{
  "non-terminal $\Ncatcher$",
  math_macro = \Ncatcher,
} =
    | ND_Catcher_One(subnode_one: Tok_when, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_colon, subnode_four: N_ty, subnode_five: Tok_arrow, subnode_six: N_stmt_list)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, the $\Nty$ non-terminal {subnode_four}, {subnode_five}, and the $\Nstmtlist$ non-terminal {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Catcher_Two(subnode_one: Tok_when, subnode_two: N_ty, subnode_three: Tok_arrow, subnode_four: N_stmt_list)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nty$ non-terminal {subnode_two}, {subnode_three}, and the $\Nstmtlist$ non-terminal {subnode_four}",
      math_macro = \dummyrulelabel
    }
;

ast N_loop_limit
{
  "non-terminal $\Nlooplimit$",
  math_macro = \Nlooplimit,
} =
    | ND_Loop_Limit_One(subnode_one: Tok_looplimit, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Loop_Limit_Two(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_stmt
{
  "non-terminal $\Nstmt$",
  math_macro = \Nstmt,
} =
    | ND_Stmt_One(subnode_one: Tok_if, subnode_two: N_expr, subnode_three: Tok_then, subnode_four: N_stmt_list, subnode_five: N_s_else, subnode_six: Tok_end, subnode_seven: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Nstmtlist$ non-terminal {subnode_four}, the $\Nselse$ non-terminal {subnode_five}, {subnode_six}, and {subnode_seven}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Two(subnode_one: Tok_case, subnode_two: N_expr, subnode_three: Tok_of, subnode_four: N_case_alt_list, subnode_five: Tok_end, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Ncasealtlist$ non-terminal {subnode_four}, {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Three(subnode_one: Tok_case, subnode_two: N_expr, subnode_three: Tok_of, subnode_four: N_case_alt_list, subnode_five: Tok_otherwise, subnode_six: Tok_arrow, subnode_seven: N_stmt_list, subnode_eight: Tok_end, subnode_nine: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Ncasealtlist$ non-terminal {subnode_four}, {subnode_five}, {subnode_six}, the $\Nstmtlist$ non-terminal {subnode_seven}, {subnode_eight}, and {subnode_nine}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Four(subnode_one: Tok_while, subnode_two: N_expr, subnode_three: N_loop_limit, subnode_four: Tok_do, subnode_five: N_stmt_list, subnode_six: Tok_end, subnode_seven: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, the $\Nlooplimit$ non-terminal {subnode_three}, {subnode_four}, the $\Nstmtlist$ non-terminal {subnode_five}, {subnode_six}, and {subnode_seven}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Five(subnode_one: Tok_for, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_eq, subnode_four: N_expr, subnode_five: N_direction, subnode_six: N_expr, subnode_seven: N_loop_limit, subnode_eight: Tok_do, subnode_nine: N_stmt_list, subnode_ten: Tok_end, subnode_eleven: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, the $\Nexpr$ non-terminal {subnode_four}, the $\Ndirection$ non-terminal {subnode_five}, the $\Nexpr$ non-terminal {subnode_six}, the $\Nlooplimit$ non-terminal {subnode_seven}, {subnode_eight}, the $\Nstmtlist$ non-terminal {subnode_nine}, {subnode_ten}, and {subnode_eleven}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Six(subnode_one: Tok_try, subnode_two: N_stmt_list, subnode_three: Tok_catch, subnode_four: /*list1*/ list1(N_catcher), subnode_five: N_otherwise_opt, subnode_six: Tok_end, subnode_seven: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nstmtlist$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, the $\Notherwiseopt$ non-terminal {subnode_five}, {subnode_six}, and {subnode_seven}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Seven(subnode_one: Tok_pass, subnode_two: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Eight(subnode_one: Tok_return, subnode_two: p_option[[N_expr]], subnode_three: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Nine(subnode_one: N_call, subnode_two: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Ncall$ non-terminal {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Ten(subnode_one: Tok_assert, subnode_two: N_expr, subnode_three: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Eleven(subnode_one: N_local_decl_keyword, subnode_two: N_decl_item, subnode_three: p_option[[N_as_ty]], subnode_four: Tok_eq, subnode_five: N_expr, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nlocaldeclkeyword$ non-terminal {subnode_one}, the $\Ndeclitem$ non-terminal {subnode_two}, {subnode_three}, {subnode_four}, the $\Nexpr$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Twelve(subnode_one: N_lexpr, subnode_two: Tok_eq, subnode_three: N_expr, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nlexpr$ non-terminal {subnode_one}, {subnode_two}, the $\Nexpr$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Thirteen(subnode_one: N_call, subnode_two: N_setter_access, subnode_three: Tok_eq, subnode_four: N_expr, subnode_five: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Ncall$ non-terminal {subnode_one}, the $\Nsetteraccess$ non-terminal {subnode_two}, {subnode_three}, the $\Nexpr$ non-terminal {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Fourteen(subnode_one: N_call, subnode_two: N_setter_access, subnode_three: N_slices, subnode_four: Tok_eq, subnode_five: N_expr, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Ncall$ non-terminal {subnode_one}, the $\Nsetteraccess$ non-terminal {subnode_two}, the $\Nslices$ non-terminal {subnode_three}, {subnode_four}, the $\Nexpr$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Fifteen(subnode_one: N_call, subnode_two: Tok_dot, subnode_three: Tok_lbracket, subnode_four: clistone[[Tok_identifier(Identifier)]], subnode_five: Tok_rbracket, subnode_six: Tok_eq, subnode_seven: N_expr, subnode_eight: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Ncall$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, {subnode_five}, {subnode_six}, the $\Nexpr$ non-terminal {subnode_seven}, and {subnode_eight}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Sixteen(subnode_one: N_local_decl_keyword, subnode_two: N_decl_item, subnode_three: N_as_ty, subnode_four: Tok_eq, subnode_five: N_elided_param_call, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: the $\Nlocaldeclkeyword$ non-terminal {subnode_one}, the $\Ndeclitem$ non-terminal {subnode_two}, the $\Nasty$ non-terminal {subnode_three}, {subnode_four}, the $\Nelidedparamcall$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Seventeen(subnode_one: Tok_var, subnode_two: N_decl_item, subnode_three: N_as_ty, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Ndeclitem$ non-terminal {subnode_two}, the $\Nasty$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Eighteen(subnode_one: Tok_var, subnode_two: clisttwo[[Tok_identifier(Identifier)]], subnode_three: N_as_ty, subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Nasty$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Nineteen(subnode_one: Tok_print, subnode_two: plistzero[[N_expr]], subnode_three: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_Twenty(subnode_one: Tok_println, subnode_two: plistzero[[N_expr]], subnode_three: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_TwentyOne(subnode_one: Tok_unreachable, subnode_two: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_TwentyTwo(subnode_one: Tok_repeat, subnode_two: N_stmt_list, subnode_three: Tok_until, subnode_four: N_expr, subnode_five: N_loop_limit, subnode_six: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nstmtlist$ non-terminal {subnode_two}, {subnode_three}, the $\Nexpr$ non-terminal {subnode_four}, the $\Nlooplimit$ non-terminal {subnode_five}, and {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_TwentyThree(subnode_one: Tok_throw, subnode_two: N_expr, subnode_three: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Stmt_TwentyFour(subnode_one: Tok_pragma, subnode_two: Tok_identifier(Identifier), subnode_three: list0(N_expr), subnode_four: Tok_semicolon)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
;

ast N_stmt_list
{
  "non-terminal $\Nstmtlist$",
  math_macro = \Nstmtlist,
} =
    ND_Stmt_List_One(stmts: list1(N_stmt))
    {
      "the statement list parse node with list of subnodes {stmts}",
      math_macro = \dummyrulelabel
    }
;

ast N_s_else
{
  "non-terminal $\Nselse$",
  math_macro = \Nselse,
} =
    | ND_S_Else_One(subnode_one: Tok_elseif, subnode_two: N_expr, subnode_three: Tok_then, subnode_four: N_stmt_list, subnode_five: N_s_else)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Nstmtlist$ non-terminal {subnode_four}, and the $\Nselse$ non-terminal {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_S_Else_Two(subnode_one: Tok_else, subnode_two: N_stmt_list)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nstmtlist$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_S_Else_Three(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_lexpr
{
  "non-terminal $\Nlexpr$",
  math_macro = \Nlexpr,
} =
    | ND_Lexpr_One(subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Lexpr_Two(subnode_one: N_basic_lexpr)
    {
      "the parse node with the following subnode: the $\Nbasiclexpr$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Lexpr_Three(subnode_one: Tok_lpar, subnode_two: plisttwo[[N_discard_or_basic_lexpr]], subnode_three: Tok_rpar)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Lexpr_Four(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_dot, subnode_three: Tok_lbracket, subnode_four: clistone[[Tok_identifier(Identifier)]], subnode_five: Tok_rbracket)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Lexpr_Five(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_dot, subnode_three: Tok_lpar, subnode_four: plisttwo[[N_discard_or_identifier]], subnode_five: Tok_rpar)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
;

ast N_basic_lexpr
{
  "non-terminal $\Nbasiclexpr$",
  math_macro = \Nbasiclexpr,
} =
    | ND_Basic_Lexpr_One(subnode_one: Tok_identifier(Identifier), subnode_two: N_access)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Naccess$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Basic_Lexpr_Two(subnode_one: Tok_identifier(Identifier), subnode_two: N_access, subnode_three: N_slices)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Naccess$ non-terminal {subnode_two}, and the $\Nslices$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_access
{
  "non-terminal $\Naccess$",
  math_macro = \Naccess,
} =
    | ND_Access_One(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Access_Two(subnode_one: Tok_dot, subnode_two: Tok_identifier(Identifier), subnode_three: N_access)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Naccess$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Access_Three(subnode_one: Tok_llbracket, subnode_two: N_expr, subnode_three: Tok_rrbracket, subnode_four: N_access)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, and the $\Naccess$ non-terminal {subnode_four}",
      math_macro = \dummyrulelabel
    }
;

ast N_discard_or_basic_lexpr
{
  "non-terminal $\Ndiscardorbasiclexpr$",
  math_macro = \Ndiscardorbasiclexpr,
} =
    | ND_Discard_Or_Basic_Lexpr_One(subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Discard_Or_Basic_Lexpr_Two(subnode_one: N_basic_lexpr)
    {
      "the parse node with the following subnode: the $\Nbasiclexpr$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_discard_or_identifier
{
  "non-terminal $\Ndiscardoridentifier$",
  math_macro = \Ndiscardoridentifier,
} =
    | (subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_setter_access
{
  "non-terminal $\Nsetteraccess$",
  math_macro = \Nsetteraccess,
} =
    | ND_Setter_Access_One(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Setter_Access_Two(subnode_one: Tok_dot, subnode_two: Tok_identifier(Identifier), subnode_three: N_setter_access)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Nsetteraccess$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_decl_item
{
  "non-terminal $\Ndeclitem$",
  math_macro = \Ndeclitem,
} =
    | ND_Decl_Item_One(subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Decl_Item_Two(subnode_one: clisttwo[[N_ignored_or_identifier]])
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_constraint_kind_opt
{
  "non-terminal $\Nconstraintkindopt$",
  math_macro = \Nconstraintkindopt,
} =
    | ND_Constraint_Kind_Opt_One(subnode_one: N_constraint_kind)
    {
      "the parse node with the following subnode: the $\Nconstraintkind$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Constraint_Kind_Opt_Two(subnode_one: epsilon)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_constraint_kind
{
  "non-terminal $\Nconstraintkind$",
  math_macro = \Nconstraintkind,
} =
    | ND_Constraint_Kind_One(subnode_one: Tok_lbrace, subnode_two: clistone[[N_int_constraint]], subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Constraint_Kind_Two(subnode_one: Tok_lbrace, subnode_two: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
;

ast N_int_constraint
{
  "non-terminal $\Nintconstraint$",
  math_macro = \Nintconstraint,
} =
    | ND_Int_Constraint_One(subnode_one: N_expr)
    {
      "the parse node with the following subnode: the $\Nexpr$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Int_Constraint_Two(subnode_one: N_expr, subnode_two: Tok_slicing, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_expr_pattern
{
  "non-terminal $\Nexprpattern$",
  math_macro = \Nexprpattern,
} =
    | ND_Expr_Pattern_One(subnode_one: N_value)
    {
      "the parse node with the following subnode: the $\Nvalue$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Two(subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Three(subnode_one: N_expr_pattern, subnode_two: N_binop, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, the $\Nbinop$ non-terminal {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Four(subnode_one: N_unop, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: the $\Nunop$ non-terminal {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Five(subnode_one: Tok_if, subnode_two: N_expr, subnode_three: Tok_then, subnode_four: N_expr, subnode_five: Tok_else, subnode_six: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Nexpr$ non-terminal {subnode_four}, {subnode_five}, and the $\Nexpr$ non-terminal {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Six(subnode_one: N_call)
    {
      "the parse node with the following subnode: the $\Ncall$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Seven(subnode_one: N_expr_pattern, subnode_two: N_slices)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one} and the $\Nslices$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Eight(subnode_one: N_expr_pattern, subnode_two: Tok_llbracket, subnode_three: N_expr, subnode_four: Tok_rrbracket)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, the $\Nexpr$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Nine(subnode_one: N_expr_pattern, subnode_two: Tok_dot, subnode_three: Tok_identifier(Identifier))
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Ten(subnode_one: N_expr_pattern, subnode_two: Tok_dot, subnode_three: Tok_lbracket, subnode_four: clistone[[Tok_identifier(Identifier)]], subnode_five: Tok_rbracket)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Eleven(subnode_one: N_expr_pattern, subnode_two: Tok_as, subnode_three: N_ty)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, and the $\Nty$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Twelve(subnode_one: N_expr_pattern, subnode_two: Tok_as, subnode_three: N_constraint_kind)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, and the $\Nconstraintkind$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Thirteen(subnode_one: N_expr, subnode_two: Tok_in, subnode_three: N_pattern_set)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Npatternset$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Fourteen(subnode_one: N_expr, subnode_two: Tok_eqop, subnode_three: Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Fifteen(subnode_one: N_expr, subnode_two: Tok_neq, subnode_three: Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Sixteen(subnode_one: Tok_arbitrary, subnode_two: Tok_colon, subnode_three: N_ty)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Nty$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Seventeen(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_minus, subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Eighteen(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: clistone[[N_field_assign]], subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Pattern_Nineteen(subnode_one: Tok_lpar, subnode_two: N_expr_pattern, subnode_three: Tok_rpar)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexprpattern$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_pattern_set
{
  "non-terminal $\Npatternset$",
  math_macro = \Npatternset,
} =
    | ND_Pattern_Set_One(subnode_one: Tok_bnot, subnode_two: Tok_lbrace, subnode_three: N_pattern_list, subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Npatternlist$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Set_Two(subnode_one: Tok_lbrace, subnode_two: N_pattern_list, subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Npatternlist$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_pattern_list
{
  "non-terminal $\Npatternlist$",
  math_macro = \Npatternlist,
} =
    | ND_Pattern_List_One(patterns: plisttwo[[N_pattern]])
    {
      "the pattern list parse node with the list of subnodes {patterns}",
      math_macro = \dummyrulelabel
    }
;

ast N_pattern
{
  "non-terminal $\Npattern$",
  math_macro = \Npattern,
} =
    | ND_Pattern_One(subnode_one: N_expr_pattern)
    {
      "the parse node with the following subnode: the $\Nexprpattern$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Two(subnode_one: N_expr_pattern, subnode_two: Tok_slicing, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexprpattern$ non-terminal {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Three(subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Four(subnode_one: Tok_leq, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Five(subnode_one: Tok_geq, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Six(subnode_one: Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Seven(subnode_one: plisttwo[[N_pattern]])
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Pattern_Eight(subnode_one: N_pattern_set)
    {
      "the parse node with the following subnode: the $\Npatternset$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_fields
{
  "non-terminal $\Nfields$",
  math_macro = \Nfields,
} =
    | ND_Fields_One(subnode_one: Tok_lbrace, subnode_two: Tok_minus, subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Fields_Two(subnode_one: Tok_lbrace, subnode_two: tclistone[[N_typed_identifier]], subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
;

ast N_slices
{
  "non-terminal $\Nslices$",
  math_macro = \Nslices,
} =
    (subnode_one: Tok_lbracket, subnode_two: clistone[[N_slice]], subnode_three: Tok_rbracket)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}"
    }
;

ast N_slice
{
  "non-terminal $\Nslice$",
  math_macro = \Nslice,
} =
    | ND_Slice_One(subnode_one: N_expr)
    {
      "the parse node with the following subnode: the $\Nexpr$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Slice_Two(subnode_one: N_expr, subnode_two: Tok_colon, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Slice_Three(subnode_one: N_expr, subnode_two: Tok_pluscolon, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Slice_Four(subnode_one: N_expr, subnode_two: Tok_starcolon, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Slice_Five(subnode_one: Tok_colon, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
;

ast N_bitfieldsopt
{
  "non-terminal $\Nbitfieldsopt$",
  math_macro = \Nbitfieldsopt,
} =
  | ND_N_bitfieldsopt_One(epsilon)
    {
      "no bitfields",
      math_macro = \dummyrulelabel,
    }
  | ND_N_bitfieldsopt_Two(bitfields: N_bitfields)
  {
    "the parse node with bitfields {bitfields}",
    math_macro = \dummyrulelabel,
  }
;

ast N_bitfields
{
  "non-terminal $\Nbitfields$",
  math_macro = \Nbitfields,
} =
    (subnode_one: Tok_lbrace, subnode_two: tclistzero[[N_bitfield]], subnode_three: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and {subnode_three}"
    }
;

ast N_bitfield
{
  "non-terminal $\Nbitfield$",
  math_macro = \Nbitfield,
} =
    | ND_Bitfield_One(subnode_one: N_slices, subnode_two: Tok_identifier(Identifier))
    {
      "the parse node with the following subnodes: the $\Nslices$ non-terminal {subnode_one} and {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Bitfield_Two(subnode_one: N_slices, subnode_two: Tok_identifier(Identifier), subnode_three: N_bitfields)
    {
      "the parse node with the following subnodes: the $\Nslices$ non-terminal {subnode_one}, {subnode_two}, and the $\Nbitfields$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Bitfield_Three(subnode_one: N_slices, subnode_two: Tok_identifier(Identifier), subnode_three: Tok_colon, subnode_four: N_ty)
    {
      "the parse node with the following subnodes: the $\Nslices$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, and the $\Nty$ non-terminal {subnode_four}",
      math_macro = \dummyrulelabel
    }
;

ast N_ty
{
  "non-terminal $\Nty$",
  math_macro = \Nty,
} =
    | ND_Ty_One(subnode_one: Tok_integer, subnode_two: N_constraint_kind_opt)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nconstraintkindopt$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Two(subnode_one: Tok_real)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Three(subnode_one: Tok_string)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Four(subnode_one: Tok_boolean)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Five(subnode_one: Tok_bit)
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Six(subnode_one: Tok_bits, subnode_two: Tok_lpar, subnode_three: N_expr, subnode_four: Tok_rpar, subnode_five: N_bitfieldsopt)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Nexpr$ non-terminal {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Seven(subnode_one: Tok_lpar, subnode_two: N_ty, subnode_three: Tok_rpar)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nty$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Eight(subnode_one: plisttwo[[N_ty]])
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Nine(subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Ten(subnode_one: Tok_array, subnode_two: Tok_llbracket, subnode_three: N_expr, subnode_four: Tok_rrbracket, subnode_five: Tok_of, subnode_six: N_ty)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, the $\Nexpr$ non-terminal {subnode_three}, {subnode_four}, {subnode_five}, and the $\Nty$ non-terminal {subnode_six}",
      math_macro = \dummyrulelabel
    }
;

ast N_ty_decl
{
  "non-terminal $\Ntydecl$",
  math_macro = \Ntydecl,
} =
    | ND_Ty_Decl_One(subnode_one: N_ty)
    {
      "the parse node with the following subnode: the $\Nty$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Decl_Two(subnode_one: Tok_enumeration, subnode_two: Tok_lbrace, subnode_three: clistone[[Tok_identifier(Identifier)]], subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Decl_Three(subnode_one: Tok_record, subnode_two: N_fields)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nfields$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Decl_Four(subnode_one: Tok_exception, subnode_two: N_fields)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nfields$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
;

ast N_field_assign
{
  "non-terminal $\Nfieldassign$",
  math_macro = \Nfieldassign,
} =
    (subnode_one: Tok_identifier(Identifier), subnode_two: Tok_eq, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}"
    }
;

ast N_ty_or_collection
{
  "non-terminal $\Ntyorcollection$",
  math_macro = \Ntyorcollection,
} =
    | ND_Ty_Or_Collection_One(subnode_one: N_ty)
    {
      "the parse node with the following subnode: the $\Nty$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Ty_Or_Collection_Two(subnode_one: Tok_collection, subnode_two: N_fields)
    {
      "the parse node with the following subnodes: {subnode_one} and the $\Nfields$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
;

ast N_expr
{
  "non-terminal $\Nexpr$",
  math_macro = \Nexpr,
} =
    | ND_Expr_One(subnode_one: N_value)
    {
      "the parse node with the following subnode: the $\Nvalue$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Two(subnode_one: Tok_identifier(Identifier))
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Three(subnode_one: N_expr, subnode_two: N_binop, subnode_three: N_expr)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, the $\Nbinop$ non-terminal {subnode_two}, and the $\Nexpr$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Four(subnode_one: N_unop, subnode_two: N_expr)
    {
      "the parse node with the following subnodes: the $\Nunop$ non-terminal {subnode_one} and the $\Nexpr$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Five(subnode_one: Tok_if, subnode_two: N_expr, subnode_three: Tok_then, subnode_four: N_expr, subnode_five: Tok_else, subnode_six: N_expr)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, {subnode_three}, the $\Nexpr$ non-terminal {subnode_four}, {subnode_five}, and the $\Nexpr$ non-terminal {subnode_six}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Six(subnode_one: N_call)
    {
      "the parse node with the following subnode: the $\Ncall$ non-terminal {subnode_one}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Seven(subnode_one: N_expr, subnode_two: N_slices)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one} and the $\Nslices$ non-terminal {subnode_two}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Eight(subnode_one: N_expr, subnode_two: Tok_llbracket, subnode_three: N_expr, subnode_four: Tok_rrbracket)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, the $\Nexpr$ non-terminal {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Nine(subnode_one: N_expr, subnode_two: Tok_dot, subnode_three: Tok_identifier(Identifier))
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Ten(subnode_one: N_expr, subnode_two: Tok_dot, subnode_three: Tok_lbracket, subnode_four: clistone[[Tok_identifier(Identifier)]], subnode_five: Tok_rbracket)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, {subnode_three}, {subnode_four}, and {subnode_five}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Eleven(subnode_one: N_expr, subnode_two: Tok_as, subnode_three: N_ty)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nty$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Twelve(subnode_one: N_expr, subnode_two: Tok_as, subnode_three: N_constraint_kind)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Nconstraintkind$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Thirteen(subnode_one: N_expr, subnode_two: Tok_in, subnode_three: N_pattern_set)
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and the $\Npatternset$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Fourteen(subnode_one: N_expr, subnode_two: Tok_eqop, subnode_three: Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Fifteen(subnode_one: N_expr, subnode_two: Tok_neq, subnode_three: Tok_masklit(list0(constants_set(zero_bit, one_bit, x_bit))))
    {
      "the parse node with the following subnodes: the $\Nexpr$ non-terminal {subnode_one}, {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Sixteen(subnode_one: Tok_arbitrary, subnode_two: Tok_colon, subnode_three: N_ty)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, and the $\Nty$ non-terminal {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Seventeen(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: Tok_minus, subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Eighteen(subnode_one: Tok_identifier(Identifier), subnode_two: Tok_lbrace, subnode_three: clistone[[N_field_assign]], subnode_four: Tok_rbrace)
    {
      "the parse node with the following subnodes: {subnode_one}, {subnode_two}, {subnode_three}, and {subnode_four}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Nineteen(subnode_one: Tok_lpar, subnode_two: N_expr, subnode_three: Tok_rpar)
    {
      "the parse node with the following subnodes: {subnode_one}, the $\Nexpr$ non-terminal {subnode_two}, and {subnode_three}",
      math_macro = \dummyrulelabel
    }
    | ND_Expr_Twenty(subnode_one: plisttwo[[N_expr]])
    {
      "the parse node with the following subnode: {subnode_one}",
      math_macro = \dummyrulelabel
    }
;

ast N_value
{
  "non-terminal $\Nvalue$",
  math_macro = \Nvalue,
} =
    | (subnode_one: Tok_intlit(Z))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_boollit(Bool))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_reallit(Q))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_bitvectorlit(list0(Bit)))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_stringlit(Strings))
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_unop
{
  "non-terminal $\Nunop$",
  math_macro = \Nunop,
} =
    | (subnode_one: Tok_bnot)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_not)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;

ast N_binop
{
  "non-terminal $\Nbinop$",
  math_macro = \Nbinop,
} =
    | (subnode_one: Tok_and)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_band)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_bor)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_beq)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_div)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_divrm)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_xor)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_eqop)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_neq)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_gt)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_geq)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_bimpl)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_lt)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_leq)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_plus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_minus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_mod)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_mul)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_or)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_rdiv)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_shl)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_shr)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_pow)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_coloncolon)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
    | (subnode_one: Tok_plusplus)
    {
      "the parse node with the following subnode: {subnode_one}"
    }
;
