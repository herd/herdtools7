# AST Build Worklist

## Scope

This worklist is for modeling the AST-building layer in `build.spec` from the TeX sources under `asllib/doc/`, excluding `generated_macros.tex`.

The intended modeling direction for `build.spec` is:

- Use syntax non-terminal types directly instead of a separate `\parsenode{...}` wrapper.
- Match rules against derivations directly, without an extra non-terminal label wrapper when that label is redundant.
- Expand the shorthand builder premises introduced by `\namednode`, `\punnode`, and the abbreviated-rule conventions in `AbstractSyntax.tex`.
- Keep helper builders and desugaring relations when they are part of building the AST from syntax productions.

## Progress

- [x] Scan all `.tex` sources except `generated_macros.tex`.
- [x] Inventory AST-building relations.
- [x] Inventory AST rules and helper rules.
- [x] Record initial source-document bugs and agreed fix names.
- [x] Complete the signatures for the concrete build relations and functions.
- [ ] Normalize the TeX relations into the target aslspec shape.
- [ ] Encode the relations and rules in `build.spec`.
- [ ] Validate with `dune exec -- aslspec asl.spec build.spec`.

## Learnings

- The generic builder conventions are defined centrally in `AbstractSyntax.tex`.
- Preserve the source distinction between `function` and `relation`. Deterministic builders such as `build_value`, `build_unop`, and `build_binop` should remain functions.
- In `prose_description`, when referring to TeX symbols from the source text, wrap them as inline math like `$\Nvalue$` and `$\literal$`.
- In `prose_description`, do not double-escape TeX macros. Use `\Nvalue`, not `\\Nvalue`.
- When no deconstruction is done, prefer `=` over `=:` for constructor matching.
- In the current encoding, `epsilon` is an exception: matching it still needs `=:` to validate.
- Do not use case names ending in `_` to dodge aslspec keyword collisions. Use names like `and_case`, `or_case`, `let_case`, `constant_case`.
- Most build rules are introduced by `\ASTRuleDef{...}`, but there are at least two relevant outliers:
  - `Literals.tex` uses `ASTRule.Value` as a subsection title rather than `\ASTRuleDef{Value}`.
  - `PatternMatching.tex` uses `ASTRule.ExprPattern` as a subsection title rather than `\ASTRuleDef{ExprPattern}`.
- `TopLevel.tex` contains `RenameLocals*` and `SetBuiltin` rules. They are AST-to-AST helpers rather than parse-tree-to-AST builders, but they are still part of the AST-building support layer and should be imported into `build.spec`.
- `Statements.tex` introduces `\buildelidedparamcall` textually under `ASTRule.ElidedParamCall`, but not via a full displayed relation signature block.
- In the TeX sources, builder-definition prose should be wrapped in `\BackupRelation{...} % END_OF_BACKUP`, followed by `\RenderRelation{...}`.
- When importing helper/desugaring signatures from `\NotImportedToASLSpecYet{...}`, exclude `\checkandinterpret`: it defines an interpreter entry point, not AST building.
- Prefer direct list combinators over inventing local helper constructors when the source rule is list-shaped; for example, `global_uninit_multiple_vars` can use `list_map` to construct `decls` directly instead of introducing `make_uninit_var_decl`.
- When rendering all cases of a function or relation, do not introduce case-specific `render rule` aliases. Render the definition directly with `\RenderProseAndFormally{name}`.
- If a source `\ASTRuleDef{...}` groups multiple formal cases that should render together, represent them as a single parent case in the `.spec` file and render that parent case.
- Every case introduced in a rule should be included in at least one render macro, otherwise it will not be rendered.
- When wrapping a source rule block with `\BackupRule{...}`, include the entire grouped rule block before placing `\RenderProseAndFormally{...}`. Do not close the wrapper after only the first case of a multi-case source rule.
- Prefer `cons(x, xs)` for constructing a non-empty list and `match_cons(x, xs)` for matching one, instead of spelling the same shape via `concat(match_singleton_list(x), xs)`.

## Source Issues To Watch

- `LocalStorageDeclarations.tex:140` declares the relation for `\Nignoredoridentifier` as `\buildfuncargs(...)`.
- `SubprogramDeclarations.tex:370` declares the relation for `\Nfuncbody` as `\buildfuncargs(...)`.
- `LocalStorageDeclarations.tex:115-116` returns the bare identifier in the variable case of `DeclItem`, but the AST type requires `LDI_Var(name)`.
- `Statements.tex:617-626` defines `\desugarelidedparameter` with arguments `(\ty, \call)`, but the first formal rule applies it to four arguments.
- `SubprogramDeclarations.tex:468-479` defines `\desugaraccessorpair` with input `\overrideinfo`, but its call sites pass the optional override value produced by `\buildoverride`.
- `GlobalStorageDeclarations.tex:116-128` uses `\Nasty` in the `global_uninit_var` rule, but the grammar production is the single-variable `\Ntyorcollection` case.
- `GlobalStorageDeclarations.tex` / `syntax.spec` `ND_Decl_Six` used `Tok_identifier(Identifier)` for the declaration name position, but this should be `N_ignored_or_identifier`.
- `TopLevel.tex` `RenameLocalsLDI` var case uses `\vxp` in the conclusion without a premise applying `\renamelocalsname` to `\vx`.
- `TopLevel.tex` `RenameLocalsStmt` return case uses `\mapopt{\renamelocals}` where it should use `\mapopt{\renamelocalsexpr}`.
- `TopLevel.tex` `RenameLocalsExpr` record case renames the record type but omits recursively renaming the field initializer expressions.
- `TopLevel.tex` `RenameLocalsPattern` single-expression case incorrectly includes `\PatternNot`, whose recursive argument is a pattern, not an expression.
- `TopLevel.tex` `RenameLocalsStmt` call case applies `\renamelocalsargs` and `\renamelocalsnamedargs` to call arguments and call parameters, but the OCaml code recursively renames them as expression lists.
- `TopLevel.tex` `RenameLocalsStmt` for-loop case leaves the loop index variable unchanged, but the OCaml code renames it.
- `SubprogramDeclarations.tex` `Accessors` applies `\buildstmt` to `\Nstmtlist` nodes, but the statement-list builder is `\buildstmtlist`.

## Bug Tracker

### Open Source Bugs

- [x] `LocalStorageDeclarations.tex:140`
  Source text uses `\buildfuncargs(...)` for `\Nignoredoridentifier`.
  Agreed fix in `build.spec`: `build_ignored_or_identifier`.
- [x] `SubprogramDeclarations.tex:370`
  Source text uses `\buildfuncargs(...)` for `\Nfuncbody`.
  Agreed fix in `build.spec`: `build_func_body`.
- [ ] `LocalStorageDeclarations.tex:115-116`
  Source text returns the bare identifier in the variable case of `DeclItem`.
  Planned fix in `build.spec`: return `LDI_Var(name)`.
- [x] `GlobalStorageDeclarations.tex:116-128`
  Source text uses `\Nasty` in `global_uninit_var`, but the grammar production is the single-variable `\Ntyorcollection` form.
  Applied fix in `build.spec`: use `build_ty_or_collection` on `N_ty_or_collection`.
- [x] `GlobalStorageDeclarations.tex` / `syntax.spec` `ND_Decl_Six`
  The second subnode was typed as `Tok_identifier(Identifier)`, but it should be `N_ignored_or_identifier`.
  Applied fix in `syntax.spec`: change the declaration-name position to `N_ignored_or_identifier`.
- [x] `AssignableExpressions.tex` `DesugarLHSAccess`
  The displayed rule omits the identifier input `\name` from the judgment input, even though the first premise defines `\vlexprs_0 \eqdef \LEVar(\name)`.
  It also uses the indexed family `\vlexprs_i` / `\vlexprs_{|\vaccess|}` without ever defining `\vlexprs` as a sequence object.
  Applied fix in `build.spec`: make the identifier input explicit in `desugar_lhs_access : (Identifier, lhs_access) -> lexpr` and encode the access-chain construction explicitly.
- [x] `AssignableExpressions.tex` `DesugarLHSFieldsTuple`
  The original rule used `\checknoduplicates(\fields) \typearrow \True \OrTypeError`, which emits a type error, but duplicate fields here are a parse/build-time issue.
  Applied fix in `build.spec` and TeX: use `\becheck(\duplicatesexist(\fields), \ParseError)` / `be_check(not(duplicates_exist(fields)), BE_ParseError)`.
- [x] `Expressions.tex` `CheckNotSamePrec`
  The `binop_same` case used `\techeck(..., \BinopPrecedence)` even though the relation returns `\OrBuildError`, not `\OrTypeError`.
  Applied fix in `build.spec`: use `be_check(..., BE_BinopPrecedence)`.
- [x] `Statements.tex` `CasesToCond`
  The `not_last` rule dropped the `otherwise` argument on the recursive `\casestocond` call, and the displayed rules had no `empty` case despite the relation taking a `list0(case_alt)`.
  Applied fix in `build.spec` and TeX: add the `empty` case returning `otherwise_stmt`, and pass `otherwise` through the recursive `not_last` call.
- [ ] `Statements.tex:617-626`
  The displayed signature for `\desugarelidedparameter` takes two arguments `(\ty, \call)`, but the first formal rule applies it to four arguments.
  Current normalization in `build.spec`: keep the displayed two-argument signature. This should be revisited when the formal rules are imported.
- [ ] `SubprogramDeclarations.tex:468-479`
  The displayed signature for `\desugaraccessorpair` takes `\overrideinfo`, but its call sites pass the optional override value produced by `\buildoverride`.
  Current normalization in `build.spec`: use `option(override_info)` to match the call sites. This should be reflected in the source.
- [x] `TopLevel.tex` `RenameLocalsLDI`
  The `var` formal rule uses `\vxp` in the conclusion but omits the premise `\renamelocalsname(\vx) \astarrow \vxp`.
  Applied fix in `build.spec`: rename the variable name explicitly before constructing `\LDIVar(\vxp)`.
- [x] `TopLevel.tex` `RenameLocalsStmt`
  The `s_return` prose/formal rule uses `\mapopt{\renamelocals}` instead of `\mapopt{\renamelocalsexpr}`.
  Applied fix in `build.spec`: rename optional return expressions via `rename_locals_expr`.
- [x] `TopLevel.tex` `RenameLocalsExpr`
  The `e_record` rule renames the record type but omits recursively renaming the field initializer expressions, and its premise uses `\vy` instead of `\vt`.
  Applied fix in `build.spec`: follow the OCaml code by keeping the record type unchanged and renaming each field initializer expression.
- [x] `TopLevel.tex` `RenameLocalsPattern`
  The `single_expr` rule incorrectly includes `\PatternNot`, whose recursive child is a pattern rather than an expression.
  Applied fix in `build.spec`: handle `Pattern_Not` recursively via `rename_locals_pattern`.
- [x] `TopLevel.tex` `RenameLocalsStmt`
  The `s_call` rule applies `\renamelocalsargs` and `\renamelocalsnamedargs` to call arguments and call parameters, but the OCaml code maps `rename_locals_expr` over both expression lists.
  Applied fix in `build.spec`: rename `call_args` and `params` elementwise with `rename_locals_expr`.
- [x] `TopLevel.tex` `RenameLocalsStmt`
  The `s_for` rule leaves the loop index variable unchanged, but the OCaml code renames `index_name`.
  Applied fix in `build.spec`: apply `rename_locals_name` to the loop index variable.
- [x] `SubprogramDeclarations.tex` `Accessors`
  The rules apply `\buildstmt` to `\Nstmtlist` metavariables, but the correct builder for `\Nstmtlist` is `\buildstmtlist`.
  Applied fix in `build.spec`: build getter and setter bodies via `build_stmt_list`.

### Applied Fixes In `build.spec`

- [x] Normalize the `\Nignoredoridentifier` builder name to `build_ignored_or_identifier`.
- [x] Normalize the `\Nfuncbody` builder name to `build_func_body`.
- [x] Import `desugar_elided_parameter` using the displayed two-argument signature from `Statements.tex`.
- [x] Import `desugar_accessor_pair` with `option(override_info)` to match its call sites.
- [x] Import the `TopLevel.tex` `RenameLocals*` helpers and `set_builtin`, but continue to exclude `check_and_interpret`.
- [x] Normalize `global_uninit_var` to the single-variable `N_ty_or_collection` grammar case and build its type via `build_ty_or_collection`.
- [x] Normalize `ND_Decl_Six` so the declaration-name position uses `N_ignored_or_identifier` rather than `Tok_identifier(Identifier)`.

### Applied Fixes In TeX

- [x] Wrap builder-definition prose blocks in `\BackupRelation{...} % END_OF_BACKUP` and add `\RenderRelation{...}` after them.
- [x] Normalize the `\Nignoredoridentifier` builder name to `build_ignored_or_identifier`.
- [x] Normalize the `\Nfuncbody` builder name to `build_func_body`.

## Build Relations

### Generic Operator Documentation

- [ ] Document the operators in `build.spec:1-55` (`clistone_to_list`, `clisttwo_to_list`, `clistzero_to_list`, `tclistone_to_list`, `tclistzero_to_list`, `plisttwo_to_list`, `plistzero_to_list`, `ioption_to_option`) in the reference documentation.

### Remaining Handwritten TeX Rule Blocks To Replace

- [x] Replace the handwritten `build_global_decl_keyword_non_config` rule block in `GlobalStorageDeclarations.tex`.
- [x] Replace the handwritten `build_global_decl_keyword` rule block in `GlobalStorageDeclarations.tex`.
- [x] Replace the handwritten `build_ignored_or_identifier` rule block in `LocalStorageDeclarations.tex`.

### Specification And Declaration Builders

- [x] `builddecl : N_decl -> list0(decl)` from `Specifications.tex:54`
- [x] `buildast : N_spec -> spec` from `Specifications.tex:102`
- [x] `buildglobaldeclkeywordnonconfig : N_globaldeclkeywordnonconfig -> global_decl_keyword` from `GlobalStorageDeclarations.tex:237`
- [x] `buildglobaldeclkeyword : N_globaldeclkeyword -> global_decl_keyword` from `GlobalStorageDeclarations.tex:281`

### Type-Related Builders

- [x] `buildsubtype : N_subtype -> (Identifier, list0((Identifier, ty)))` from `TypeDeclarations.tex:80`
- [x] `buildsubtypeopt : N_subtypeopt -> option((Identifier, list0((Identifier, ty))))` from `TypeDeclarations.tex:114`
- [x] `buildfields : N_fields -> list0(field)` from `TypeDeclarations.tex:134`
- [x] `buildty : N_ty -> ty | BuildError` from `Types.tex:67`
- [x] `buildtydecl : N_tydecl -> ty | BuildError` from `Types.tex:81`
- [x] `buildtyorcollection : N_tyorcollection -> ty | BuildError` from `Types.tex:95`
- [x] `buildasty : N_asty -> ty | BuildError` from `Types.tex:109`
- [x] `buildconstraintkindopt : N_constraintkindopt -> constraintkind` from `Types.tex:231`
- [x] `buildconstraintkind : N_constraintkind -> constraintkind` from `Types.tex:260`
- [x] `buildintconstraint : N_intconstraint -> intconstraint` from `Types.tex:291`

### Subprogram Builders

- [x] `buildaccessorbody : N_accessorbody -> accessorpair` from `SubprogramDeclarations.tex:215`
- [x] `buildrecurselimit : N_recurselimit -> option(expr)` from `SubprogramDeclarations.tex:235`
- [x] `buildtypedidentifier : N_typedidentifier -> (Identifier, ty)` from `SubprogramDeclarations.tex:262`
- [x] `buildopttypedidentifier : N_opttypedidentifier -> (Identifier, option(ty))` from `SubprogramDeclarations.tex:278`
- [x] `buildreturntype : N_returntype -> ty` from `SubprogramDeclarations.tex:302`
- [x] `buildparamsopt : N_paramsopt -> list0((Identifier, option(ty)))` from `SubprogramDeclarations.tex:320`
- [x] `buildfuncargs : N_funcargs -> list0((Identifier, ty))` from `SubprogramDeclarations.tex:349`
- [x] `build_func_body : N_funcbody -> stmt` from `SubprogramDeclarations.tex:370`
- [x] `buildaccessors : N_accessors -> accessor_pair` from `SubprogramDeclarations.tex:393`
- [x] `buildqualifier : N_qualifier -> option(qualifier)` from `SubprogramDeclarations.tex:536`
- [x] `buildisreadonly : N_isreadonly -> Bool` from `SubprogramDeclarations.tex:572`
- [x] `buildoverride : N_override -> option(override)` from `SubprogramDeclarations.tex:596`

### Local Declaration Builders

- [x] `build_local_decl_keyword : N_localdeclkeyword -> local_decl_keyword` from `LocalStorageDeclarations.tex:81`
- [x] `build_decl_item : N_declitem -> local_decl_item` from `LocalStorageDeclarations.tex:107`
- [x] `build_ignored_or_identifier : N_ignoredoridentifier -> Identifier` from `LocalStorageDeclarations.tex:140`

### Literal, Operator, Slice, Expression, Pattern, LExpr, Bitfield, Catcher Builders

- [x] `build_value : N_value -> literal` from `Literals.tex:56`
- [x] `build_unop : N_unop -> unop` from `PrimitiveOperations.tex:73`
- [x] `build_binop : N_binop -> binop` from `PrimitiveOperations.tex:102`
- [x] `buildslice : N_slice -> slice` from `Slicing.tex:65`
- [x] `buildslices : N_slices -> list1(slice)` from `Slicing.tex:231`
- [x] `buildexpr : N_expr -> expr | BuildError` from `Expressions.tex:53`
- [x] `buildcall : N_call -> call | BuildError` from `Expressions.tex:639`
- [x] `buildfieldassign : N_fieldassign -> (Identifier, expr)` from `Expressions.tex:1467`
- [x] `buildpattern : N_pattern -> pattern` from `PatternMatching.tex:35`
- [x] `buildpatternset : N_patternset -> pattern` from `PatternMatching.tex:385`
- [x] `buildpatternlist : N_patternlist -> pattern` from `PatternMatching.tex:414`
- [x] `buildexprpattern : N_exprpattern -> expr` from `PatternMatching.tex:537`
- [x] `buildlexpr : N_lexpr -> lexpr` from `AssignableExpressions.tex:130`
- [x] `buildbasiclexpr : N_basiclexpr -> (Identifier, lhsaccess)` from `AssignableExpressions.tex:193`
- [x] `buildaccess : N_access -> lhsaccess` from `AssignableExpressions.tex:236`
- [x] `builddiscardorbasiclexpr : N_discardorbasiclexpr -> option((Identifier, lhsaccess))` from `AssignableExpressions.tex:266`
- [x] `builddiscardoridentifier : N_discardoridentifier -> option(Identifier)` from `AssignableExpressions.tex:294`
- [x] `buildsetteraccess : N_setteraccess -> list0(fieldorarrayaccess)` from `Statements.tex:221`
- [x] `desugar_lhs_access_opt : option((Identifier, lhs_access)) -> lexpr` from `AssignableExpressions.tex:356`
- [x] `desugar_lhs_field_opt : Identifier * option(Identifier) -> lexpr` from `AssignableExpressions.tex:402`
- [x] `buildbitfields : N_bitfields -> list0(bitfield)` from `Bitfields.tex:131`
- [x] `buildbitfield : N_bitfield -> bitfield` from `Bitfields.tex:155`
- [x] `buildcatcher : N_catcher -> catcher` from `CatchingExceptions.tex:57`

### Helper And Desugaring Functions

- [x] `binop_prec : binop -> N` from `Expressions.tex:266`
- [x] `desugar_lhs_access : (Identifier, lhs_access) -> lexpr` from `AssignableExpressions.tex:310`
- [x] `desugar_lhs_tuple : list0(option((Identifier, lhs_access))) -> lexpr` from `AssignableExpressions.tex:346`
- [x] `desugar_lhs_access_opt : option((Identifier, lhs_access)) -> lexpr` from `AssignableExpressions.tex:367`
- [x] `desugar_lhs_fields_tuple : Identifier * list0(option(Identifier)) -> lexpr | BuildError` from `AssignableExpressions.tex:398`
- [x] `desugar_lhs_field_opt : Identifier * option(Identifier) -> lexpr` from `AssignableExpressions.tex:417`
- [x] `check_not_same_prec : binop * expr -> CheckResult | BuildError` from `Expressions.tex:265`
- [x] `set_call_type : call * subprogram_type -> call` from `Statements.tex:779`
- [x] `make_setter : call * expr -> call` from `Statements.tex:210`
- [x] `desugar_setter : call * lhs_access * expr -> stmt` from `Statements.tex:249`
- [x] `desugar_setter_set_fields : call * list0(Identifier) * expr -> stmt` from `Statements.tex:282`
- [x] `read_modify_write : call * Identifier * stmt -> stmt` from `Statements.tex:301`
- [x] `desugar_elided_parameter : ty * call -> option(expr) | BuildError` from `Statements.tex:606`
- [x] `stmt_from_list : list0(stmt) -> stmt` from `Statements.tex:684`
- [x] `sequence_stmts : stmt * stmt -> stmt` from `Statements.tex:702`
- [x] `desugar_case_stmt : expr * list0(case_alt) * stmt -> stmt` from `Statements.tex:1160`
- [x] `cases_to_cond : expr * list0(case_alt) * stmt -> stmt` from `Statements.tex:1187`
- [x] `case_to_cond : expr * case_alt * stmt -> stmt` from `Statements.tex:1209`
- [x] `desugar_accessor_pair : option(override_info) * Identifier * list0((Identifier, option(ty))) * list0(typed_identifier) * Identifier * ty * accessor_pair -> list0(decl)` from `SubprogramDeclarations.tex:388`
- [x] `rename_locals : list0(decl) -> list0(decl)` from `TopLevel.tex:118`
- [x] `rename_locals_decl : decl -> decl` from `TopLevel.tex:145`
- [x] `rename_locals_func : func -> func` from `TopLevel.tex:180`
- [x] `rename_locals_args : list0((Identifier, ty)) -> list0((Identifier, ty))` from `TopLevel.tex`
- [x] `rename_locals_named_args : list0((Identifier, option(ty))) -> list0((Identifier, option(ty)))` from `TopLevel.tex`
- [x] `rename_locals_ty : ty -> ty` from `TopLevel.tex`
- [x] `rename_locals_stmt : stmt -> stmt` from `TopLevel.tex`
- [x] `rename_locals_expr : expr -> expr` from `TopLevel.tex`
- [x] `rename_locals_lexpr : lexpr -> lexpr` from `TopLevel.tex`
- [x] `rename_locals_ldi : local_decl_item -> local_decl_item` from `TopLevel.tex`
- [x] `rename_locals_constraint : int_constraint -> int_constraint` from `TopLevel.tex`
- [x] `rename_locals_slice : slice -> slice` from `TopLevel.tex`
- [x] `rename_locals_name : Identifier -> Identifier` from `TopLevel.tex`
- [x] `rename_locals_array_index : array_index -> array_index` from `TopLevel.tex`
- [x] `rename_locals_pattern : pattern -> pattern` from `TopLevel.tex`
- [x] `rename_locals_catcher : catcher -> catcher` from `TopLevel.tex`
- [x] `set_builtin : decl -> decl | BuildError` from `TopLevel.tex`

### Statement Builders

- [x] `buildstmt : N_stmt -> stmt` from `Statements.tex:42`
- [x] `buildelidedparamcall : N_elidedparamcall -> call` from `Statements.tex:560`
- [x] `buildstmtlist : N_stmtlist -> stmt` from `Statements.tex:687`
- [x] `buildselse : N_selse -> stmt` from `Statements.tex:894`
- [x] `buildcasealtlist : N_casealtlist -> list1(casealt)` from `Statements.tex:1150`
- [x] `buildcasealt : N_casealt -> casealt` from `Statements.tex:1168`
- [x] `buildlooplimit : N_looplimit -> option(expr)` from `Statements.tex:1417`
- [x] `build_direction : N_direction -> for_direction` from `Statements.tex:1669`
- [x] `buildotherwiseopt : N_otherwiseopt -> option(stmt)` from `Statements.tex:1869`

## Rule Inventory

### `AbstractSyntax.tex`

- [ ] Document the generic operators corresponding to the parameterized-list conversion operators in `build.spec:1-55`.
- [ ] `Identifier`

### `Specifications.tex`

- [x] `AST`

### `GlobalStorageDeclarations.tex`

- [x] `GlobalStorageDecl`
- [x] `GlobalDeclKeywordNonConfig`
- [x] `GlobalDeclKeyword`

### `TypeDeclarations.tex`

- [x] `TypeDecl`
- [x] `Subtype`
- [x] `Subtypeopt`
- [x] `Fields`

### `SubprogramDeclarations.tex`

- [x] `SubprogramDecl`
- [x] `AccessorBody`
- [x] `RecurseLimit`
- [x] `TypedIdentifier`
- [x] `OptTypedIdentifier`
- [x] `ReturnType`
- [x] `ParamsOpt`
- [x] `FuncArgs`
- [x] `FuncBody`
- [x] `Accessors`
- [x] `DesugarAccessorPair`
- [x] `Qualifier`
- [x] `IsReadonly`
- [x] `Override`

### `GlobalPragmas.tex`

- [x] `GlobalPragma`

### `LocalStorageDeclarations.tex`

- [x] `LocalDeclKeyword`
- [x] `DeclItem`
- [x] `IgnoredOrIdentifier`

### `Literals.tex`

- [ ] `Value` (declared as `ASTRule.Value`, not via `\ASTRuleDef`)

### `PrimitiveOperations.tex`

- [ ] `Unop`
- [ ] `Binop`

### `Types.tex`

- [ ] `Ty.TInt`
- [x] `IntConstraintsOpt`
- [x] `IntConstraint`
- [ ] `TReal`
- [ ] `Ty.String`
- [ ] `Ty.BoolType`
- [ ] `Ty.TBits`
- [ ] `BitfieldsOpt`
- [ ] `Ty.TTuple`
- [ ] `ParenType`
- [ ] `TyDecl.TEnum`
- [ ] `Ty.TArray`
- [ ] `TyDecl.TRecord`
- [ ] `TyDecl.TException`
- [x] `TyDecl.TCollection`
- [ ] `Ty.TNamed`
- [ ] `TyDecl`

### `Slicing.tex`

- [x] `Slice`
- [x] `Slices`

### `AssignableExpressions.tex`

- [x] `LExpr`
- [x] `BasicLexpr`
- [x] `Access`
- [x] `DiscardOrBasicLexpr`
- [x] `DiscardOrIdentifier`
- [x] `BuildSetterAccess`
- [x] `DesugarLHSAccess`
- [x] `DesugarLHSTuple`
- [x] `DesugarLHSAccessOpt`
- [x] `DesugarLHSFieldsTuple`
- [x] `DesugarLHSFieldOpt`

### `Expressions.tex`

- [x] `ELit`
- [x] `EVAR`
- [x] `EBinop`
- [x] `CheckNotSamePrec`
- [x] `EUnop`
- [x] `ECond`
- [x] `Call`
- [x] `ECall`
- [x] `ESlice`
- [x] `EGetArray`
- [x] `EGetField`
- [x] `EGetFields`
- [x] `ATC`
- [x] `EPattern`
- [x] `EArbitrary`
- [x] `ERecord`
- [x] `FieldAssign`
- [x] `ETuple`
- [x] `ParenExpr`

### `PatternMatching.tex`

- [x] `PAll`
- [x] `PSingle`
- [x] `PRange`
- [x] `PLeq`
- [x] `PGeq`
- [x] `PMask`
- [x] `PTuple`
- [x] `PatternIsPatternSet`
- [x] `PatternSet`
- [x] `PatternList`
- [x] `ExprPattern` (declared as `ASTRule.ExprPattern`, not via `\ASTRuleDef`)

### `CatchingExceptions.tex`

- [x] `Catcher`

### `Statements.tex`

- [x] `SPass`
- [x] `SAssign`
- [x] `MakeSetter`
- [x] `BuildSetterAccess`
- [x] `DesugarSetter`
- [x] `DesugarSetterSetfields`
- [x] `ReadModifyWrite`
- [x] `SetterAssign`
- [x] `SDecl`
- [x] `ElidedParamCall`
- [x] `DesugarElidedParameter`
- [x] `ElidedParamDecl`
- [x] `StmtList`
- [x] `StmtFromList`
- [x] `SequenceStmts`
- [x] `SCall`
- [x] `SetCallType`
- [x] `SCond`
- [x] `SElse`
- [x] `SCase`
- [x] `CaseAltList`
- [x] `CaseAlt`
- [x] `DesugarCaseStmt`
- [x] `CasesToCond`
- [x] `CaseToCond`
- [x] `SAssert`
- [x] `SWhile`
- [x] `LoopLimit`
- [x] `SRepeat`
- [x] `SFor`
- [x] `Direction`
- [x] `SThrow`
- [x] `STry`
- [x] `OtherwiseOpt`
- [x] `SReturn`
- [x] `SPrint`
- [x] `SUnreachable`
- [x] `SPragma`

### `TopLevel.tex`

- [x] `RenameLocals`
- [x] `RenameLocalsDecl`
- [x] `RenameLocalsFunc`
- [x] `RenameLocalsArgs`
- [x] `RenameLocalsNamedArgs`
- [x] `RenameLocalsTy`
- [x] `RenameLocalsStmt`
- [x] `RenameLocalsExpr`
- [x] `RenameLocalsLexpr`
- [x] `RenameLocalsLDI`
- [x] `RenameLocalsConstraint`
- [x] `RenameLocalsSlice`
- [x] `RenameLocalsName`
- [x] `RenameLocalsArrayIndex`
- [x] `RenameLocalsPattern`
- [x] `RenameLocalsCatcher`
- [x] `SetBuiltin`
