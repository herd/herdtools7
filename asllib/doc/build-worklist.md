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

## Source Issues To Watch

- `LocalStorageDeclarations.tex:140` declares the relation for `\Nignoredoridentifier` as `\buildfuncargs(...)`.
- `SubprogramDeclarations.tex:370` declares the relation for `\Nfuncbody` as `\buildfuncargs(...)`.
- `LocalStorageDeclarations.tex:90-99` only gives cases for `\Tlet` and `\Tvar`, although `syntax.spec` currently includes `\Tconstant` in `\Nlocaldeclkeyword`.
- `LocalStorageDeclarations.tex:115-116` returns the bare identifier in the variable case of `DeclItem`, but the AST type requires `LDI_Var(name)`.
- `Statements.tex:617-626` defines `\desugarelidedparameter` with arguments `(\ty, \call)`, but the first formal rule applies it to four arguments.
- `SubprogramDeclarations.tex:468-479` defines `\desugaraccessorpair` with input `\overrideinfo`, but its call sites pass the optional override value produced by `\buildoverride`.
- `GlobalStorageDeclarations.tex:116-128` uses `\Nasty` in the `global_uninit_var` rule, but the grammar production is the single-variable `\Ntyorcollection` case.

## Bug Tracker

### Open Source Bugs

- [x] `LocalStorageDeclarations.tex:140`
  Source text uses `\buildfuncargs(...)` for `\Nignoredoridentifier`.
  Agreed fix in `build.spec`: `build_ignored_or_identifier`.
- [x] `SubprogramDeclarations.tex:370`
  Source text uses `\buildfuncargs(...)` for `\Nfuncbody`.
  Agreed fix in `build.spec`: `build_func_body`.
- [x] `LocalStorageDeclarations.tex:90-99`
  Source text omits the `\Tconstant` case for `\Nlocaldeclkeyword`.
  Applied fix in `build.spec`: map `Tok_constant` to `LDK_Let`.
- [ ] `LocalStorageDeclarations.tex:115-116`
  Source text returns the bare identifier in the variable case of `DeclItem`.
  Planned fix in `build.spec`: return `LDI_Var(name)`.
- [x] `GlobalStorageDeclarations.tex:116-128`
  Source text uses `\Nasty` in `global_uninit_var`, but the grammar production is the single-variable `\Ntyorcollection` form.
  Applied fix in `build.spec`: use `build_ty_or_collection` on `N_ty_or_collection`.

### Applied Fixes In `build.spec`

- [x] Normalize the `\Nignoredoridentifier` builder name to `build_ignored_or_identifier`.
- [x] Normalize the `\Nfuncbody` builder name to `build_func_body`.
- [x] Add the missing `Tok_constant -> LDK_Let` case in `build_local_decl_keyword`.
- [x] Import `desugar_elided_parameter` using the displayed two-argument signature from `Statements.tex`.
- [x] Import `desugar_accessor_pair` with `option(override_info)` to match its call sites.
- [x] Import the `TopLevel.tex` `RenameLocals*` helpers and `set_builtin`, but continue to exclude `check_and_interpret`.
- [x] Normalize `global_uninit_var` to the single-variable `N_ty_or_collection` grammar case and build its type via `build_ty_or_collection`.

### Applied Fixes In TeX

- [x] Wrap builder-definition prose blocks in `\BackupRelation{...} % END_OF_BACKUP` and add `\RenderRelation{...}` after them.
- [x] Normalize the `\Nignoredoridentifier` builder name to `build_ignored_or_identifier`.
- [x] Normalize the `\Nfuncbody` builder name to `build_func_body`.

## Build Relations

### Generic Helpers

- [ ] `buildlist[b] : maybeemptylist(N) -> list0(A)` from `AbstractSyntax.tex:442`
- [ ] `buildclist[b] : ClistOne/ClistZero(N) -> list0(A)` from `AbstractSyntax.tex:470`
- [ ] `buildplist[b] : PlistZero/PlistTwo(N) -> list0(A)` from `AbstractSyntax.tex:497`
- [ ] `buildtclist[b] : TClistOne/TClistZero(N) -> list1(A)` from `AbstractSyntax.tex:515`
- [ ] `buildoption[b] : option(N) -> option(A)` from `AbstractSyntax.tex:542`
- [ ] `buildidentity : T -> T` from `AbstractSyntax.tex:576`

### Specification And Declaration Builders

- [ ] `builddecl : N_decl -> list0(decl)` from `Specifications.tex:54`
- [ ] `buildast : N_spec -> spec` from `Specifications.tex:102`
- [ ] `buildglobaldeclkeywordnonconfig : N_globaldeclkeywordnonconfig -> global_decl_keyword` from `GlobalStorageDeclarations.tex:237`
- [ ] `buildglobaldeclkeyword : N_globaldeclkeyword -> global_decl_keyword` from `GlobalStorageDeclarations.tex:281`

### Type-Related Builders

- [ ] `buildsubtype : N_subtype -> (Identifier, list0((Identifier, ty)))` from `TypeDeclarations.tex:80`
- [ ] `buildsubtypeopt : N_subtypeopt -> option((Identifier, list0((Identifier, ty))))` from `TypeDeclarations.tex:114`
- [ ] `buildfields : N_fields -> list0(field)` from `TypeDeclarations.tex:134`
- [ ] `buildty : N_ty -> ty | BuildError` from `Types.tex:67`
- [ ] `buildtydecl : N_tydecl -> ty | BuildError` from `Types.tex:81`
- [ ] `buildtyorcollection : N_tyorcollection -> ty | BuildError` from `Types.tex:95`
- [ ] `buildasty : N_asty -> ty | BuildError` from `Types.tex:109`
- [ ] `buildconstraintkindopt : N_constraintkindopt -> constraintkind` from `Types.tex:231`
- [ ] `buildconstraintkind : N_constraintkind -> constraintkind` from `Types.tex:260`
- [ ] `buildintconstraint : N_intconstraint -> intconstraint` from `Types.tex:291`

### Subprogram Builders

- [ ] `buildaccessorbody : N_accessorbody -> accessorpair` from `SubprogramDeclarations.tex:215`
- [ ] `buildrecurselimit : N_recurselimit -> option(expr)` from `SubprogramDeclarations.tex:235`
- [ ] `buildtypedidentifier : N_typedidentifier -> (Identifier, ty)` from `SubprogramDeclarations.tex:262`
- [ ] `buildopttypedidentifier : N_opttypedidentifier -> (Identifier, option(ty))` from `SubprogramDeclarations.tex:278`
- [ ] `buildreturntype : N_returntype -> ty` from `SubprogramDeclarations.tex:302`
- [ ] `buildparamsopt : N_paramsopt -> list0((Identifier, option(ty)))` from `SubprogramDeclarations.tex:320`
- [ ] `buildfuncargs : N_funcargs -> list0((Identifier, ty))` from `SubprogramDeclarations.tex:349`
- [ ] `build_func_body : N_funcbody -> stmt` from `SubprogramDeclarations.tex:370`
- [ ] `buildaccessors : N_accessors -> subprogrambody` from `SubprogramDeclarations.tex:393`
- [ ] `buildqualifier : N_qualifier -> option(qualifier)` from `SubprogramDeclarations.tex:536`
- [ ] `buildisreadonly : N_isreadonly -> Bool` from `SubprogramDeclarations.tex:572`
- [ ] `buildoverride : N_override -> option(override)` from `SubprogramDeclarations.tex:596`

### Local Declaration Builders

- [x] `build_local_decl_keyword : N_localdeclkeyword -> local_decl_keyword` from `LocalStorageDeclarations.tex:81`
- [ ] `build_decl_item : N_declitem -> local_decl_item` from `LocalStorageDeclarations.tex:107`
- [x] `build_ignored_or_identifier : N_ignoredoridentifier -> Identifier` from `LocalStorageDeclarations.tex:140`

### Literal, Operator, Slice, Expression, Pattern, LExpr, Bitfield, Catcher Builders

- [x] `build_value : N_value -> literal` from `Literals.tex:56`
- [x] `build_unop : N_unop -> unop` from `PrimitiveOperations.tex:73`
- [x] `build_binop : N_binop -> binop` from `PrimitiveOperations.tex:102`
- [ ] `buildslice : N_slice -> slice` from `Slicing.tex:65`
- [ ] `buildslices : N_slices -> list1(slice)` from `Slicing.tex:231`
- [ ] `buildexpr : N_expr -> expr | BuildError` from `Expressions.tex:53`
- [ ] `buildcall : N_call -> call | BuildError` from `Expressions.tex:639`
- [ ] `buildfieldassign : N_fieldassign -> (Identifier, expr)` from `Expressions.tex:1467`
- [ ] `buildpattern : N_pattern -> pattern` from `PatternMatching.tex:35`
- [ ] `buildpatternset : N_patternset -> pattern` from `PatternMatching.tex:385`
- [ ] `buildpatternlist : N_patternlist -> pattern` from `PatternMatching.tex:414`
- [ ] `buildexprpattern : N_exprpattern -> expr` from `PatternMatching.tex:537`
- [ ] `buildlexpr : N_lexpr -> lexpr` from `AssignableExpressions.tex:130`
- [ ] `buildbasiclexpr : N_basiclexpr -> (Identifier, lhsaccess)` from `AssignableExpressions.tex:193`
- [ ] `buildaccess : N_access -> lhsaccess` from `AssignableExpressions.tex:236`
- [ ] `builddiscardorbasiclexpr : N_discardorbasiclexpr -> option((Identifier, lhsaccess))` from `AssignableExpressions.tex:266`
- [ ] `builddiscardoridentifier : N_discardoridentifier -> option(Identifier)` from `AssignableExpressions.tex:294`
- [ ] `buildbitfields : N_bitfields -> list0(bitfield)` from `Bitfields.tex:131`
- [ ] `buildbitfield : N_bitfield -> bitfield` from `Bitfields.tex:155`
- [ ] `buildcatcher : N_catcher -> catcher` from `CatchingExceptions.tex:57`

### Statement Builders

- [ ] `buildstmt : N_stmt -> stmt` from `Statements.tex:42`
- [ ] `buildelidedparamcall : N_elidedparamcall -> call` from `Statements.tex:560`
- [ ] `buildstmtlist : N_stmtlist -> stmt` from `Statements.tex:687`
- [ ] `buildselse : N_selse -> stmt` from `Statements.tex:894`
- [ ] `buildcasealtlist : N_casealtlist -> list1(casealt)` from `Statements.tex:1150`
- [ ] `buildcasealt : N_casealt -> casealt` from `Statements.tex:1168`
- [ ] `buildlooplimit : N_looplimit -> option(expr)` from `Statements.tex:1417`
- [x] `build_direction : N_direction -> for_direction` from `Statements.tex:1669`
- [ ] `buildotherwiseopt : N_otherwiseopt -> option(stmt)` from `Statements.tex:1869`

## Rule Inventory

### `AbstractSyntax.tex`

- [ ] `List`
- [ ] `CList`
- [ ] `PList`
- [ ] `NTCList`
- [ ] `Option`
- [ ] `Identity`

### `Specifications.tex`

- [ ] `AST`

### `GlobalStorageDeclarations.tex`

- [ ] `GlobalStorageDecl`
- [ ] `GlobalDeclKeywordNonConfig`
- [ ] `GlobalDeclKeyword`

### `TypeDeclarations.tex`

- [ ] `TypeDecl`
- [ ] `Subtype`
- [ ] `Subtypeopt`
- [ ] `Fields`

### `SubprogramDeclarations.tex`

- [ ] `SubprogramDecl`
- [ ] `AccessorBody`
- [ ] `RecurseLimit`
- [ ] `TypedIdentifier`
- [ ] `OptTypedIdentifier`
- [ ] `ReturnType`
- [ ] `ParamsOpt`
- [ ] `FuncArgs`
- [ ] `FuncBody`
- [ ] `Accessors`
- [ ] `DesugarAccessorPair`
- [ ] `Qualifier`
- [ ] `IsReadonly`
- [ ] `Override`

### `GlobalPragmas.tex`

- [ ] `GlobalPragma`

### `LocalStorageDeclarations.tex`

- [ ] `LocalDeclKeyword`
- [ ] `DeclItem`
- [ ] `IgnoredOrIdentifier`

### `Literals.tex`

- [ ] `Value` (declared as `ASTRule.Value`, not via `\ASTRuleDef`)

### `PrimitiveOperations.tex`

- [ ] `Unop`
- [ ] `Binop`

### `Types.tex`

- [ ] `Ty.TInt`
- [ ] `IntConstraintsOpt`
- [ ] `IntConstraint`
- [ ] `TReal`
- [ ] `Ty.String`
- [ ] `Ty.BoolType`
- [ ] `Ty.TBits`
- [ ] `Ty.TTuple`
- [ ] `ParenType`
- [ ] `TyDecl.TEnum`
- [ ] `Ty.TArray`
- [ ] `TyDecl.TRecord`
- [ ] `TyDecl.TException`
- [ ] `TyDecl.TCollection`
- [ ] `Ty.TNamed`
- [ ] `TyDecl`

### `Slicing.tex`

- [ ] `Slice`
- [ ] `Slices`

### `AssignableExpressions.tex`

- [ ] `LExpr`
- [ ] `BasicLexpr`
- [ ] `Access`
- [ ] `DiscardOrBasicLexpr`
- [ ] `DiscardOrIdentifier`
- [ ] `DesugarLHSAccess`
- [ ] `DesugarLHSTuple`
- [ ] `DesugarLHSAccessOpt`
- [ ] `DesugarLHSFieldsTuple`
- [ ] `DesugarLHSFieldOpt`

### `Expressions.tex`

- [ ] `ELit`
- [ ] `EVAR`
- [ ] `EBinop`
- [ ] `CheckNotSamePrec`
- [ ] `EUnop`
- [ ] `ECond`
- [ ] `Call`
- [ ] `ECall`
- [ ] `ESlice`
- [ ] `EGetArray`
- [ ] `EGetField`
- [ ] `EGetFields`
- [ ] `ATC`
- [ ] `EPattern`
- [ ] `EArbitrary`
- [ ] `ERecord`
- [ ] `FieldAssign`
- [ ] `ETuple`
- [ ] `ParenExpr`

### `PatternMatching.tex`

- [ ] `PAll`
- [ ] `PSingle`
- [ ] `PRange`
- [ ] `PLeq`
- [ ] `PGeq`
- [ ] `PMask`
- [ ] `PTuple`
- [ ] `PatternSet`
- [ ] `PatternList`
- [ ] `ExprPattern` (declared as `ASTRule.ExprPattern`, not via `\ASTRuleDef`)

### `CatchingExceptions.tex`

- [ ] `Catcher`

### `Statements.tex`

- [ ] `SPass`
- [ ] `SAssign`
- [ ] `MakeSetter`
- [ ] `DesugarSetter`
- [ ] `DesugarSetterSetfields`
- [ ] `ReadModifyWrite`
- [ ] `SetterAssign`
- [ ] `SDecl`
- [ ] `ElidedParamCall`
- [ ] `DesugarElidedParameter`
- [ ] `ElidedParamDecl`
- [ ] `StmtList`
- [ ] `StmtFromList`
- [ ] `SequenceStmts`
- [ ] `SCall`
- [ ] `SetCallType`
- [ ] `SCond`
- [ ] `SElse`
- [ ] `SCase`
- [ ] `CaseAltList`
- [ ] `CaseAlt`
- [ ] `DesugarCaseStmt`
- [ ] `CasesToCond`
- [ ] `CaseToCond`
- [ ] `SAssert`
- [ ] `SWhile`
- [ ] `LoopLimit`
- [ ] `SRepeat`
- [ ] `SFor`
- [ ] `Direction`
- [ ] `SThrow`
- [ ] `STry`
- [ ] `OtherwiseOpt`
- [ ] `SReturn`
- [ ] `SPrint`
- [ ] `SUnreachable`
- [ ] `SPragma`

## Out Of Scope For First Pass

- `TopLevel.tex`:
  - `RenameLocals`
  - `RenameLocalsDecl`
  - `RenameLocalsFunc`
  - `RenameLocalsArgs`
  - `RenameLocalsNamedArgs`
  - `RenameLocalsTy`
  - `RenameLocalsStmt`
  - `RenameLocalsExpr`
  - `RenameLocalsLexpr`
  - `RenameLocalsLDI`
  - `RenameLocalsConstraint`
  - `RenameLocalsSlice`
  - `RenameLocalsName`
  - `RenameLocalsArrayIndex`
  - `RenameLocalsPattern`
  - `RenameLocalsCatcher`
  - `SetBuiltin`
