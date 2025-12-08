# ASL Reference — Working Notes

Purpose: incremental summaries, macro expansions, and issue tracking while reviewing the ASL reference (`ASLReference.tex` and included sections).

## Process
- Read one section at a time.
- Expand relevant LaTeX macros locally (record expansions here).
- Note any logical/consistency issues with file/line references and context.
- Ask the user whether each issue is a problem before proposing fixes.

## Review Policy (canonical)
- Ignore `\identX{...}` macros and any generated identifier noise.
- Ignore pure formatting/rendering issues in LaTeX (line breaks, spaces).
- Focus on logical/consistency errors across grammar → AST → typing → semantics.
- Surface one issue at a time, include precise `file:line` references.
- Follow the order dictated by `ASLReference.tex`, recursively following `\input{...}`.
- `render` elements can be ignored when requested; prioritize DSL truth in `asl.spec`.

## Generation Notes
- Inference rules in `\BackupOriginalRule{...}` are deprecated and not rendered.
- Actual rules are rendered via `\RenderRule{...}` and generated from the DSL (`asl.spec`).
- Trust the DSL (`asl.spec`) as the source of truth for rule generation.
- `prose_application = "..."` strings are currently unused and can be ignored.

## Confirmed Clarifications
- Dynamic semantics: keys in `storage` and `pendingcalls` need not be disjoint.
- `typed_S_Throw` is the only typed statement variant; others remain untyped.
- Naming asymmetry in numeric types (e.g., `T_Int(WellConstrained(...))` vs bare `T_Real`) is intentional.
- Using type names (not parameters) to the right of `->` in operator signatures is correct.
- Grouping multiple related targets in a single `render` statement is acceptable.

## Active Workflow
- Keep `ASL-Review.md` updated after each confirmed fix/clarification.
- Maintain a "Deferred Fixes" list for non-blocking inconsistencies to address later.

## Macro Cheat-Sheet (to be populated)
- Source: `ASLmacros.tex`, `rendering_macros.tex`, `generated_macros.tex`.
- Notation: judgments, typing rules, semantic transition symbols, environment shorthands.

## Section Summaries (incremental)
- Introduction: (TBD)
- Lexical Structure: (TBD)
- Syntax: (TBD)
- Types & Type Checking: (TBD)
- Semantics (Dynamic): (TBD)
- Static Model & Interpreter: (TBD)
- Runtime Environment & Errors: (TBD)

## Issues Found (one-at-a-time log)
# ASL Manual Review Notes

## Deferred Fixes
- Inconsistent constructor naming across AST renders and rule renders:
	- `expr` uses `E_GetField` and `E_GetArray`, while rule renders reference `EGetField.structured.*` and `EGetArray`.
	- Align rule render names to the underscore form (e.g., `E_GetField.structured.record_or_exception`, `E_GetField.structured.collection`, `E_GetArray`) to match AST constructors.
	- References: `asllib/doc/asl.spec` around lines 692–707 (expr renders) and 1965–1970 (rule renders).
	- Additionally, align array access rule render `annotate_expr(EGetArray)` to `annotate_expr(E_GetArray)` to match `render expr_getarray = expr(E_GetArray)`.

- Consider correlating rule names and examples to test artifacts where applicable.

## Session Log (highlights)
- Syntax — Parametric Productions wording corrected to replace instances of `N(S_{1..m})` (confirmed).
- Type System — environment update notation clarified; consistent use applied.
- Semantics — confirmed coexistence of `pendingcalls` and `storage` keys.
- Literals — real rule brace issue noted originally in LaTeX; actual rule generated from DSL (`annotate_literal` in `asl.spec`) verified.
- Operators — `nvstring` parameter type corrected to `Strings`; `string_of_nat` input aligned with intent.
- Prose — minor string fixes (e.g., missing space in `ast call` description) applied.

## Recent Fixes Applied
- `Lang` operator prose clarified ("{l} is the set...").
- `T_Array` prose corrected ("array type" and "element type").
- `array_index` untyped variant bar usage confirmed optional; parser accepts current form.
- `value_read_from` prose switched to "read from {id}".
- `ast call` prose placeholders aligned: `{call_name}` and `{call_args}`.
- Tuple type prose corrected to "component types".
