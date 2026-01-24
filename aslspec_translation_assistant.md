# ASLSpec Translation Assistant

## Workflow for Translating LaTeX Rules to ASLSpec

This is the step-by-step workflow to translate inference rules from LaTeX to ASLSpec implementation code.

### 10-Step Translation Workflow

1. **Scan asl.spec starting from `typing relation annotate_expr`**
   - Look for each `typing relation <name>` or `typing function <name>` declaration
   - Note the line number and relation/function name

2. **Find LaTeX source**
   - Search for `\RenderRelation{<name>}` in the `.tex` files under `doc/`
   - Record the file and line number

3. **Locate LaTeX rules**
   - Scan down from `\RenderRelation{<name>}`
   - Find consecutive blocks of `\begin{mathpar}...\end{mathpar}` instances
   - These LaTeX rules define the formal semantics to be translated

4. **Translate LaTeX rules to ASLSpec**
   - Use the translation patterns and best practices documented below
   - If uncertain about any part, add `// UNCERTAIN: [description]` comments
   - Generate the complete ASLSpec implementation

5. **Modify asl.spec**
   - Locate the relation/function definition in asl.spec
   - Insert `} =` followed by the translated ASLSpec rule before the closing `;`
   - Ensure proper indentation and syntax

6. **Run typechecker and fix errors**
   - Run `dune exec -- aslspec ./asl.spec --render` from `asllib/doc/`
   - Fix any syntax errors, type mismatches, or undefined functions reported
   - Iterate until the typechecker runs successfully
   - This validates the implementation before proceeding

7. **Add RenderRule directive**
   - In the `.tex` file, add `\RenderRule{<name>}` immediately AFTER the LaTeX rules (after `\end{mathpar}`)
   - This tells the PDF renderer to include the ASLSpec implementation

8. **Render and inspect**
   - Stop and ask user to render: `dune exec -- aslspec ./asl.spec --render` from `asllib/doc/`
   - Wait for user to inspect the PDF output
   - User may edit if needed; continue when approved

9. **Wrap LaTeX rules**
   - Once approved, wrap the original LaTeX rules in the `.tex` file with:
     ```latex
     \BackupOriginalRule{
       % Original rules here
     } % END_OF_BACKUP
     ```
   - This preserves the original rules as reference

10. **Move to next relation**
   - Return to step 1
   - Continue with the next `typing relation` or `typing function` in asl.spec

### Important Notes

- **Direct translation only**: Do NOT add or remove premises from the LaTeX rules. Translate them exactly as written.
- **When errors occur**: If you have high confidence in your translation but the typechecker reports errors, the bug may be in:
  - The source LaTeX code itself
  - The function/relation signature in asl.spec
  - In these cases, stop and inform the user so they can fix it manually
- **Uncertain translations**: Always add `// UNCERTAIN: ...` comments if you're not confident about the translation
- **RenderRule placement**: ALWAYS place `\RenderRule{<name>}` AFTER the LaTeX rules, never before
- **Backup rules**: Wrap LaTeX rules with `\BackupOriginalRule{...} % END_OF_BACKUP` only AFTER user approval
- **Comments for review**: Use comments to highlight any translation decisions that might need user verification

---

# LaTeX to ASLSpec Translation Examples

## Purpose
This document contains 67+ real-world examples of inference rule translations from LaTeX mathematical notation to ASLSpec code. Use these examples to understand the translation patterns when converting LaTeX inference rules to ASLSpec definitions.

## Key Translation Patterns

### Basic Pattern Mapping
- **LaTeX `\inferrule{premises}{conclusion}`** → **ASLSpec premise lines ending with `;` followed by `--` and result**
- **LaTeX `\typearrow`** → **ASLSpec `->`**
- **LaTeX `\eqdef`** → **ASLSpec `:=`**
- **LaTeX variable names** → **ASLSpec snake_case function/variable names**
- **LaTeX function calls like `\functionname(...)`** → **ASLSpec `function_name(...)`**

### Structure Translation
1. **Premises** (conditions that must hold):
   - Each becomes an ASLSpec statement ending with `;`
   - Use `->` to assign results: `function_call(...) -> result;`
   - Use `te_check(condition) -> True;` for error-checking premises
   - Use `INDEX(i, list: operation) -> result;` for list comprehensions

2. **Conclusion** (the rule head):
   - Appears after `--` separator in ASLSpec
   - Is the value that the relation/function returns
   - Can include multiple components in tuples

3. **Multiple Cases** (when rules have `[label]`):
   - Translate to ASLSpec `case label { ... }` blocks
   - Each case has its own premises, `--`, and conclusion
   - All cases belong to one function/relation body

### Common Constructs
- **`\or(...) \and(...)` chains** → **ASLSpec `or()` or multiple `case` blocks**
- **Set operations `\bigcup`, `\cap`, `\emptyset`** → **ASLSpec `union()`, `intersect()`, `empty_set`**
- **List/sequence operations** → **ASLSpec `list_...` operators and `INDEX()`**
- **Note**: Layout annotations like `{ math_layout = [...] }` are optional metadata used only when PDF rendering requires specific visual alignment; omit them in initial translations.

### Critical Syntax Rules
- **Conclusion termination**: Each ASLSpec rule conclusion must end with a semicolon. Correct: `(value);` not `(value)` followed by `};` on the next line.
- **Variable naming convention**: Variables ending in `_p` (meaning "primed") should use apostrophe notation instead. Use `t'` not `t_p`, and `t_spec'` not `t_spec_p`, following mathematical convention for primed variables.

## Examples by Category

### Bitfields

#### 1. `annotate_bitfields`

**Source:** Bitfields.tex:198

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \names \eqdef [\vfield\in\fields: \bitfieldgetname(\vfield)]\\
  \checknoduplicates(\names) \typearrow \True \OrTypeError\\\\
  \staticeval(\tenv, \ewidth) \typearrow \LInt(\width) \OrTypeError\\\\
  \vf\in\fields: \annotatebitfield(\tenv, \width, \vfield) \typearrow (\vfp, \vxs_\vf) \OrTypeError\\\\
  \newfields \eqdef [\vf\in\fields: \vfp]\\
  \vses \eqdef \bigcup_{\vf\in\fields}{\vxs_\vf}
}{
  \annotatebitfields(\tenv, \ewidth, \fields) \typearrow (\newfields, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
names := list_map(field, fields, bitfield_get_name(field));
check_no_duplicates(names) -> True;
static_eval(tenv, e_width) -> L_Int(width);
(
  INDEX(i, fields: annotate_bitfield(tenv, width, field) -> (fields'[i], xs[i]))
);
ses := union_list(xs);
--
(fields', ses);
```

---

#### 2. `bitfield_get_name`

**Source:** Bitfields.tex:236

**LaTeX (Source):**

```latex
\begin{mathpar}
  \inferrule[simple]{}{
    \bitfieldgetname(\overname{\BitFieldSimple(\name, \Ignore)}{\vbf}) \typearrow \name
  }
  \and
  \inferrule[nested]{}{
    \bitfieldgetname(\overname{\BitFieldNested(\name, \Ignore, \Ignore)}{\vbf}) \typearrow \name
  }
  \and
  \inferrule[type]{}{
    \bitfieldgetname(\overname{\BitFieldType(\name, \Ignore, \Ignore)}{\vbf}) \typearrow \name
  }
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
or(
  bf =: BitField_Simple(name, _),
  bf =: BitField_Nested(name, _, _),
  bf =: BitField_Type(name, _, _)
);
--
name;
```

---

#### 3. `bitfield_get_slices`

**Source:** Bitfields.tex:273

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[simple]{}{
  \bitfieldgetslices(\overname{\BitFieldSimple(\Ignore, \vslices)}{\vbf}) \typearrow \vslices
}
\and
\inferrule[nested]{}{
  \bitfieldgetslices(\overname{\BitFieldNested(\Ignore, \vslices, \Ignore)}{\vbf}) \typearrow \vslices
}
\and
\inferrule[type]{}{
  \bitfieldgetslices(\overname{\BitFieldType(\Ignore, \vslices, \Ignore)}{\vbf}) \typearrow \vslices
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
or(
  bf =: BitField_Simple(_, slices),
  bf =: BitField_Nested(_, slices, _),
  bf =: BitField_Type(_, slices, _)
);
--
slices;
```

---

#### 5. `annotate_bitfield`

**Source:** Bitfields.tex:371

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[simple]{
  \annotateslices(\tenv, \vslices) \typearrow (\slicesone, \vsesslices) \OrTypeError\\\\
  \commonprefixline\\\\
  \checkslicesinwidth(\tenv, \width, \slicesone) \typearrow \True \OrTypeError
}{
  \annotatebitfield(\tenv, \width, \overname{\BitFieldSimple(\name, \vslices)}{\vfield}) \typearrow \\
  (\overname{\BitFieldSimple(\name, \slicesone)}{\newfield}, \overname{\vsesslices}{\vses})
}
\end{mathpar}

\begin{mathpar}
\inferrule[nested]{
  \annotateslices(\tenv, \vslices) \typearrow (\slicesone, \vsesslices) \OrTypeError\\\\
  \commonprefixline\\\\
  \disjointslicestopositions(\tenv, \True, \slicesone) \typearrow \positions \OrTypeError\\\\
  \checkpositionsinwidth(\tenv, \width, \positions) \typearrow \True \OrTypeError\\\\
  \widthp \eqdef \listlen{\positions}\\
  {
  \begin{array}{r}
  \annotatebitfields(\tenv, \widthp, \bitfieldsp) \typearrow \\ (\bitfieldspp, \vsesbitfields) \OrTypeError
  \end{array}
  }\\
  \vses \eqdef \vsesslices \cup \vsesbitfields
}{
  \annotatebitfield(\tenv, \width, \overname{\BitFieldNested(\name, \vslices, \bitfieldsp)}{\vfield}) \typearrow \\
  (\overname{\BitFieldNested(\slicesone, \bitfieldspp)}{\newfield}, \vses)
}
\end{mathpar}

\begin{mathpar}
\inferrule[type]{
  \annotateslices(\tenv, \vslices) \typearrow (\slicesone, \vsesslices) \OrTypeError\\\\
  \commonprefixline\\\\
  \annotatetype(\tenv, \vt) \typearrow (\vtp, \vsesty) \OrTypeError\\\\
  \checkslicesinwidth(\tenv, \width, \slicesone) \typearrow \True \OrTypeError\\\\
  \disjointslicestopositions(\tenv, \True, \slicesone) \typearrow \positions \OrTypeError\\\\
  \checkpositionsinwidth(\tenv, \slicesone, \width, \positions) \typearrow \True \OrTypeError\\\\
  \widthp \eqdef \listlen{\positions}\\
  \checkbitsequalwidth(\TBits(\widthp, \emptylist), \vt) \typearrow \True \OrTypeError\\\\
  \vses \eqdef \vsesslices \cup \vsesty
}{
  \annotatebitfield(\tenv, \width, \overname{\BitFieldType(\name, \vslices, \vt)}{\vfield}) \typearrow \\
  (\overname{\BitFieldType(\name, \slicesone, \vtp)}{\newfield}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case simple {
  field =: BitField_Simple(name, slices);
  annotate_slices(tenv, slices) -> (slices1, ses_slices);
  check_slices_in_width(tenv, width, slices1) -> True;
  --
  (BitField_Simple(name, slices1), ses_slices)
  { math_layout = [_] };
}

case nested {
  field =: BitField_Nested(name, slices, bitfields');
  annotate_slices(tenv, slices) -> (slices1, ses_slices);
  disjoint_slices_to_positions(tenv, True, slices1) -> positions;
  check_positions_in_width(width, positions) -> True;
  width' := ELint(cardinality(positions));
  annotate_bitfields(tenv, width', bitfields') -> (bitfields'', ses_bitfields)
  { math_layout = [_] };
  ses := union(ses_slices, ses_bitfields);
  --
  (BitField_Nested(name, slices1, bitfields''), ses)
  { math_layout = [_]};
}

case type {
  field =: BitField_Type(name, slices, t);
  annotate_slices(tenv, slices) -> (slices1, ses_slices);
  annotate_type(False, tenv, t) -> (t', ses_ty);
  check_slices_in_width(tenv, width, slices1) -> True;
  disjoint_slices_to_positions(tenv, True, slices1) -> positions;
  check_positions_in_width(width, positions) -> True;
  width' := ELint(cardinality(positions));
  check_bits_equal_width(tenv, T_Bits(width', empty_list), t) -> True;
  ses := union(ses_slices, ses_ty);
  --
  (BitField_Type(name, slices1, t'), ses)
  { math_layout = [_]};
}
```

---

#### 6. `check_slices_in_width`

**Source:** Bitfields.tex:440

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
    \disjointslicestopositions(\tenv, \True, \vslices) \typearrow \positions \OrTypeError\\\\
    \checkpositionsinwidth(\vwidth, \positions) \typearrow \True \OrTypeError
}{
    \checkslicesinwidth(\tenv, \vwidth, \vslices) \typearrow \True
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
disjoint_slices_to_positions(tenv, True, slices) -> positions;
check_positions_in_width(width, positions) -> True;
--
True;
```

---

#### 7. `check_positions_in_width`

**Source:** Bitfields.tex:474

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
    \minpos \eqdef \min(\positions)\\
    \maxpos \eqdef \max(\positions)\\
    \techeck( \leq \minpos \land \maxpos < \vwidth, \BadSlices) \typearrow \True \OrTypeError
}{
    \checkpositionsinwidth(\vwidth, \positions) \typearrow \True
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
min_pos := set_min(positions);
max_pos := set_max(positions);
te_check( zero <= min_pos && max_pos < width, TE_BS ) -> True;
--
True;
```

---

#### 8. `disjoint_slices_to_positions`

**Source:** Bitfields.tex:523

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[empty]{}{
  \disjointslicestopositions(\tenv, \isstatic, \overname{\emptylist}{\vslices}) \typearrow \overname{\emptyset}{\positions}
}
\end{mathpar}

\begin{mathpar}
\inferrule[non\_empty]{
  \vslices = \vs \concat \vslicesone\\
  \bitfieldslicetopositions(\tenv, \isstatic, \vs) \typearrow \positionsoneopt \OrTypeError\\\\
  \positionsone \eqdef \choice{\positionsoneopt = \some{\vsone}}{\vsone}{\emptyset}\\
  \disjointslicestopositions(\tenv, \isstatic, \vslicesone) \typearrow \positionstwoopt \OrTypeError\\\\
  \positionstwo \eqdef \choice{\positionstwoopt = \some{\vstwo}}{\vstwo}{\emptyset}\\
  \techeck(\positionsone \cap \positionstwo = \emptyset, \BadSlices) \typearrow \True \OrTypeError
}{
  \disjointslicestopositions(\tenv, \isstatic, \vslices) \typearrow \overname{\positionsone \cup \positionstwo}{\positions}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case empty {
  slices = empty_list;
  --
  empty_set;
}

case non_empty {
  slices =: match_cons(s, slices1);
  bitfield_slice_to_positions(tenv, is_static, s) -> positions1_opt;
  positions1 := if positions1_opt =: some(s1) then s1 else empty_set;
  disjoint_slices_to_positions(tenv, is_static, slices1) -> positions2;
  te_check(intersect(positions1, positions2) = empty_set, TE_BS) -> True;
  --
  union_finite(positions1, positions2);
}
```

---

#### 9. `bitfield_slice_to_positions`

**Source:** Bitfields.tex:587

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \evalsliceexpr(\tenv, \isstatic, \veone) \typearrow \some{\offset} \terminateas\TypeErrorConfig, \None\\\\
  \evalsliceexpr(\tenv, \isstatic, \vetwo) \typearrow \some{\length} \terminateas\TypeErrorConfig, \None\\\\
  \techeck(\offset \leq \offset + \length - 1, \BadSlices) \typearrow \True \OrTypeError
}{
  {
  \begin{array}{r}
  \bitfieldslicetopositions(\tenv, \isstatic, \overname{\SliceLength(\veone, \vetwo)}{\vslice}) \typearrow\\
  \overname{\some{\{n \;|\; \offset \leq n \leq \offset+\length-1\}}}{\positions}
  \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
slice =: Slice_Length(e1, e2);
eval_slice_expr(tenv, is_static, e1) -> some(offset);
eval_slice_expr(tenv, is_static, e2) -> some(length);
te_check(offset <= offset + length - one, TE_BS) -> True;
--
some(range_set(offset, offset + length - one));
```

---

#### 10. `eval_slice_expr`

**Source:** Bitfields.tex:635

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[static]{
  \staticeval(\tenv, \ve) \typearrow \vz \OrTypeError
}{
  \evalsliceexpr(\tenv, \overname{\True}{\isstatic}, \ve) \typearrow \overname{\some{\vz}}{\vzopt}
}
\end{mathpar}

\begin{mathpar}
\inferrule[symbolic]{
  \reducetozopt(\tenv, \ve) \typearrow \vzopt
}{
  \evalsliceexpr(\tenv, \overname{\False}{\isstatic}, \ve) \typearrow \vzopt
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case static {
  is_static = True;
  static_eval(tenv, e) -> z;
  --
  some(z);
}

case symbolic {
  is_static = False;
  reduce_to_z_opt(tenv, e) -> z_opt;
  --
  z_opt;
}
```

---

#### 11. `check_common_bitfields_align`

**Source:** Bitfields.tex:701

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[empty]{
  \vbitfields = \emptylist
}{
  \checkcommonbitfieldsalign(\tenv, \vbitfields, \overname{0}{\vwidth}) \typearrow \True
}
\end{mathpar}

\begin{mathpar}
\inferrule[non\_empty]{
  \vbitfields \neq \emptylist\\
  \vlastindex \eqdef \vwidth - 1\\
  \vtopabsolute \eqdef (\emptylist, \vlastindex..0)\\
  \bitfieldstoabsolute(\tenv, \vbitfields, \vtopabsolute) \typearrow \vfs\\
  \techeck(\forall \vfone, \vftwo \in \vfs: \absolutebitfieldsalign(\vfone, \vftwo), \BadSlices) \typearrow \True \OrTypeError
}{
  \checkcommonbitfieldsalign(\tenv, \vbitfields, \vwidth) \typearrow \True
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case empty {
  bitfields = empty_list;
  --
  True;
}

case non_empty {
  not_equal(bitfields, empty_list);
  last_index := width - one;
  top_absolute := (empty_list, range_list(last_index, zero));
  bitfields_to_absolute(tenv, bitfields, top_absolute) -> fs;
  te_check(forall(f1, fs, forall(f2, fs, absolute_bitfields_align(f1, f2))), TE_BS) -> True;
  --
  True;
}
```

---

#### 12. `bitfields_to_absolute`

**Source:** Bitfields.tex:738

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \vf\in\vbitfields: \bitfieldtoabsolute(\tenv, \vf, \vabsoluteparent) \typearrow \va_\vf\\
  \vabsbitfields \eqdef \bigcup_{\vf\in\vbitfields} \va_\vf
}{
  \bitfieldstoabsolute(\tenv, \vbitfields, \vabsoluteparent) \typearrow \vabsbitfields
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
abs_field_sets := list_map(i, indices(bitfields), bitfield_to_absolute(tenv, bitfields[i], absolute_parent))
{ math_layout = (_, [_])};
abs_bitfields := union_list(abs_field_sets);
--
abs_bitfields;
```

---

#### 13. `bitfield_to_absolute`

**Source:** Bitfields.tex:774

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \bitfieldgetname(\vbf) \typearrow \name\\
  \vbfname \eqdef \vabsname \concat [\name]\\
  \bitfieldgetslices(\vbf) \typearrow \vslices\\
  \vs \in \vslices: \slicetoindices(\tenv, \vs) \typearrow \indices_\vs\\
  \vslicesasindices \eqdef [\vs \in \vslices: \indices_\vs]\\
  \selectindicesbyslices(\vabsslices, \vslicesasindices) \typearrow \vbfindices\\
  \vbfabsolute \eqdef (\vbfname, \vbfindices)\\
  \bitfieldgetnested(\vbf) \typearrow \vnested\\
  \bitfieldstoabsolute(\tenv, \vnested, \vbfabsolute) \typearrow \vabsbitfieldsone
}{
  {
    \begin{array}{r}
      \bitfieldtoabsolute(\tenv, \vbf, \overname{(\vabsname, \vabsslices)}{\vabsoluteparent}) \typearrow \\
      \overname{\{\vbfabsolute\} \cup \vabsbitfieldsone}{\vabsbitfields}
    \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
bitfield_get_name(bf) -> name;
(absolute_name, absolute_slices) := absolute_parent;
bf_name := concat(absolute_name, make_singleton_list(name));
bitfield_get_slices(bf) -> slices;
INDEX(i, slices: slice_to_indices(tenv, slices[i]) -> indices[i]);
slices_as_indices := list_flatten(indices);
select_indices_by_slices(absolute_slices, slices_as_indices) -> bf_indices;
bf_absolute := (bf_name, bf_indices);
bitfield_get_nested(bf) -> nested;
bitfields_to_absolute(tenv, nested, bf_absolute) -> abs_bitfields1;
--
union(make_set(bf_absolute), abs_bitfields1)
{ math_layout = [_] };
```

---

#### 14. `slice_to_indices`

**Source:** Bitfields.tex:885

**LaTeX (Source):**

```latex
\begin{mathpar}
  \inferrule{
  \staticeval(\tenv, \vi) \typearrow \LInt(\vz_\vi)\\
  \staticeval(\tenv, \vw) \typearrow \LInt(\vz_\vw)\\
  \vstart \eqdef \vz_\vi\\
  \vend \eqdef \vz_\vi + \vz_\vw - 1\\
}{
  \slicetoindices(\tenv, \overname{\SliceLength(\vi, \vw)}{\vs}) \typearrow \overname{\vend..\vstart}{\indices}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
s =: Slice_Length(i, w);
static_eval(tenv, i) -> L_Int(z_i) | ; // This evaluation always succeeds since i is a bound variable.
static_eval(tenv, w) -> L_Int(z_w) | ; // This evaluation always succeeds since i is a bound variable.
v_start := z_i;
v_end := z_i + z_w - one;
--
range(v_end, v_start);
```

---

### Bitvector Slices

#### 1. `annotate_slice`

**Source:** Slicing.tex:172

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[single]{
  \annotateslice(\SliceLength(\vi, \eliteral{1})) \typearrow \vsp \OrTypeError
}{
  \annotateslice(\tenv, \overname{\SliceSingle(\vi)}{\vs}) \typearrow \vsp
}
\end{mathpar}

\begin{mathpar}
\inferrule[range]{
  \binopliterals(\SUB, \vj, \vi) \typearrow \lengthp\\
  \binopliterals(\ADD, \lengthp, \eliteral{1}) \typearrow \length\\
  \annotateslice(\SliceLength(\vi, \length)) \typearrow \vsp \OrTypeError
}{
  \annotateslice(\tenv, \overname{\SliceRange(\vj, \vi)}{\vs}) \typearrow \vsp
}
\end{mathpar}

\begin{mathpar}
\inferrule[length]{
  \vs = \SliceLength(\eoffset, \elength)\\
  \annotateexpr(\tenv, \eoffset) \typearrow (\toffset, \eoffsetp, \vsesoffset) \OrTypeError\\\\
  {
    \begin{array}{r}
  \annotatesymbolicconstrainedinteger(\tenv, \elength) \typearrow \\ (\elengthp, \vseslength) \OrTypeError
  \end{array}
  }\\\\
  \techeck(\sesisreadonly(\vsesoffset), \SideEffectViolation) \typearrow \True \OrTypeError\\\\
  \techeck(\sesisreadonly(\vseslength), \SideEffectViolation) \typearrow \True \OrTypeError\\\\
  \checkunderlyinginteger(\tenv, \toffset) \typearrow \True \OrTypeError\\\\
  \vses \eqdef \vsesoffset \cup \vseslength
}{
  {
    \begin{array}{r}
  \annotateslice(\tenv, \vs) \typearrow \\
    (\overname{\SliceLength(\eoffsetp, \elength')}{\vsp}, \vses)
    \end{array}
  }
}
\end{mathpar}

\begin{mathpar}
\inferrule[scaled]{
  \binopliterals(\MUL, \factor, \length) \typearrow \offset\\
  \annotateslice(\SliceLength(\offset, \length)) \typearrow \vsp \OrTypeError
}{
  \annotateslice(\tenv, \overname{\SliceStar(\factor, \length)}{\vs}) \typearrow \vsp
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case single {
  s = Slice_Single(i);
  annotate_slice(Slice_Length(i, E_Literal(one))) -> s';
}

case range {
  s = Slice_Range(j, i);
  binop_literals(SUB, j, i) -> length';
  binop_literals(ADD, length', ELint(one)) -> length;
  annotate_slice(Slice_Length(i, length)) -> s';
}

case length {
  s = Slice_Length(offset, length);
  annotate_expr(tenv, offset) -> (t_offset, offset', ses_offset);
  annotate_symbolic_constrained_integer(tenv, length) -> (length', ses_length);
  te_check(ses_is_readonly(ses_offset), TE_SEV) -> True;
  te_check(ses_is_readonly(ses_length), TE_SEV) -> True;
  check_underlying_integer(tenv, t_offset) -> True;
  ses := union(ses_offset, ses_length);
  s' := Slice_Length(offset', length');
}

case scaled {
  s = Slice_Star(factor, length);
  binop_literals(MUL, factor, length) -> offset;
  annotate_slice(Slice_Length(offset, length)) -> s';
}
--
(s', ses);
```

---

#### 2. `slices_width`

**Source:** Slicing.tex:253

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[empty]{}{
  \sliceswidth(\tenv, \overname{\emptylist}{\vslices}) \typearrow \overname{\ELInt{0}}{\vwidth}
}
\end{mathpar}

\begin{mathpar}
\inferrule[non\_empty]{
  \slicewidth(\vs) \typearrow \veone\\
  \sliceswidth(\slicesone) \typearrow \vetwo\\
  \normalize(\AbbrevEBinop{\ADD}{\veone}{\vetwo}) \typearrow \vwidth \OrTypeError
}{
  \sliceswidth(\tenv, \overname{[\vs]\concat\slicesone}{\vslices}) \typearrow \vwidth
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case empty {
  slices = empty_list;
  --
  ELint(zero);
}

case non_empty {
  slices = cons(s, slices1);
  slice_width(s) -> e1;
  slices_width(tenv, slices1) -> e2;
  normalize(tenv, EBinop(ADD, e1, e2)) -> width;
  --
  width;
}
```

---

#### 3. `slice_width`

**Source:** Slicing.tex:313

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[single]{}{
  \slicewidth(\overname{\SliceSingle(\Ignore)}{\vslice}) \typearrow \overname{\ELInt{1}}{\vwidth}
}
\and
\inferrule[scaled]{}{
  \slicewidth(\overname{\SliceStar(\Ignore, \ve)}{\vslice}) \typearrow \overname{\ve}{\vwidth}
}
\and
\inferrule[length]{}{
  \slicewidth(\overname{\SliceLength(\Ignore, \ve)}{\vslices}) \typearrow \overname{\ve}{\vwidth}
}
\and
\inferrule[range]{}{
  \slicewidth(\overname{\SliceRange(\veone, \vetwo)}{\vslices}) \typearrow
  \overname{\AbbrevEBinop{\ADD}{\ELInt{1}}{(\AbbrevEBinop{\SUB}{\veone}{\vetwo})}}{\vwidth}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case single {
  slice = Slice_Single(_);
  --
  ELint(one);
}

case scaled {
  slice =: Slice_Star(_, e);
  --
  e;
}

case length {
  slice =: Slice_Length(_, e);
  --
  e;
}

case range {
  slice =: Slice_Range(e1, e2);
  --
  EBinop(ADD, ELint(one), EBinop(SUB, e1, e2));
}
```

---

#### 4. `annotate_symbolic_constrained_integer`

**Source:** Slicing.tex:358

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotatesymbolicallyevaluableexpr(\tenv, \ve) \typearrow (\vt, \vep, \vses) \OrTypeError\\\\
  \checkconstrainedinteger(\tenv, \vt) \typearrow \True \OrTypeError\\\\
  \normalize(\tenv, \vep) \typearrow \vepp
}{
  \annotatesymbolicconstrainedinteger(\tenv, \ve) \typearrow (\vepp, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
annotate_symbolically_evaluable_expr(tenv, e) -> (t, e', ses);
check_constrained_integer(tenv, t) -> True;
normalize(tenv, e') -> e'';
--
(e'', ses);
```

---

#### 5. `eval_slice`

**Source:** Slicing.tex:447

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[single]{
  \evalexpr(\env, \ve) \evalarrow \ResultExpr((\vstart, \newg), \newenv) \OrAbnormal\\
  \vlength \eqdef \nvint(1)
}{
  \evalslice(\env, \SliceSingle(\ve)) \evalarrow (((\vstart, \vlength), \newg), \newenv)
}
\end{mathpar}

\begin{mathpar}
\inferrule[range]{
  \vs = \SliceRange(\etop, \estart)\\
  \evalexpr(\env, \etop) \evalarrow \ResultExpr(\mtop, \envone) \OrAbnormal\\\\
  \mtop \eqname (\vvsubtop, \vgone)\\
  \evalexpr(\envone, \estart) \evalarrow \ResultExpr(\mstart, \newenv) \OrAbnormal\\\\
  \mstart \eqname (\vstart, \vgtwo)\\
  \evalbinop(\SUB, \vvsubtop, \vstart) \evalarrow \vdiff\\
  \evalbinop(\ADD, \nvint(1), \vdiff) \evalarrow \vlength\\
  \newg \eqdef \vgone \parallelcomp \vgtwo
}{
  \evalslice(\env, \vs) \evalarrow (((\vstart, \vlength), \newg), \newenv)
}
\end{mathpar}

\begin{mathpar}
\inferrule[length]{
  \vs = \SliceLength(\estart, \elength)\\
  \evalexpr(\env, \estart) \evalarrow \ResultExpr(\mstart, \envone) \OrAbnormal\\
  \evalexpr(\envone, \elength) \evalarrow \ResultExpr(\mlength, \newenv) \OrAbnormal\\
  \mstart \eqname (\vstart, \vgone)\\
  \mlength \eqname (\vlength, \vgtwo)\\
  \newg \eqdef \vgone \parallelcomp \vgtwo
}{
  \evalslice(\env, \vs) \evalarrow (((\vstart, \vlength), \newg), \newenv)
}
\end{mathpar}

\begin{mathpar}
\inferrule[scaled]{
  \vs = \SliceStar(\efactor, \elength)\\
  \evalexpr(\env, \efactor) \evalarrow \ResultExpr(\mfactor, \envone) \OrAbnormal\\
  \mfactor \eqname (\vfactor, \vgone)\\
  \evalexpr(\envone, \elength) \evalarrow \ResultExpr(\mlength, \newenv) \OrAbnormal\\
  \mlength \eqname (\vlength, \vgtwo)\\
  \evalbinop(\MUL, \vfactor, \vlength) \evalarrow \vstart \\
  \newg \eqdef \vgone \parallelcomp \vgtwo
}{
  \evalslice(\env, \vs) \evalarrow (((\vstart, \vlength), \newg), \newenv)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case single {
  s =: Slice_Single(e);
  eval_expr(env, e) -> ResultExpr((v_start, new_g), new_env);
  v_length := nvint(one);
}

case range {
  s =: Slice_Range(e_top, e_start);
  eval_expr(env, e_top) -> ResultExpr(m_top, env1);
  (v_top, g1) := m_top;
  eval_expr(env1, e_start) -> ResultExpr(m_start, new_env);
  (v_start, g2) := m_start;
  eval_binop(SUB, v_top, v_start) -> v_diff;
  eval_binop(ADD, nvint(one), v_diff) -> v_length;
  new_g := parallel(g1, g2);
}

case length {
  s =: Slice_Length(e_start, e_length);
  eval_expr(env, e_start) -> ResultExpr(m_start, env1);
  (v_start, g1) := m_start;
  eval_expr(env1, e_length) -> ResultExpr(m_length, new_env);
  (v_length, g2) := m_length;
  new_g := parallel(g1, g2);
}

case scaled {
  s =: Slice_Star(e_factor, e_length);
  eval_expr(env, e_factor) -> ResultExpr(m_factor, env1);
  (v_factor, g1) := m_factor;
  eval_expr(env1, e_length) -> ResultExpr(m_length, new_env);
  (v_length, g2) := m_length;
  eval_binop(MUL, v_factor, v_length) -> v_start;
  new_g := parallel(g1, g2);
}
--
((v_start, v_length), new_g), new_env;
```

---

#### 6. `annotate_slices`

**Source:** Slicing.tex:561

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  i\in\listrange(\vslices): \annotateslice(\tenv, \vslices[i]) \typearrow (\vs_i, \vxs_i) \OrTypeError\\\\
  \slicesp \eqdef [i\in\listrange(\vslices): \vs_i]\\
  \vses \eqdef \bigcup_{i\in\listrange(\vslices)} \vxs_i
}{
  \annotateslices(\tenv, \vslices) \typearrow (\slicesp, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
INDEX(i, slices: annotate_slice(tenv, slices[i]) -> (slices'[i], xs[i]));
ses := union_list(xs);
--
(slices', ses);
```

---

#### 7. `eval_slices`

**Source:** Slicing.tex:613

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[empty]{}
{
  \evalslices(\env, \emptylist) \evalarrow \ResultSlices((\emptylist, \emptygraph), \env)
}
\end{mathpar}
\begin{mathpar}
\inferrule[non\_empty]{
  \slices \eqname [\vslice] \concat \slicesone\\
  \evalslice(\env, \vslice) \evalarrow ((\range, \vgone), \envone) \OrAbnormal\\\\
  \evalslices(\envone, \slicesone) \evalarrow \ResultSlices((\rangesone, \vgtwo), \newenv) \OrAbnormal\\\\
  \ranges \eqdef [\range] \concat \rangesone\\
  \newg \eqdef \vgone \parallelcomp \vgtwo
}{
  \evalslices(\env, \slices) \evalarrow \ResultSlices((\ranges, \newg), \newenv)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case empty {
  slices = empty_list;
  --
  ResultSlices((empty_list, empty_graph), env);
}

case non_empty {
  slices = cons(slice, slices1);
  eval_slice(env, slice) -> ((range, g1), env1);
  eval_slices(env1, slices1) -> ResultSlices((ranges1, g2), new_env);
  ranges := cons(range, ranges1);
  new_g := parallel(g1, g2);
  --
  ResultSlices((ranges, new_g), new_env);
}
```
---

### Assignable Expressions

#### 1. `fold_bitvector_fields`

**Source:** AssignableExpressions.tex:1645

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[empty]{}{
  \foldbitvectorfields(\tenv, \vbasefields, \overname{\emptylist}{\vlefields}) \typearrow (\overname{0}{\vlength}, \overname{\emptylist}{\vslices})
}
\end{mathpar}

\begin{mathpar}
\inferrule[non\_empty]{
  \foldbitvectorfields(\tenv, \vbasefields, \vlefieldsone) \typearrow (\vstart, \vslicesone)\OrTypeError\\\\
  \assocopt{\vbasefields}{\vfield} \typearrow \tyopt\\
  \techeck(\tyopt \neq \None, \BadField) \typearrow \True \OrTypeError\\\\
  \tyoptp \eqname \some{\vtfield}\\
  \getbitvectorconstwidth(\tenv, \vtfield) \typearrow \vfieldwidth \OrTypeError\\\\
}{
  {
  \begin{array}{r}
  \foldbitvectorfields(\tenv, \vbasefields, \overname{\vlefieldsone \concat [\vfield]}{\vlefields}) \typearrow\\
  (\overname{\vstart + \vfieldwidth}{\vlength}, \overname{[(\vstart, \vfieldwidth)] \concat \vslicesone}{\vslices})
  \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case empty {
  le_fields = empty_list;
  --
  (zero, empty_list);
}

case non_empty {
  le_fields =: concat(le_fields1, match_singleton_list(field));
  fold_bitvector_fields(tenv, base_fields, le_fields1) -> (start, slices1);
  assoc_opt(base_fields, field) =: ty_opt;
  te_check(ty_opt != None, TE_BF) -> True;
  ty_opt =: some(t_field);
  get_bitvector_const_width(tenv, t_field) -> field_width;
  --
  (start + field_width, concat(make_singleton_list((start, field_width)), slices1))
  { [_] };
}
```

---

### Base Values

#### `base_value`

**Source:** BaseValues.tex:169-290

This function generates initialization expressions for any ASL type. It has 15 cases covering different type constructors. Notable patterns:

- **t_bits_static case**: Uses `list_map(i, range_list(one, length), zero_bit)` to create a bitvector of zeros. Cannot use `INDEX` nested inside `L_Bitvector` constructor - must use `list_map` in the conclusion.
- **t_int_wellconstrained case**: Uses `INDEX` to build an array `z_min_lists` from constraint results, then flattens it.
- **structured case**: Decomposes the type with `make_structured(L, fields)` and `list_combine`, uses `INDEX` to compute base values for field types, then recombines with `list_combine(field_names, field_base_values)`.

**LaTeX:**

```latex
\begin{mathpar}
\inferrule[t\_bool]{}{
    \basevalue(\tenv, \overname{\TBool}{\vt}) \typearrow \overname{\ELiteral(\LBool(\False))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_bits\_static]{
    \reducetozopt(\tenv, \ve) \typearrow \vzopt\\
    \vzopt \neq \None\\\\
    \vzopt \eqname \some{\length}\\
    \techeck(\length \geq 0, \NoBaseValue) \typearrow \True\OrTypeError
}{
    \basevalue(\tenv, \overname{\TBits(\ve, \Ignore)}{\vt}) \typearrow \overname{\ELiteral(\LBitvector(i=1..\length: 0))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_bits\_non\_static]{
    \reducetozopt(\tenv, \ve) \typearrow \vzopt\\
    \vzopt = \None \\\\
    \veinit \eqdef \ESlice (\ELInt{0}, [\SliceLength(\ELInt{0}, \ve)])
}{
    \basevalue(\tenv, \overname{\TBits(\ve, \Ignore)}{\vt}) \typearrow \veinit
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_enum]{%
    \lookupconstant(\tenv, \name) \typearrow \vl
}{%
    \basevalue(\tenv, \overname{\TEnum(\name \concat \Ignore)}{\vt}) \typearrow \overname{\ELiteral(\vl)}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_int\_unconstrained]{}{
    \basevalue(\tenv, \overname{\unconstrainedinteger}{\vt}) \typearrow \overname{\ELiteral(\LInt(0))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_int\_parameterized]{}{
    \basevalue(\tenv, \overname{\TInt(\Parameterized(\id))}{\vt}) \typearrow \TypeErrorVal{\NoBaseValue}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_int\_wellconstrained]{
    \cs \eqname \vc_{1..k}\\
    \vzminlist \eqdef \constraintabsmin(\tenv, \vc_1) \concat \ldots \concat \constraintabsmin(\tenv, \vc_k)\\
    \techeck(\vzminlist \neq \emptyset, \NoBaseValue) \typearrow \True \OrTypeError\\\\
    \listminabs(\vzminlist) \typearrow \vzmin
}{
    \basevalue(\tenv, \overname{\TInt(\WellConstrained(\cs))}{\vt}) \typearrow \overname{\ELiteral(\LInt(\vzmin))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_named]{
    \makeanonymous(\tenv, \TNamed(\id)) \typearrow \vtp \OrTypeError\\\\
    \basevalue(\tenv, \vtp) \typearrow \veinit \OrTypeError
}{
    \basevalue(\tenv, \overname{\TNamed(\id)}{\vt}) \typearrow \veinit
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_real]{}{
    \basevalue(\tenv, \overname{\TReal}{\vt}) \typearrow \overname{\ELiteral(\LReal(0))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[structured]{
    \isstructured(\vt) \typearrow \True\\
    \vt \eqname L(\fields)\\
    (\name, \vtefield) \in \fields: \basevalue(\tenv, \vtefield) \typearrow \ve_\name \OrTypeError
}{
    \basevalue(\tenv, \vt) \typearrow \overname{\ERecord(\vt, (\name, \vtefield) \in \fields: (\name, \ve_\name))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_string]{}{
    \basevalue(\tenv, \overname{\TString}{\vt}) \typearrow \overname{\ELiteral(\LString(\emptylist))}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_tuple]{
    \vi=1..k: \basevalue(\tenv, \vt_\vi) \typearrow \ve_\vi \OrTypeError
}{
    \basevalue(\tenv, \overname{\TTuple}{\vt_{1..k}}) \typearrow \overname{\ETuple(\ve_{1..k})}{\veinit}
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_array\_enum]{
    \basevalue(\tenv, \tty) \typearrow \vvalue \OrTypeError
}{
    {
        \begin{array}{r}
            \basevalue(\tenv, \overname{\TArray(\ArrayLengthEnum(\venum, \vlabels), \tty)}{\vt}) \typearrow \\
            \overname{\EEnumArray\{\FIELDenum: \venum, \FIELDlabels: \vlabels, \enumarrayvalue: \vvalue\}}{\veinit}
        \end{array}
    }
}
\end{mathpar}

\begin{mathpar}
\inferrule[t\_array\_expr]{
    \basevalue(\tenv, \tty) \typearrow \vvalue \OrTypeError
}{
    {
        \begin{array}{r}
            \basevalue(\tenv, \overname{\TArray(\ArrayLengthExpr(\length), \tty)}{\vt}) \typearrow\\
            \overname{\EArray\{\EArrayLength: \length, \EArrayValue: \vvalue\}}{\veinit}
        \end{array}
    }
}
\end{mathpar}
```

**ASLSpec Translation:**

```aslspec
typing function base_value(tenv: static_envs, t: ty) ->
         (e_init: expr) | type_error
{
  "returns the expression {e_init} which can be used to
  initialize a storage element of type {t} in the
  \staticenvironmentterm{} {tenv}.
  \ProseOtherwiseTypeError",
  prose_application = "\hyperlink{relation-basevalue}{computing} initial value for type {t} in {tenv} yields expression {e_init}",
} =
  case t_bool {
    t = T_Bool;
    --
    E_Literal(L_Bool(False));
  }

  case t_bits_static {
    t =: T_Bits(e, _);
    reduce_to_z_opt(tenv, e) -> z_opt;
    z_opt != None;
    z_opt =: some(length);
    te_check(length >= zero, TE_NBV) -> True;
    --
    E_Literal(L_Bitvector(list_map(i, range_list(one, length), zero_bit)));
  }

  case t_bits_non_static {
    t =: T_Bits(e, _);
    reduce_to_z_opt(tenv, e) -> z_opt;
    z_opt = None;
    e_init := E_Slice(ELint(zero), make_singleton_list(Slice_Length(ELint(zero), e)));
    --
    e_init;
  }

  case t_enum {
    t =: T_Enum(match_non_empty_cons(name, _));
    lookup_constant(tenv, name) -> l;
    --
    E_Literal(l);
  }

  case t_int_unconstrained {
    t = T_Int(Unconstrained);
    --
    E_Literal(L_Int(zero));
  }

  case t_int_parameterized {
    t =: T_Int(Parameterized(id));
    --
    TypeError(TE_NBV);
  }

  case t_int_wellconstrained {
    t =: T_Int(WellConstrained(cs));
    INDEX(i, cs: constraint_abs_min(tenv, cs[i]) -> z_min_lists[i]);
    z_min_list := list_flatten(z_min_lists);
    te_check(z_min_list != empty_list, TE_NBV) -> True;
    list_min_abs(z_min_list) -> z_min;
    --
    E_Literal(L_Int(z_min));
  }

  case t_named {
    t =: T_Named(id);
    make_anonymous(tenv, T_Named(id)) -> t';
    base_value(tenv, t') -> e_init;
    --
    e_init;
  }

  case t_real {
    t = T_Real;
    --
    E_Literal(L_Real(rational_zero));
  }

  case structured {
    is_structured(t) -> True;
    t =: make_structured(L, fields);
    fields =: list_combine(field_names, field_types);
    ( INDEX(i, field_types: base_value(tenv, field_types[i]) -> field_base_values[i]) ) { ( [_] ) };
    e := list_combine(field_names, field_base_values);
    --
    E_Record(t, e);
  }

  case t_string {
    t = T_String;
    --
    E_Literal(L_String(empty_list));
  }

  case t_tuple {
    t =: T_Tuple(ts);
    INDEX(i, ts: base_value(tenv, ts[i]) -> es[i]);
    --
    E_Tuple(es);
  }

  case t_array_enum {
    t =: T_Array(ArrayLength_Enum(enum, labels), ty);
    base_value(tenv, ty) -> value;
    --
    E_EnumArray [ enum : enum, labels : labels, enum_array_value : value ];
  }

  case t_array_expr {
    t =: T_Array(ArrayLength_Expr(length), ty);
    base_value(tenv, ty) -> value;
    --
    E_Array[ length : length, array_value : value ];
  }
;
```

---

### Block Statements

#### 1. `annotate_block`

**Source:** BlockStatements.tex:56

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotatestmt(\tenv, \vs) \typearrow (\newstmt, \Ignore, \vses) \OrTypeError
}{
  \annotateblock(\tenv, \vs) \typearrow (\newstmt, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
annotate_stmt(tenv, s) -> (new_stmt, _, ses);
--
(new_stmt, ses);
```

---

### Expressions

#### 1. `annotate_expr_ELit`

**Source:** Expressions.tex:104

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \ve = \ELiteral(\vv)\\
  \annotateliteral(\tenv, \vv) \typearrow \vt
}{
  \annotateexpr(\tenv, \ve) \typearrow (\vt, \overname{\ve}{\newe}, \overname{\emptyset}{\vses})
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case ELit {
  e =: E_Literal(v);
  annotate_literal(tenv, v) -> t;
  --
  (t, e, empty_set);
}
```
---

#### 2. `annotate_expr_EVar`

**Source:** Expressions.tex:212

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[local]{
  \tenv.\staticenvsL.\localstoragetypes(\vx) =  (\vt, k) \\
  \sesldk(k) \typearrow \vses
}{
  \annotateexpr(\tenv, \overname{\EVar(\vx)}{\ve}) \typearrow (\vt, \overname{\EVar(\vx)}{\newe}, \vses)
}
\end{mathpar}

\begin{mathpar}
\inferrule[global\_constant]{
  \tenv.\staticenvsL.\localstoragetypes(\vx) = \bot \\
  \tenv.\staticenvsG.\globalstoragetypes(\vx) = (\tty, \GDKConstant)\\
  \tenv.\staticenvsG.\constantvalues(\vx) = \vv
}{
  \annotateexpr(\tenv, \overname{\EVar(\vx)}{\ve}) \typearrow (\tty, \overname{\eliteral{\vv}}{\newe}, \overname{\emptyset}{\vses})
}
\end{mathpar}

\begin{mathpar}
\inferrule[global\_non\_constant]{
  \tenv.\staticenvsL.\localstoragetypes(\vx) = \bot \\
  \tenv.\staticenvsG.\globalstoragetypes(\vx) = (\tty, k)\\
  \tenv.\staticenvsG.\constantvalues(\vx) = \bot \lor k \neq \GDKConstant\\
  \sesgdk(k) \typearrow \vses
}{
  \annotateexpr(\tenv, \overname{\EVar(\vx)}{\ve}) \typearrow (\tty, \overname{\EVar(\vx)}{\newe}, \vses)
}
\end{mathpar}

\begin{mathpar}
\inferrule[error\_undefined]{
  \tenv.\staticenvsL.\localstoragetypes(\vx) = \bot \\
  \tenv.\staticenvsG.\globalstoragetypes(\vx) = \bot
}{
  \annotateexpr(\tenv,\overname{\EVar(\vx)}{\ve}) \typearrow \TypeErrorVal{\UndefinedIdentifier}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EVar {
  e =: E_Var(x);
  case local {
    tenv.static_envs_L.local_storage_types(x) = (t, k);
    ses_ldk(k) -> ses;
    --
    (t, E_Var(x), ses);
  }

  case global {
    tenv.static_envs_L.local_storage_types(x) = bot;
    tenv.static_envs_G.global_storage_types(x) = (ty, k);
    case const {
      k = GDK_Constant;
      tenv.static_envs_G.constant_values(x) = v;
      --
      (ty, E_Literal(v), empty_set);
    }

    case non_const {
      k != GDK_Constant || tenv.static_envs_G.constant_values(x) = bot;
      ses_gdk(k) -> ses;
      --
      (ty, E_Var(x), ses);
    }
  }

  case error_undefined {
    tenv.static_envs_L.local_storage_types(x) = bot;
    tenv.static_envs_G.global_storage_types(x) = bot;
    --
    TypeError(TE_UI);
  }
}
```
---

#### 3. `annotate_expr_EBinop`

**Source:** Expressions.tex:567

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \veone) \typearrow (\vtone, \veonep, \vsesone) \OrTypeError\\\\
  \annotateexpr(\tenv, \vetwo) \typearrow (\vttwo, \vetwop, \vsestwo) \OrTypeError\\\\
  \applybinoptypes(\tenv, \op, \vtone, \vttwo) \typearrow \vt \OrTypeError\\\\
  \vses \eqdef \vsesone \cup \vsestwo
}{
  \annotateexpr(\tenv, \overname{\EBinop(\op, \veone, \vetwo)}{\ve}) \typearrow (\vt, \overname{\EBinop(\op, \veone', \vetwo')}{\newe}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EBinop {
  e =: E_Binop(op, e1, e2);
  annotate_expr(tenv, e1) -> (t1, e1', ses1);
  annotate_expr(tenv, e2) -> (t2, e2', ses2);
  apply_binop_types(tenv, op, t1, t2) -> t;
  ses := union(ses1, ses2);
  --
  (t, E_Binop(op, e1', e2'), ses);
}
```
---

#### 4. `annotate_expr_EUnop`

**Source:** Expressions.tex:809

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vep) \typearrow (\vtpp, \vepp, \vses) \OrTypeError\\\\
  \applyunoptype(\tenv, \op, \vtpp) \typearrow \vt \OrTypeError
}{
  \annotateexpr(\tenv, \overname{\EUnop(\op, \vep)}{\ve}) \typearrow (\vt, \EUnop(\op, \vepp), \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EUnop {
  e =: E_Unop(op, e');
  annotate_expr(tenv, e') -> (t'', e'', ses);
  apply_unop_type(tenv, op, t'') -> t;
  --
  (t, E_Unop(op, e''), ses);
}
```
---

#### 5. `annotate_expr_ECond`

**Source:** Expressions.tex:902

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \econd) \typearrow (\tcond, \econd', \vsescond) \OrTypeError\\\\
  \annotateexpr(\tenv, \etrue) \typearrow (\ttrue, \etrue', \vsestrue) \OrTypeError\\\\
  \annotateexpr(\tenv, \efalse) \typearrow (\tfalse, \efalse', \vsesfalse) \OrTypeError\\\\
  \lca(\ttrue, \tfalse) \typearrow \vt \OrTypeError\\\\
  \vses \eqdef \vsescond \cup \vsestrue \cup \vsesfalse
}{
  {
    \begin{array}{r}
  \annotateexpr(\tenv, \overname{\ECond(\econd, \etrue, \efalse)}{\ve}) \typearrow \\
  (\vt, \ECond(\econdp, \etruep, \efalsep), \vses)
    \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case ECond {
  e =: E_Cond(e_cond, e_true, e_false);
  annotate_expr(tenv, e_cond) -> (t_cond, e_cond', ses_cond);
  check_structure_label(tenv, t_cond, label_T_Bool) -> True;
  annotate_expr(tenv, e_true) -> (t_true, e_true', ses_true);
  annotate_expr(tenv, e_false) -> (t_false, e_false', ses_false);
  lowest_common_ancestor(tenv, t_true, t_false) -> t;
  ses := union(ses_cond, ses_true, ses_false);
  --
  (t, E_Cond(e_cond', e_true', e_false'), ses);
}
```
---

#### 6. `annotate_expr_ECall`

**Source:** Expressions.tex:1090

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotatecall(\vcall) \typearrow (\vcallp, \some{ \vt }, \vses) \OrTypeError
}{
  \annotateexpr(\tenv, \overname{\ECall(\vcall)}{\ve}) \typearrow (\vt, \overname{\ECall(\vcallp)}{\newe}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case ECall {
  e = E_Call(call);
  annotate_call(tenv, call) -> (call', some(t), ses);
  --
  (t, E_Call(call'), ses);
}
```
---

#### 7. `annotate_expr_ESlice`

**Source:** Expressions.tex:1245

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vep) \typearrow (\tep, \vepp, \vsesone) \OrTypeError\\\\
  \tstruct(\tenv, \tep) \typearrow \structtep \OrTypeError\\\\
  \astlabel(\structtep) \in \{\TInt, \TBits\}\\
  \techeck(\slices \neq \emptylist, \BadSlices) \typearrow \True \OrTypeError\\\\
  \annotateslices(\tenv, \slices) \typearrow (\slicesp, \vsestwo) \OrTypeError\\\\
  \sliceswidth(\tenv, \slices) \typearrow \vw \OrTypeError\\\\
  \vses \eqdef \vsesone \cup \vsestwo
}{
  {
    \begin{array}{r}
  \annotateexpr(\tenv, \overname{\ESlice(\vep, \slices)}{\ve}) \typearrow\\
  (\overname{\TBits(\vw, \emptylist)}{\vt}, \overname{\ESlice(\vepp, \slicesp)}{\newe}, \vses)
    \end{array}
  }
}
\end{mathpar}

\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vep) \typearrow (\tep, \vepp) \OrTypeError\\\\
  \tstruct(\tenv, \tep) \typearrow \vtp\\
  \astlabel(\vtp) \not\in \{\TInt, \TBits\}
}{
  \annotateexpr(\tenv, \overname{\ESlice(\vep, \slices)}{\ve}) \typearrow \TypeErrorVal{\BadSlices}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
  case ESlice {
    e = E_Slice(e', slices);
    annotate_expr(tenv, e') -> (t_e', e'', ses1);
    get_structure(tenv, t_e') -> struct_t_e';
    case okay {
        ast_label(struct_t_e') in make_set(label_T_Int, label_T_Bits);
      te_check(slices != empty_list, TE_BS) -> True;
      annotate_slices(tenv, slices) -> (slices', ses2);
      slices_width(tenv, slices) -> w;
      ses := union(ses1, ses2);
      --
      (T_Bits(w, empty_list), E_Slice(e'', slices'), ses);
    }
    case error {
        ast_label(struct_t_e') not_in make_set(label_T_Int, label_T_Bits);
      --
      TypeError(TE_BS);
    }
  }
```
---

#### 8. `annotate_expr_EGetArray`

**Source:** Expressions.tex:1386

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \ebase) \typearrow (\tbase, \ebasep, \vsesbase) \OrTypeError\\\\
  \makeanonymous(\tenv, \tbase) \typearrow \tanonbase \OrTypeError\\\\
  \techeck(\astlabel(\tanonbase) = \TArray, \UnexpectedType) \typearrow \True \OrTypeError\\\\
  \tanonbase \eqname \TArray(\size, \telem)\\
  {
    \begin{array}{r}
  \annotategetarray(\tenv, (\size, \telem), (\ebasep, \vsesbase, \eindex)) \typearrow \\ (\vt, \newe, \vses)
    \end{array}
  }
}{
  \annotateexpr(\tenv, \overname{\EGetArray(\ebase, \eindex)}{\ve}) \typearrow (\vt, \newe, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EGetArray {
  e = E_GetArray(e_base, e_index);
  annotate_expr(tenv, e_base) -> (t_base, e_base', ses_base);
  make_anonymous(tenv, t_base) -> t_anon_base;
  te_check(ast_label(t_anon_base) = T_Array, TE_UT) -> True;
  t_anon_base =: T_Array(size, t_elem);
  annotate_get_array(tenv, (size, t_elem), (e_base', ses_base, e_index)) -> (t, new_e, ses)
  { math_layout = [_] };
  --
  (t, new_e, ses);
}
```
---

#### 14. `annotate_expr_EGetFields`

**Source:** Expressions.tex:2013

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[bits]{
  \annotateexpr(\tenv, \ebase) \typearrow (\tbaseannot, \ebaseannot, \vsesbase) \OrTypeError\\\\
  \makeanonymous(\tenv, \tbaseannot) \typearrow \TBits(\Ignore, \vbitfields) \OrTypeError\\\\
  \name\in\vfields: \findbitfieldsslices(\name, \vbitfields) \typearrow \vslices_\name \OrTypeError\\\\
  \veslice \eqdef \ESlice(\ebase, [\name\in\vfields: \vslices_\name])\\
  \annotateexpr(\tenv, \veslice) \typearrow (\vt, \newe, \vses) \OrTypeError
}{
  \annotateexpr(\tenv, \overname{\EGetFields(\ebase, \vfields)}{\ve}) \typearrow (\vt, \newe, \vses)
}
\end{mathpar}

\begin{mathpar}
\inferrule[record]{
  \annotateexpr(\tenv, \ebase) \typearrow (\tbaseannot, \ebaseannot, \vsesbase) \OrTypeError\\\\
  \makeanonymous(\tenv, \tbaseannot) \typearrow \TRecord(\vbasefields) \OrTypeError\\\\
  \vf \in \vbasefields: \getbitfieldwidth(\tenv, \vf, \tfields) \typearrow \ewidth_\vf \OrTypeError\\\\
  \widthplus(\tenv, [\vf \in \vbasefields: \ewidth_\vf]) \typearrow \veslicewidth\OrTypeError
}{
  {
  \begin{array}{r}
    \annotateexpr(\tenv, \overname{\EGetFields(\ebase, \vfields)}{\ve}) \typearrow \\
    (\overname{\TBits(\veslicewidth, \emptylist)}{\vt}, \overname{\EGetFields(\ebaseannot, \vfields)}{\newe}, \overname{\vsesbase}{\vses})
  \end{array}
  }
}
\end{mathpar}

\begin{mathpar}
\inferrule[collection]{
  \annotateexpr(\tenv, \ebase) \typearrow (\tbaseannot, \ebaseannot, \vsesbase) \OrTypeError\\\\
  \ebaseannot = \EVar(\vbase)\\
  \makeanonymous(\tenv, \tbaseannot) \typearrow \TRecord(\vbasefields) \OrTypeError\\\\
  \vf \in \vbasefields: \getbitfieldwidth(\tenv, \vf, \tfields) \typearrow \ewidth_\vf \OrTypeError\\\\
  \widthplus(\tenv, [\vf \in \vbasefields: \ewidth_\vf]) \typearrow \veslicewidth\OrTypeError\\
  \vt \eqdef \TBits(\veslicewidth, \emptylist)\\
  \newe \eqdef \EGetCollectionFields(\vbase, \vbasefields)
}{
  {
  \begin{array}{r}
    \annotateexpr(\tenv, \overname{\EGetFields(\ebase, \vfields)}{\ve}) \typearrow \\
    (\vt, \newe, \overname{\vsesbase}{\vses})
  \end{array}
  }
}
\end{mathpar}

\begin{mathpar}
\inferrule[error]{
  \annotateexpr(\tenv, \veone) \typearrow (\tbaseannot, \ebaseannot, \Ignore) \OrTypeError\\\\
  \makeanonymous(\tenv, \tbaseannot) \typearrow \tbaseannotanon \OrTypeError\\\\
  \astlabel(\tbaseannotanon) \not\in \{\TBits, \TRecord\}
}{
  \annotateexpr(\tenv, \overname{\EGetFields(\veone, \vfields)}{\ve}) \typearrow \TypeErrorVal{\UnexpectedType}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EGetFields {
  e =: E_GetFields(e_base, fields);
  annotate_expr(tenv, e_base) -> (t_base_annot, e_base_annot, ses_base);

  case bits {
    make_anonymous(tenv, t_base_annot) -> T_Bits(_, bitfields);
    INDEX(i, fields: find_bitfields_slices(fields[i], bitfields) -> slices[i]);
    e_slice := E_Slice(e_base, list_flatten(slices));
    annotate_expr(tenv, e_slice) -> (t, new_e, ses);
    --
    (t, new_e, ses);
  }
  case record {
    make_anonymous(tenv, t_base_annot) -> T_Record(base_fields);
    INDEX(i, fields: get_bitfield_width(tenv, fields[i], base_fields) -> e_width[i]);
    width_plus(tenv, e_width) -> e_slice_width;
    --
    (T_Bits(e_slice_width, empty_list), E_GetFields(e_base_annot, fields), ses_base)
    { math_layout = (_, [_]) };
  }
  case collection {
    make_anonymous(tenv, t_base_annot) -> T_Collection(base_fields);
    e_base_annot =: E_Var(base_collection_name);
    INDEX(i, fields: get_bitfield_width(tenv, fields[i], base_fields) -> e_width[i]);
    width_plus(tenv, e_width) -> e_slice_width;
    --
    (T_Bits(e_slice_width, empty_list), E_GetCollectionFields(base_collection_name, fields), ses_base)
    { math_layout = [_, [_]] };
  }
  case error {
    make_anonymous(tenv, t_base_annot) -> t_base_annot_anon;
    ast_label(t_base_annot_anon) not_in make_set(label_T_Bits, label_T_Record, label_T_Collection);
    --
    TypeError(TE_UT);
  }
}
```
---

#### 15. `annotate_expr_EATC`

**Source:** Expressions.tex:2404

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vep) \typearrow (\vt, \vepp, \vsese) \OrTypeError\\\\
  \tstruct(\tenv, \vt) \typearrow \vtstruct \OrTypeError\\\\
  \annotatetype(\tenv, \tty) \typearrow (\ttyp, \vsesty) \OrTypeError\\\\
  \tstruct(\tenv, \ttyp) \typearrow \vtystruct \OrTypeError\\\\
  \checkatc(\tenv, \vtstruct, \vtystruct) \typearrow \True \OrTypeError\\\\
  \vsesp \eqdef \vsesty \cup \vsese \\
  \subtypesatisfies(\tenv, \vtstruct, \vtystruct) \typearrow \valwayssucceeds \OrTypeError\\\\
  (\newe, \vses) \eqdef \choice{\valwayssucceeds}{(\vepp, \vsese)}{(\EATC(\vepp, \ttyp), \vsesp)}
}{
  \annotateexpr(\tenv, \overname{\EATC(\vep, \tty)}{\ve}) \typearrow (\overname{\ttyp}{\vt}, \newe, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EATC {
  e =: E_ATC(e', ty);
  annotate_expr(tenv, e') -> (t, e'', ses_e);
  get_structure(tenv, t) -> t_struct;
  annotate_type(False, tenv, ty) -> (ty', ses_ty);
  get_structure(tenv, ty') -> ty_struct;
  check_atc(tenv, t_struct, ty_struct) -> True;
  ses' := union(ses_ty, ses_e);
  subtype_satisfies(tenv, t_struct, ty_struct) -> always_succeeds;
  (new_e, ses) := if always_succeeds then (e'', ses_e) else (E_ATC(e'', ty'), ses');
  --
  (ty', new_e, ses);
}
```
---

#### 16. `annotate_expr_EPattern`

**Source:** Expressions.tex:2853

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \ve = \EPattern(\veone, \vpat)\\
  \annotateexpr(\tenv, \veone) \typearrow (\vtetwo, \vetwo, \vsese) \OrTypeError\\\\
  \annotatepattern(\tenv, \vtetwo, \vpat) \typearrow (\vpatp, \vsespat) \OrTypeError\\\\
  \vses \eqdef \vsese \cup \vsespat
}{
  \annotateexpr(\tenv, \ve) \typearrow (\overname{\TBool}{\vt}, \overname{\EPattern(\vetwo, \vpatp)}{\newe}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EPattern {
  e =: E_Pattern(e1, pat);
  annotate_expr(tenv, e1) -> (t_e2, e2, ses_e);
  annotate_pattern(tenv, t_e2, pat) -> (pat', ses_pat);
  ses := union(ses_e, ses_pat);
  --
  (T_Bool, E_Pattern(e2, pat'), ses);
}
```
---

#### 17. `annotate_expr_EArbitrary`

**Source:** Expressions.tex:2952

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \ve = \EArbitrary(\tty)\\
  \annotatetype(\tenv, \tty) \typearrow (\ttyone, \vsesty) \OrTypeError\\\\
  \tstruct(\tenv, \ttyone) \typearrow \ttytwo \OrTypeError\\\\
  {
  \vses \eqdef \vsesty \cup \left\{
  \begin{array}{l}
  \LocalEffect(\SEReadonly),\\
  \GlobalEffect(\SEReadonly),\\
  \Immutability(\False)
  \end{array}
  \right\}
  }
}{
  \annotateexpr(\tenv, \ve) \typearrow (\ttyone, \EArbitrary(\ttytwo), \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case EArbitrary {
  e =: E_Arbitrary(ty);
  annotate_type(False, tenv, ty) -> (ty1, ses_ty);
  get_structure(tenv, ty1) -> ty2;
  ses := union(ses_ty,
        make_set(
          LocalEffect(SE_Readonly),
          GlobalEffect(SE_Readonly),
          Immutability(False)))
  { math_layout = (lhs, (_, [_])) };
  --
  (ty1, E_Arbitrary(ty2), ses);
}
```
---

#### 18. `annotate_expr_ERecord`

**Source:** Expressions.tex:3140

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \ve = \ERecord(\tty, \vfields)\\
  \techeck(\astlabel(\tty) = \TNamed, \UnexpectedType) \typearrow \True \OrTypeError\\\\
  \makeanonymous(\tenv, \tty) \typearrow \ttyanon \OrTypeError\\\\
  \techeck(\astlabel(\ttyanon) \in \{\TRecord, \TException\}, \UnexpectedType) \typearrow \True\OrTypeError\\\\
  \ttyanon \eqname L(\fieldtypes)\\
  \initializedfields \eqdef \{\name \;|\; (\name, \Ignore)\in\vfields\}\\
  \names \eqdef \fieldnames(\fieldtypes)\\
  \techeck(\{\names\} = \{\initializedfields\}, \BadField) \typearrow \True \OrTypeError\\\\
  \checknoduplicates(\initializedfields) \typearrow \True \OrTypeError\\\\
  {
    \begin{array}{r}
  (\name, \vep) \in \vfields: \annotatefieldinit(\tenv, (\name, \vep), \fieldtypes) \typearrow \\
  (\name, \ve_\name, \vxs_\name) \OrTypeError
    \end{array}
  }\\
  \fieldsp \eqdef [(\name, \vep) \in \fields : (\name, \ve_\name)]\\
  \vses \eqdef \bigcup_{(\name, \Ignore) \in \fields} \vxs_\name\\
}{
  \annotateexpr(\tenv, \ve) \typearrow
  (\overname{\tty}{\vt}, \overname{\ERecord(\tty, \fieldsp)}{\newe}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case ERecord {
  e =: E_Record(ty, fields);
  te_check(is_named(ty), TE_UT) -> True;
  make_anonymous(tenv, ty) -> ty_anon;
  te_check(is_structured(ty_anon), TE_UT) -> True;
  ty_anon =: make_structured(L, field_types);
  initialized_fields := list_fst(fields);
  names := list_fst(field_types);
  te_check(make_set(names) = make_set(initialized_fields), TE_BF) -> True;
  check_no_duplicates(initialized_fields) -> True;
  INDEX(i, fields: annotate_field_init(tenv, fields[i], field_types) ->
                   (field_names[i], field_inits[i], field_effects[i]))
  { math_layout = (_, [_]) };
  fields' := list_combine(field_names, field_inits);
  ses := union_list(field_effects);
  --
  (ty, E_Record(ty, fields'), ses);
}
```
---

#### 19. `annotate_expr_ETuple`

**Source:** Expressions.tex:3281

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[parenthesized]{
  \annotateexpr(\tenv, \vep) \typearrow (\vt, \newe, \vses) \OrTypeError
}{
  \annotateexpr(\tenv, \overname{\ETuple(\vep)}{\ve}) \typearrow (\vt, \newe, \vses)
}
\end{mathpar}

\begin{mathpar}
\inferrule[list]{
  |\vli| > 1\\
  i=1..k: \annotateexpr(\tenv, \vle[i]) \typearrow (\vt_i, \ve_i, \vxs_i) \OrTypeError\\\\
  \vses \eqdef \bigcup_{i=1..k} \vxs_i
}{
  \annotateexpr(\tenv, \overname{\ETuple(\vli)}{\ve}) \typearrow (\overname{\TTuple(\vt_{1..k})}{\vt}, \overname{\ETuple(\ve_{1..k})}{\newe}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case ETuple {
  e =: E_Tuple(li);
  case parenthesized {
    li =: make_singleton_list(e');
    annotate_expr(tenv, e') -> (t, new_e, ses);
    --
    (t, new_e, ses);
  }

  case list {
    list_len(li) > one;
    INDEX(i, li: annotate_expr(tenv, li[i]) -> (t[i], es[i], xs[i]));
    ses := union_list(xs);
    --
    (T_Tuple(t), E_Tuple(es), ses);
  }
}
```
---

### Literals

#### 1. `annotate_literal`

**Source:** Literals.tex:147

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[int]{}{
{
\begin{array}{r}
  \annotateliteral(\overname{\Ignore}{\tenv}, \overname{\LInt(n)}{\vl}) \typearrow\\
  \TInt(\WellConstrained([\ConstraintExact(\ELInt{n})]))
\end{array}
}
}
\end{mathpar}

\begin{mathpar}
\inferrule[bool]{}{\annotateliteral(\overname{\Ignore}{\tenv}, \overname{\LBool(\Ignore)}{\vl})\typearrow \TBool}
\end{mathpar}

\begin{mathpar}
\inferrule[real]{}{\annotateliteral(\overname{\Ignore}{\tenv}, \overname{\LReal(\Ignore)}{\vl})}\typearrow \TReal
\end{mathpar}

\begin{mathpar}
\inferrule[string]{}{\annotateliteral(\overname{\Ignore}{\tenv}, \overname{\LString(\Ignore)}{\vl})\typearrow \TString}
\end{mathpar}

\begin{mathpar}
\inferrule[bits]{
  n \eqdef \listlen{\bits}
}{
  \annotateliteral(\overname{\Ignore}{\tenv}, \overname{\LBitvector(\bits)}{\vl})\typearrow \TBits(\ELInt{n}, \emptylist)
}
\end{mathpar}

\begin{mathpar}
\inferrule[label]{
  \tenv.\staticenvsG.\declaredtypes(\vlabel) = (\vt, \Ignore)
}{
  \annotateliteral(\tenv, \overname{\LLabel(\vlabel)}{\vl}) \typearrow \vt
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case Int {
  l =: L_Int(n);
  cs := make_singleton_list(Constraint_Exact(E_Literal(L_Int(n))));
  --
  T_Int(WellConstrained(cs));
}

case Bool {
  l =: L_Bool(_);
  --
  T_Bool;
}

case Real {
  l =: L_Real(_);
  --
  T_Real;
}

case String {
  l =: L_String(_);
  --
  T_String;
}

case Bits {
  l =: L_Bitvector(bits);
  n := list_len(bits);
  --
  T_Bits(E_Literal(L_Int(n)), empty_list);
}

case Label {
  l =: L_Label(label);
  tenv.static_envs_G.declared_types(label) = (t, _);
  --
  t;
}
```

---

### Primitive Operations

#### 1. `unop_literals`

**Source:** PrimitiveOperations.tex:543

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[error]{
  (\op, \astlabel(\vl)) \not\in \unopsignatures
}{
  \unopliterals(\op, \vl) \typearrow \TypeErrorVal{\BadOperands}
}
\and
\inferrule[negate\_int]{}{
  \unopliterals(\overname{\NEG}{\op}, \overname{\LInt(n)}{\vl}) \typearrow \overname{\LInt(- n)}{\vr}
}
\and
\inferrule[negate\_real]{}{
  \unopliterals(\overname{\NEG}{\op}, \overname{\LReal(q)}{\vl}) \typearrow \overname{\LReal(- q)}{\vr}
}
\and
\inferrule[not\_bool]{}{
  \unopliterals(\overname{\BNOT}{\op}, \overname{\LBool(b)}{\vl}) \typearrow \overname{\LBool(\neg b)}{\vr}
}
\and
\inferrule[not\_bits.empty]{
  \bits \eqname \emptylist\\
  c \eqdef \emptylist
}{
  \unopliterals(\overname{\NOT}{\op}, \overname{\LBitvector(\bits)}{\vl}) \typearrow\overname{\LBitvector(c)}{\vr}
}
\and
\inferrule[not\_bits.non\_empty]{
  \bits \eqname \vb_{1..k}\\
  c \eqdef [i=1..k: (1-\vb_\vi)]
}{
  \unopliterals(\overname{\NOT}{\op}, \overname{\LBitvector(\bits)}{\vl}) \typearrow\overname{\LBitvector(c)}{\vr}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case Error {
  (op, ast_label(l)) not_in unop_signatures;
  --
  TypeError(TE_BO) { auto_name = false, };
}

case negate_int {
  op = NEG;
  l =: L_Int(n);
  --
  L_Int(negate(n));
}

case negate_real {
  op = NEG;
  l =: L_Real(n);
  --
  L_Real(negate(n));
}

case not_bool {
  op = BNOT;
  l =: L_Bool(b);
  --
  L_Bool(not(b));
}

case not_bits {
  l =: L_Bitvector(bits);
  op = NOT;
  c := list_map(b, bits, negate_bit(b));
  --
  L_Bitvector(c);
}
```

---

### Statements

#### 1. `annotate_stmt_SPass`

**Source:** Statements.tex:93

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{}{\annotatestmt(\tenv, \SPass) \typearrow (\SPass, \tenv, \overname{\emptyset}{\vses})}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SPass {
  s = S_Pass;
  --
  (S_Pass, tenv, empty_set);
}
```
---

#### 2. `annotate_stmt_SAssign`

**Source:** Statements.tex:160

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vre) \typearrow (\vtre, \vreone, \vsesre) \OrTypeError\\\\
  \annotatelexpr(\tenv, \vle, \vtre) \typearrow (\vleone, \vsesle) \OrTypeError\\\\
  \vses \eqdef \vsesre \cup \vsesle
}{
  \annotatestmt(\tenv, \overname{\SAssign(\vle, \vre)}{\vs}) \typearrow
  (\overname{\SAssign(\vleone, \vreone)}{\news}, \overname{\tenv}{\newtenv}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SAssign {
  s =: S_Assign(le, re);
  annotate_expr(tenv, re) -> (t_re, re1, ses_re);
  annotate_lexpr(tenv, le, t_re) -> (le1, ses_le);
  ses := union(ses_re, ses_le);
  --
  (S_Assign(le1, re1), tenv, ses);
}
```

---

#### 3. `annotate_stmt_SDecl`

**Source:** Statements.tex:602

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[some]{
  \annotateexpr(\tenv, \ve) \typearrow (\vte, \vep, \vsese) \OrTypeError\\\\
  {
  \begin{array}{r}
  \annotatelocaldecltypeannot(\tenv, \tyopt, \vte, \ldk, \vep, \ldi) \typearrow \\
  (\tenvone, \tyoptp, \vsesldi) \OrTypeError
  \end{array}
  }\\
  \vses \eqdef \vsese \cup \vsesldi\\
  \news \eqdef \SDecl(\ldk, \ldi, \tyoptp, \some{\vep})
}{
  \annotatestmt(\tenv, \overname{\SDecl(\ldk, \ldi, \tyopt, \some{\ve})}{\vs}) \typearrow (\news, \overname{\tenvone}{\newtenv}, \vses)
}
\end{mathpar}
\identr{YSPM}

\begin{mathpar}
\inferrule[none]{
  \vs = \SDecl(\LDKVar, \ldi, \tyopt, \None)\\
  \techeck(\tyopt \neq \None, \TypeErrorVal{\BadDeclaration}) \typearrow \True \OrTypeError \\
  \tyopt \eqname \some{\vt} \\
  \annotatetype(\tenv, \vt) \typearrow (\vtp, \vses) \OrTypeError\\\\
  \basevalue(\tenv, \vtp) \typearrow \veinit \OrTypeError\\\\
  \annotatelocaldeclitem(\tenv, \vtp, \LDKVar, \None, \ldip) \typearrow \newtenv \OrTypeError \\
  \news \eqdef \SDecl(\LDKVar, \ldi, \some{\vtp}, \some{\veinit})
}{
  \annotatestmt(\tenv, \vs) \typearrow (\news, \newtenv, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SDecl {
  case Some {
    s =: S_Decl(ldk, ldi, ty_opt, some(e));
    annotate_expr(tenv, e) -> (t_e, e', ses_e);
    annotate_local_decl_type_annot(tenv, ty_opt, t_e, ldk, e', ldi) -> (tenv1, ty_opt', ses_ldi)
    { math_layout = [_,_] };
    ses := union(ses_e, ses_ldi);
    new_s := S_Decl(ldk, ldi, ty_opt', some(e'));
    --
    (new_s, tenv1, ses);
  }

  case None {
    s =: S_Decl(LDK_Var, ldi, ty_opt, None);
    te_check(ty_opt = some(_), TE_BD) -> True;
    base_value(tenv, t') -> e_init;
    annotate_local_decl_item(tenv, t', LDK_Var, None, ldi') -> new_tenv;
    new_s := S_Decl(LDK_Var, ldi, some(t'), some(e_init));
    --
    (new_s, new_tenv, ses);
  }
}
```

---

#### 4. `annotate_stmt_SSeq`

**Source:** Statements.tex:1159

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotatestmt(\tenv, \vs1) \typearrow (\newsone, \tenvone, \vsesone) \OrTypeError\\\\
  \annotatestmt(\tenvone, \vs2) \typearrow (\newstwo, \newtenv, \vsestwo) \OrTypeError\\\\
  \vses \eqdef \vsesone \cup \vsestwo
}{
  \annotatestmt(\tenv, \overname{\SSeq(\vsone, \vstwo)}{\vs}) \typearrow (\overname{\SSeq(\newsone, \newstwo)}{\news}, \newtenv, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SSeq {
  s =: S_Seq(s1, s2);
  annotate_stmt(tenv, s1) -> (new_s1, tenv1, ses1);
  annotate_stmt(tenv1, s2) -> (new_s2, new_tenv, ses2);
  ses := union(ses1, ses2);
  --
  (S_Seq(new_s1, new_s2), new_tenv, ses);
}
```

---

#### 5. `annotate_stmt_SCall`

**Source:** Statements.tex:1252

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  {
    \begin{array}{r}
      \annotatecall(\vcall) \typearrow (\vcallp, \None, \vses) \OrTypeError
    \end{array}
  }
}{
  {
    \begin{array}{r}
  \annotatestmt(\tenv, \overname{\SCall(\vcall)}{\vs}) \typearrow
  (\overname{\SCall(\vcallp)}{\news}, \tenv, \vses)
    \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SCall {
  s =: S_Call(call);
  annotate_call(call) -> (call', None, ses);
  --
  (S_Call(call'), tenv, ses);
}
```
---

#### 6. `annotate_stmt_SCond`

**Source:** Statements.tex:1397

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \ve) \typearrow (\tcond, \econd, \vsescond) \OrTypeError\\\\
  \checktypesat(\tenv, \tcond, \TBool) \typearrow \True \OrTypeError\\\\
  \annotateblock(\tenv, \vsone) \typearrow (\vsonep, \vsesone) \OrTypeError\\\\
  \annotateblock(\tenv, \vstwo) \typearrow (\vstwop, \vsestwo) \OrTypeError\\\\
  \vses \eqdef \vsescond \cup \vsesone \cup \vsestwo
}{
  \annotatestmt(\tenv, \overname{\SCond(\ve, \vsone, \vstwo)}{\vs}) \typearrow
  (\overname{\SCond(\econd, \vsonep, \vstwop)}{\news}, \overname{\tenv}{\newtenv})
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SCond {
  s =: S_Cond(e, s1, s2);
  annotate_expr(tenv, e) -> (t_cond, e_cond, ses_cond);
  checked_typesat(tenv, t_cond, T_Bool) -> True;
  annotate_block(tenv, s1) -> (s1', ses1);
  annotate_block(tenv, s2) -> (s2', ses2);
  ses := union(ses_cond, ses1, ses2);
  --
  (S_Cond(e_cond, s1', s2'), tenv, ses);
}
```

---

#### 7. `annotate_stmt_SAssert`

**Source:** Statements.tex:1863

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \ve) \typearrow (\vtep, \vep, \vsese) \OrTypeError\\\\
  \techeck(\sesisreadonly(\vsese) \typearrow \True, \SideEffectViolation) \typearrow \True \OrTypeError\\\\
  \checktypesat(\tenv, \vtep, \TBool) \typearrow \True \OrTypeError\\\\
  \vses \eqdef \vsese
  }{
  \annotatestmt(\tenv, \overname{\SAssert(\ve)}{\vs}) \typearrow (\overname{\SAssert(\vep)}{\news}, \overname{\tenv}{\newtenv}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SAssert {
  s =: S_Assert(e);
  annotate_expr(tenv, e) -> (t_e', e', ses_e);
  te_check( ses_is_readonly(ses_e) -> True, TE_SEV ) -> True;
  checked_typesat(tenv, t_e', T_Bool) -> True;
  ses := ses_e;
  --
  (S_Assert(e'), tenv, ses);
}
```
---

#### 8. `annotate_stmt_SWhile`

**Source:** Statements.tex:2030

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \vs = \SWhile(\veone, \vlimitone, \vsone)\\
  \annotateexpr(\tenv, \veone) \typearrow (\vt, \vetwo, \vsese) \OrTypeError\\\\
  \annotatelimitexpr(\tenv, \vlimitone) \typearrow (\vlimittwo, \vseslimit) \OrTypeError\\\\
  \checktypesat(\tenv, \vt, \TBool) \typearrow \True \OrTypeError\\\\
  \annotateblock(\tenv, \vsone) \typearrow (\vstwo, \vsesblock) \OrTypeError\\\\
  \vses \eqdef \vsesblock \cup \vsese \cup \vseslimit
}{
  \annotatestmt(\tenv, \vs) \typearrow
  (\overname{\SWhile(\vetwo, \vlimittwo, \vstwo)}{\news}, \overname{\tenv}{\newtenv}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SWhile {
  s =: S_While(e1, limit1, s1);
  annotate_expr(tenv, e1) -> (t, e2, ses_e);
  annotate_limit_expr(tenv, limit1) -> (limit2, ses_limit);
  checked_typesat(tenv, t, T_Bool) -> True;
  annotate_block(tenv, s1) -> (s2, ses_block);
  ses := union(ses_block, ses_e, ses_limit);
  --
  (S_While(e2, limit2, s2), tenv, ses);
}
```

---

#### 9. `annotate_stmt_SRepeat`

**Source:** Statements.tex:2373

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateblock(\tenv, \vsone) \typearrow (\vstwo, \vsesblock) \OrTypeError\\\\
  \annotatelimitexpr(\tenv, \vlimitone) \typearrow (\vlimittwo, \vseslimit) \OrTypeError\\\\
  \annotateexpr(\tenv, \veone) \typearrow (\vt, \vetwo, \vsese) \OrTypeError\\\\
  \checktypesat(\tenv, \vt, \TBool) \typearrow \True \OrTypeError\\\\
  \vses \eqdef \vsesblock \cup \vsese \cup \vseslimit
}{
  {
  \begin{array}{r}
    \annotatestmt(\tenv, \overname{\SRepeat(\vsone, \veone, \vlimitone)}{\vs}) \typearrow\\
    (\overname{\SRepeat(\vstwo, \vetwo, \vlimittwo)}{\news}, \overname{\tenv}{\newtenv}, \vses)
  \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SRepeat {
  s =: S_Repeat(s1, e1, limit1);
  annotate_block(tenv, s1) -> (s2, ses_block);
  annotate_limit_expr(tenv, limit1) -> (limit2, ses_limit);
  annotate_expr(tenv, e1) -> (t, e2, ses_e);
  checked_typesat(tenv, t, T_Bool) -> True;
  ses := union(ses_block, ses_e, ses_limit);
  --
  (S_Repeat(s2, e2, limit2), tenv, ses);
}
```
---

#### 10. `annotate_stmt_SFor`

**Source:** Statements.tex:2588

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \vstarte) \typearrow (\vstartt, \vstartep, \vsesstart) \OrTypeError\\\\
  \annotateexpr(\tenv, \vende) \typearrow (\vendt, \vendep, \vsesend) \OrTypeError\\\\
  \annotatelimitexpr(\tenv, \vlimit) \typearrow (\vlimitp, \vseslimit) \OrTypeError\\\\
  \techeck(\sesisreadonly(\vsesstart), \SideEffectViolation) \typearrow \True\OrTypeError\\\\
  \techeck(\sesisreadonly(\vsesend), \SideEffectViolation) \typearrow \True\OrTypeError\\\\
  \vsescond \eqdef \vsesstart \cup \vsesend \cup \vseslimit\\
  \makeanonymous(\tenv, \vstartt) \typearrow \vstartstruct \OrTypeError\\\\
  \makeanonymous(\tenv, \vendt) \typearrow \vendstruct \OrTypeError\\\\
  {
    \begin{array}{r}
  \getforconstraints(\tenv, \vstartstruct, \vendstruct, \vstartep, \vendep, \dir) \typearrow \\
    \cs \OrTypeError
    \end{array}
  }\\\\
  \tty \eqdef \TInt(\cs)\\
  \checkvarnotinenv(\tenv, \vindexname) \typearrow \True \OrTypeError\\\\
  \addlocal(\tenv, \tty, \vindexname, \LDKLet) \typearrow \tenvp\\
  \annotateblock(\tenvp, \vbody) \typearrow (\vbodyp, \vsesblock) \OrTypeError\\\\
  \vses \eqdef \vsesblock \cup \vsescond
}{
  {
    \begin{array}{r}
  \annotatestmt\left(\tenv, \overname{\SFor\left\{\begin{array}{rcl}
    \Forindexname &:& \vindexname\\
    \Forstarte &:& \vstarte\\
    \Fordir &:& \vdir\\
    \Forende &:& \vende\\
    \Forbody &:& \vbody\\
    \Forlimit &:& \vlimit
  \end{array}\right\}}{\vs}\right) \typearrow \\
  \left(\overname{\SFor\left\{\begin{array}{rcl}
    \Forindexname &:& \vindexname\\
    \Forstarte &:& \vstartep\\
    \Fordir &:& \vdir\\
    \Forende &:& \vendep\\
    \Forbody &:& \vbodyp\\
    \Forlimit &:& \vlimitp
  \end{array}\right\}}{\news}, \overname{\tenv}{\newtenv}, \vses\right)
\end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SFor {
  s =: S_For[
    index_name : index_name,
    start_e    : start_e,
    dir        : dir,
    end_e      : end_e,
    body       : body,
    limit      : limit
  ];
  annotate_expr(tenv, start_e) -> (start_t, start_e', ses_start);
  annotate_expr(tenv, end_e) -> (end_t, end_e', ses_end);
  annotate_limit_expr(tenv, limit) -> (limit', ses_limit);
  te_check(ses_is_readonly(ses_start), TE_SEV) -> True;
  te_check(ses_is_readonly(ses_end), TE_SEV) -> True;
  ses_cond := union(ses_start, ses_end, ses_limit);
  make_anonymous(tenv, start_t) -> start_struct;
  make_anonymous(tenv, end_t) -> end_struct;
  get_for_constraints(tenv, start_struct, end_struct, start_e', end_e', dir, cs) -> cs
  { ([_,_,_,_,_,_,_], _) };
  ty := T_Int(cs);
  check_var_not_in_env(tenv, index_name) -> True;
  add_local(tenv, ty, index_name, LDK_Let) -> tenv';
  annotate_block(tenv', body) -> (body', ses_block);
  ses := union(ses_block, ses_cond);
  --
  (
    S_For[
    index_name : index_name,
    start_e    : start_e',
    dir        : dir,
    end_e      : end_e',
    body       : body',
    limit      : limit'
    ],
    tenv,
    ses
  );
}
```
---

#### 11. `annotate_stmt_SThrow`

**Source:** Statements.tex:2999

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \ve) \typearrow (\vte, \vep, \vsesone) \OrTypeError\\\\
  \checkstructurelabel(\tenv, \vte, \TException) \typearrow \True \OrTypeError\\\\
  \vte \eqname \TNamed(\exnname)\\
  \vses \eqdef \vsesone \cup \{\LocalEffect(\SEImpure), \GlobalEffect(\SEImpure)\}
}{
  \annotatestmt(\tenv, \overname{\SThrow(\ve)}{\vs}) \typearrow
  (\overname{\SThrow(\vep, \vte)}{\news}, \overname{\tenv}{\newtenv}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SThrow {
  s =: S_Throw(e);
  annotate_expr(tenv, e) -> (t_e, e', ses1);
  check_structure_label(tenv, t_e, T_Exception) -> True;
  t_e =: T_Named(exn_name);
  ses := union(ses1, make_set(LocalEffect(SE_Impure), GlobalEffect(SE_Impure)));
  --
  (S_Throw(e', t_e), tenv, ses);
}
```
---

#### 12. `annotate_stmt_SReturn`

**Source:** Statements.tex:3305

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[error]{
  \vb \eqdef (\tenv.\staticenvsL.\returntype = \None \leftrightarrow \veopt = \None) \\
  \vb = \False
}{
  \annotatestmt(\tenv, \overname{\SReturn(\veopt)}{\vs}) \typearrow \TypeErrorVal{\BadSubprogramDeclaration}
}
\end{mathpar}

\begin{mathpar}
\inferrule[none]{
  \tenv.\staticenvsL.\returntype = \None
}{
  \annotatestmt(\tenv, \overname{\SReturn(\None)}{\vs}) \typearrow
  (\overname{\SReturn(\None)}{\news}, \overname{\tenv}{\newtenv}, \overname{\emptyset}{\vses})
}
\end{mathpar}

\begin{mathpar}
\inferrule[some]{
  \vs = \SReturn(\some{ \ve })\\
  \tenv.\staticenvsL.\returntype = \some{ \vt }\\
  \annotateexpr(\tenv, \ve) \typearrow (\vtep, \vep, \vses) \OrTypeError\\\\
  \checktypesat(\tenv, \vtep, \vt) \typearrow \True \OrTypeError
}{
  \annotatestmt(\tenv, \vs) \typearrow
  (\overname{\SReturn(\some{ \vep })}{\news}, \overname{\tenv}{\newtenv}, \vses)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SReturn {
  case Error {
    s =: S_Return(e_opt);
    b := (tenv.static_envs_L.return_type = None) <=> (e_opt = None);
    b = False;
    --
    TypeError(TE_BSPD);
  }

  case None {
    s = S_Return(None);
    tenv.static_envs_L.return_type = None;
    --
    (S_Return(None), tenv, empty_set);
  }

  case Some {
    s =: S_Return(some(e));
    tenv.static_envs_L.return_type = some(t);
    annotate_expr(tenv, e) -> (t_e', e', ses);
    checked_typesat(tenv, t_e', t) -> True;
    --
    (S_Return(some(e')), tenv, ses);
  }
}
```
---

#### 14. `annotate_stmt_SUnreachable`

**Source:** Statements.tex:3779

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{}{
  \annotatestmt(\tenv, \SUnreachable) \typearrow (\SUnreachable, \tenv, \overname{\emptyset}{\vses})
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SUnreachable {
  s = S_Unreachable;
  --
  (S_Unreachable, tenv, empty_set);
}
```
---

#### 15. `annotate_stmt_SPragma`

**Source:** Statements.tex:3847

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  i\in\listrange(\vargs): \annotateexpr(\tenv, \vargs[i]) \typearrow (\Ignore, \Ignore, \vxs_i) \OrTypeError\\\\
  \vses \eqdef \bigcup_{i\in\listrange(\vargs)} \vxs_i
}{
  \annotatestmt(\tenv, \overname{\SPragma(\id, \vargs)}{\vs}) \typearrow (\overname{\SPass}{\news}, \tenv)
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case SPragma {
  s =: S_Pragma(id, args);
  INDEX(i, args : annotate_expr(tenv, args[i]) -> (_, _, sess[i]));
  ses := union_list(sess);
  --
  (S_Pass, tenv, ses);
}
```
---

### Type System Utility Rules

#### 1. `lookup_constant`

**Source:** TypeSystemUtilities.tex:460

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{}{
  \lookupconstant(\tenv, \vs) \typearrow \overname{\tenv.\staticenvsG.\constantvalues(\vs)}{\vv}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
--
tenv.static_envs_G.constant_values(s);
```

---

### TypeAttributes

#### 1. `is_builtin_singular`

**Source:** TypeAttributes.tex:46

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \vb \eqdef \astlabel(\tty) \in \{\TReal, \TString, \TBool, \TBits, \TEnum, \TInt\}
}{
  \isbuiltinsingular(\tty) \typearrow \vb
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
b := ast_label(ty) in make_set(label_T_Real, label_T_String, label_T_Bool, label_T_Bits, label_T_Enum, label_T_Int);
--
b;
```

---

#### 2. `is_named`

**Source:** TypeAttributes.tex:76

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \vb \eqdef \astlabel(\tty) = \TNamed
}{
  \isnamed(\tty) \typearrow \vb
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
--
ast_label(ty) = label_T_Named;
```

---

#### 3. `is_anonymous`

**Source:** TypeAttributes.tex:104

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \vb \eqdef \astlabel(\tty) \neq \TNamed
}{
  \isanonymous(\tty) \typearrow \vb
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
--
not_equal(ast_label(ty), label_T_Named);
```

---

#### 4. `is_singular`

**Source:** TypeAttributes.tex:136

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \makeanonymous(\tenv, \tty) \typearrow \vtone \OrTypeError\\\\
  \isbuiltinsingular(\vtone) \typearrow \vb
}{
  \issingular(\tenv, \tty) \typearrow \vb
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
make_anonymous(tenv, ty) -> t1;
is_builtin_singular(t1) -> b;
--
b;
```

---

#### 5. `is_structured`

**Source:** TypeAttributes.tex:172

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{}{
  \isstructured(\tty) \typearrow \overname{\astlabel(\tty) \in \{\TRecord, \TException, \TCollection\}}{\vb}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
--
ast_label(ty) in make_set(label_T_Record, label_T_Exception, label_T_Collection);
```

---

#### 6. `get_structure`

**Source:** TypeAttributes.tex:230

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[named]{
  \declaredtype(\tenv, \vx) \typearrow \vtone \OrTypeError\\\\
  \tstruct(\tenv, \vtone)\typearrow\vt \OrTypeError
}{
  \tstruct(\tenv, \overname{\TNamed(\vx)}{\tty}) \typearrow \vt
}
\end{mathpar}

\begin{mathpar}
\inferrule[builtin\_singular]{
  \isbuiltinsingular(\tty) \typearrow \True
}{
  \tstruct(\tenv, \tty) \typearrow \tty
}
\end{mathpar}

\begin{mathpar}
\inferrule[tuple]{
  \tys \eqname \vt_{1..k}\\
  i=1..k: \tstruct(\tenv, \vt_i) \typearrow \vtp_i \OrTypeError
}{
  \tstruct(\tenv, \overname{\TTuple(\tys)}{\tty}) \typearrow  \TTuple(i=1..k: \vtp_i)
}
\end{mathpar}

\begin{mathpar}
\inferrule[array]{
  \tstruct(\tenv, \vt) \typearrow \vtone \OrTypeError
}{
  \tstruct(\tenv, \overname{\TArray(\ve, \vt)}{\tty}) \typearrow \TArray(\ve, \vtone)
}
\end{mathpar}

\begin{mathpar}
\inferrule[structured]{
  L \in \{\TRecord, \TException, \TCollection\}\\\\
  (\id,\vt) \in \fields : \tstruct(\tenv, \vt) \typearrow \vt_\id \OrTypeError
}{
  \tstruct(\tenv, \overname{L(\fields)}{\tty}) \typearrow
 L([ (\id,\vt) \in \fields : (\id,\vt_\id) ])
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case named {
  ty =: T_Named(x);
  declared_type(tenv, x) -> t1;
  get_structure(tenv, t1) -> t;
  --
  t;
}

case builtin_singular {
  is_builtin_singular(ty) -> True;
  --
  ty;
}

case tuple {
  ty =: T_Tuple(tys);
  INDEX(i, tys: get_structure(tenv, tys[i]) -> tys'[i]);
  --
  T_Tuple(tys');
}

case array {
  ty =: T_Array(e, t);
  get_structure(tenv, t) -> t1;
  --
  T_Array(e, t1);
}

case structured {
  ty =: L(fields);
  L in make_set(T_Record, T_Exception, T_Collection);
  list_combine(names, types) := fields;
  INDEX(i, types: get_structure(tenv, types[i]) -> types'[i]);
  --
  L(list_combine(names, types'));
}
```

---

#### 7. `make_anonymous`

**Source:** TypeAttributes.tex:315

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[named]{
  \tty \eqname \TNamed(\vx) \\
  \declaredtype(\tenv, \vx) \typearrow \vtone \OrTypeError \\\\
  \makeanonymous(\tenv, \vtone) \typearrow \vt
}{
  \makeanonymous(\tenv, \tty) \typearrow \vt
}
\and
\inferrule[non-named]{
  \astlabel(\tty) \neq \TNamed
}{
  \makeanonymous(\tenv, \tty) \typearrow \tty
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case named {
  ty = T_Named(x);
  declared_type(tenv, x) -> t1;
  make_anonymous(tenv, t1) -> t;
  --
  t;
}

case non_named {
  not_equal(ast_label(ty), T_Named);
  --
  ty;
}
```

---

#### 8. `check_constrained_integer`

**Source:** TypeAttributes.tex:376

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule[well-constrained]{}
{
  \checkconstrainedinteger(\tenv, \overname{\TInt(\WellConstrained(\Ignore))}{\tty}) \typearrow \True
}
\and
\inferrule[parameterized]{}
{
  \checkconstrainedinteger(\tenv, \overname{\TInt(\Parameterized(\Ignore))}{\tty}) \typearrow \True
}
\and
\inferrule[unconstrained]{
  \astlabel(\vc) = \Unconstrained \;\lor\; \astlabel(\vc) = \PendingConstrained
}{
  \checkconstrainedinteger(\tenv, \overname{\TInt(\vc)}{\tty}) \typearrow \TypeErrorVal{\UnexpectedType}
}
\and
\inferrule[conflicting\_type]{
  \astlabel(\vt) \neq \TInt
}{
  \checkconstrainedinteger(\tenv, \vt) \typearrow \TypeErrorVal{\UnexpectedType}
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
case well_constrained {
  t = T_Int(WellConstrained(_));
  --
  True;
}

case parameterized {
  t = T_Int(Parameterized(_));
  --
  True;
}

case unconstrained {
  t = T_Int(c);
  ast_label(c) = Unconstrained || ast_label(c) = PendingConstrained;
  --
  TypeError(TE_UT);
}

case conflicting_type {
  not_equal(ast_label(t), T_Int);
  --
  TypeError(TE_UT);
}
```

---


#### 68. `annotate_get_array`

**Source:** Expressions.tex:1407

**LaTeX (Source):**

```latex
\begin{mathpar}
\inferrule{
  \annotateexpr(\tenv, \eindex) \typearrow (\tindexp, \eindexp, \vsesindex) \OrTypeError\\\\
  \typeofarraylength(\size) \typearrow \wantedtindex\\
  \checktypesat(\tenv, \tindexp, \wantedtindex) \typearrow \True \OrTypeError\\\\
  \vses \eqdef \vsesindex \cup \vsesbase \\
  \newe \eqdef \ifthenelse{\astlabel(\size) = \ArrayLengthExpr}{\EGetArray(\ebase, \eindexp)}{\EGetEnumArray(\ebase, \eindexp)}
}{
  {
  \begin{array}{r}
  \annotategetarray(\tenv, (\size, \telem), (\ebase, \vsesbase, \eindex)) \typearrow\\
  (\overname{\telem}{\vt}, \newe, \vses)
  \end{array}
  }
}
\end{mathpar}
```

**ASLSpec (Target):**

```aslspec
annotate_expr(tenv, e_index) -> (t_index', e_index', ses_index);
type_of_array_length(size) -> wanted_t_index;
check_type_satisfies(tenv, t_index', wanted_t_index) -> True;
ses := union(ses_index, ses_base);
new_e :=
  if (ast_label(size) = label_ArrayLength_Expr) then
    E_GetArray(e_base, e_index')
  else
    E_GetEnumArray(e_base, e_index');
--
(t_elem, new_e, ses);
```

**Translation Notes:**
- **If-then-else syntax**: LaTeX `\ifthenelse{condition}{true_branch}{false_branch}` is translated to ASLSpec's if-then-else:
  ```aslspec
  variable := if (condition) then branch1 else branch2;
  ```
  The condition is wrapped in parentheses, and both branches are indented on separate lines for readability.
- **Set union**: LaTeX `\cup` (union operator) becomes `union()` function call in ASLSpec
- **Input parameters**: Note that `t_elem` is a direct input parameter to the relation (part of the tuple `(size: expr, t_elem: ty)`), so it appears directly in the conclusion without being computed
- **Primed variables**: The index expression after annotation is `e_index'` (with apostrophe), following the mathematical convention for "new" or "annotated" versions of values
- **Correct structure**: This is a single-rule relation where the entire implementation is the premises followed by `--` and the conclusion, all inside the `} =` block
- **RenderRule placement**: The `\RenderRule{annotate_get_array}` directive should appear AFTER the LaTeX inference rule in the mathpar block, not before it

---

### Translation Best Practices Summary

1. **Conditional Expressions**: Use if-then-else for LaTeX `\ifthenelse`, NOT C-style ternary (`?:`). Format:
   ```aslspec
   var := if (condition) then value1 else value2;
   ```
   Indent both branches on separate lines for readability.

2. **Set Operations**: Always use function calls:
   - `\cup` → `union(set1, set2)` or `union_list(list_of_sets)`
   - `\cap` → `intersect(set1, set2)`
   - `\emptyset` → `empty_set`

3. **Type Checking**: Use `te_check(condition, ERROR_CODE) -> True;` for premises that check type errors

4. **Variable Naming**: Apply mathematical convention - primed variables use apostrophes:
   - `t_p` → `t'`
   - `e_p` → `e'`
   - NOT `_p` suffix

5. **Conclusion Syntax**: Every rule conclusion must:
   - Come after `--` separator
   - End with semicolon: `(result);` not `(result)` on separate line
   - Be the final statement in the `} =` block

6. **Function Calls with Results**: Use arrow notation for assignments:
   - `function_call(...) -> result;` binds result
   - `assignment := expression;` for local definitions
   - `te_check(cond, code) -> True;` for error predicates

7. **RenderRule Directives**: Always place `\RenderRule{relation_name}` AFTER the corresponding LaTeX inference rule block (after `\end{mathpar}`), never before

````
