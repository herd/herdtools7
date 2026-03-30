# Basic tests
  $ aslspec hello.spec --render; diff -w generated_macros.tex hello.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  11c11
  < \newcommand\none[0]{ \hyperlink{constant-none}{\textsf{none}} } % Generated from none
  ---
  > \newcommand\None[0]{ \hyperlink{constant-None}{\textsf{None}} } % Generated from None
  19c19
  < \DefineConstant{none}{\texthypertarget{constant-none}$\none$} % EndDefineConstant
  ---
  > \DefineConstant{None}{\texthypertarget{constant-None}$\None$} % EndDefineConstant
  $ aslspec typedefs.spec --render; diff -w generated_macros.tex typedefs.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  12a13
  > \newcommand\None[0]{ \hyperlink{constant-None}{\textsf{None}} } % Generated from None
  24d24
  < \newcommand\none[0]{ \hyperlink{constant-none}{\textsf{none}} } % Generated from none
  30c30
  < \DefineConstant{none}{\texthypertarget{constant-none}$\none$} % EndDefineConstant
  ---
  > \DefineConstant{None}{\texthypertarget{constant-None}$\None$} % EndDefineConstant
  $ aslspec relations.spec --render; diff -w generated_macros.tex relations.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  9a10
  > \newcommand\None[0]{ \hyperlink{constant-None}{\textsf{None}} } % Generated from None
  18d18
  < \newcommand\none[0]{ \hyperlink{constant-none}{\textsf{none}} } % Generated from none
  26c26
  < \DefineConstant{none}{\texthypertarget{constant-none}$\none$} % EndDefineConstant
  ---
  > \DefineConstant{None}{\texthypertarget{constant-None}$\None$} % EndDefineConstant
  $ aslspec rule.spec --render; diff -w generated_macros.tex rule.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  8a9
  > \newcommand\None[0]{ \hyperlink{constant-None}{\textsf{None}} } % Generated from None
  12d12
  < \newcommand\none[0]{ \hyperlink{constant-none}{\textsf{none}} } % Generated from none
  20c20
  < \DefineConstant{none}{\texthypertarget{constant-none}$\none$} % EndDefineConstant
  ---
  > \DefineConstant{None}{\texthypertarget{constant-None}$\None$} % EndDefineConstant
  $ aslspec operators.spec --render; diff -w generated_macros.tex operators.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  10a11
  > \newcommand\None[0]{ \hyperlink{constant-None}{\textsf{None}} } % Generated from None
  12d12
  < \newcommand\none[0]{ \hyperlink{constant-none}{\textsf{none}} } % Generated from none
  18c18
  < \DefineConstant{none}{\texthypertarget{constant-none}$\none$} % EndDefineConstant
  ---
  > \DefineConstant{None}{\texthypertarget{constant-None}$\None$} % EndDefineConstant
  $ aslspec type_name.bad
  Syntax Error: type_name.bad:1:9: illegal element-defining identifier: t2
  [1]

# Test that --pp generates legal output
  $ aslspec typedefs.spec --pp > tmp.spec; aslspec tmp.spec
  $ aslspec relations.spec --pp > tmp.spec; aslspec tmp.spec

# Test that --pp is idempotent
  $ aslspec typedefs.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec
  $ aslspec relations.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec

  $ aslspec unmatched_prose_var.spec
  Specification Error: Unknown location: The prose template "transforms {a} to {b}" contains the following unmatched variables: {b}
  [1]

# Check that all type terms are well-formed
  $ aslspec instantiation_expansion.spec
  $ aslspec instantiation_labelled_tuple2.bad
  Specification Error: instantiation_labelled_tuple2.bad:9:8: The type term `L(O, A, B)` cannot be instantiated since it has 3 type terms and `L` requires 2 type terms
  [1]
  $ aslspec instantiation_labelled_tuple.bad
  Specification Error: instantiation_labelled_tuple.bad:6:17: The type term `A(Int)` cannot be instantiated since A is not a labelled tuple type
  [1]
  $ aslspec instantiation_function.bad
  Specification Error: instantiation_function.bad:9:25: Unable to determine that `fun O -> P` is subsumed by `fun P -> P`
  [1]
  $ aslspec instantiation_constant.bad
  Specification Error: instantiation_constant.bad:3:18: Int is used as a constant even though it is not defined as one
  [1]
  $ aslspec instantiation_record.bad
  Specification Error: instantiation_record.bad:6:17: The type term `A[f: Int]` cannot be instantiated since A is not a labelled record type
  [1]
  $ aslspec instantiation_label.bad
  Specification Error: instantiation_label.bad:7:8: The type term `B` cannot be instantiated since B is not a type
  [1]
  $ aslspec instantiation_recursion.bad
  Specification Error: instantiation_recursion.bad:9:11: Unable to determine that `B` is subsumed by `A`
  [1]
  $ aslspec relation_unnamed_arguments.bad
  Specification Error: relation_unnamed_arguments.bad:6:38: The term Num in relation unnamed_arg_has_rule does not provide a name for at least one of its sub-terms.
  [1]
  $ aslspec constants.spec
