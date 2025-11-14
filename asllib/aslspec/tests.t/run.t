# Basic tests
  $ aslspec hello.spec --render; diff generated_macros.tex hello.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec typedefs.spec --render; diff generated_macros.tex typedefs.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec relations.spec --render; diff generated_macros.tex relations.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex

  $ aslspec type_name.bad
  Syntax Error: illegal element-defining identifier: t2 around type_name.bad line 1 column 41
  [1]

# Test that --pp generates legal output
  $ aslspec typedefs.spec --pp > tmp.spec; aslspec tmp.spec
  $ aslspec relations.spec --pp > tmp.spec; aslspec tmp.spec

# Test that --pp is idempotent
  $ aslspec typedefs.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec
  $ aslspec relations.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec

  $ aslspec unmatched_prose_var.spec
  Specification Error: The prose template 'transforms {a} to {b}' contains the following unmatched variables: {b}
  [1]

# Check that all type terms are well-formed
  $ aslspec instantiation_expansion.spec
  $ aslspec instantiation_labelled_tuple2.bad
  Specification Error: The type term `L(O, A, B)` cannot be instantiated since it has 3 type terms and `L` requires 2 type terms
  While checking: B
  [1]
  $ aslspec instantiation_labelled_tuple.bad
  Specification Error: The type term `A(Int)` cannot be instantiated since 'A' is not a labelled tuple type
  While checking: A
  [1]
  $ aslspec instantiation_function.bad
  Specification Error: Unable to determine that `fun O -> P` is subsumed by `fun P -> P`
  While checking: Incorrect
  [1]
  $ aslspec instantiation_constant.bad
  Specification Error: Int is used as a constant even though it is not defined as one
  While checking: B
  [1]
  $ aslspec instantiation_record.bad
  Specification Error: The type term `A[f: Int]` cannot be instantiated since 'A' is not a labelled record type
  While checking: A
  [1]
  $ aslspec instantiation_label.bad
  Specification Error: The type term `B` cannot be instantiated since 'B' is not a type
  While checking: A
  [1]
  $ aslspec instantiation_recursion.bad
  Specification Error: Unable to determine that `B` is subsumed by `A`
  While checking: B
  [1]
