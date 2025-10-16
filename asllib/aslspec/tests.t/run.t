# Basic tests
  $ aslspec hello.spec --render; diff generated_macros.tex hello.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec typedefs.spec --render; diff generated_macros.tex typedefs.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec relations.spec --render; diff generated_macros.tex relations.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex

  $ aslspec type_name.bad
  Syntax Error: illegal element-defining identifier: t2 around type_name.bad line 2 column 1
  [1]
  $ aslspec type_instance.bad

# Test that --pp generates legal output
  $ aslspec typedefs.spec --pp > tmp.spec; aslspec tmp.spec
  $ aslspec relations.spec --pp > tmp.spec; aslspec tmp.spec

# Test that --pp is idempotent
  $ aslspec typedefs.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec
  $ aslspec relations.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff --ignore-all-space tmp1.spec tmp2.spec

  $ aslspec unmatched_prose_var.spec
  Specification Error: The prose template 'transforms {a} to {b}' contains the following unmatched variables: {b}
  [1]

  $ aslspec labelled_tuple_instantiation.bad
  Specification Error: The type term `A(Int)` cannot be instantiated since 'A' is not a labelled tuple type
  [1]
  $ aslspec type_instantiation_function.bad
  Specification Error: The type term `fun P -> O` is not subsumed by `fun O -> O`
  [1]
