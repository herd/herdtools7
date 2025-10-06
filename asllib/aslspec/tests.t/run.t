# Basic tests
  $ aslspec hello.spec --render; diff generated_macros.tex hello_macros.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec typedefs.spec --render; diff generated_macros.tex typedefs_macros.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec relations.spec --render; diff generated_macros.tex relations_macros.expected; rm -f generated_macros.tex
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
