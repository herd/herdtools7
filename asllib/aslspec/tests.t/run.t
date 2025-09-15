# Basic tests
  $ aslspec hello.spec
  $ aslspec typedefs.spec
  $ aslspec relations.spec

  $ aslspec type_name.bad
  Fatal error: exception Dune__exe__AST.SpecError("element-defining identifiers must not contain digits: t2")
  [2]
  $ aslspec type_instance.bad

# Test that --pp generates legal output
  $ aslspec typedefs.spec --pp > tmp.spec; aslspec tmp.spec
  $ aslspec relations.spec --pp > tmp.spec; aslspec tmp.spec

# Test that --pp is idempotent
  $ aslspec typedefs.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff tmp1.spec tmp2.spec
  $ aslspec relations.spec --pp > tmp1.spec; aslspec tmp1.spec --pp > tmp2.spec; diff tmp1.spec tmp2.spec
