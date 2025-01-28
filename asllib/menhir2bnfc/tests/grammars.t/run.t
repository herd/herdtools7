Ensure that a bnfc-style grammar can be generated from a simple menhir grammar
  $ menhir --cmly calc.mly
  $ menhir2bnfc calc.cmly calc.cf
  $ diff calc.cf calc.cf.expected

Test the bnf output
  $ menhir --cmly calc.mly
  $ menhir2bnfc --no-ast calc.cmly calc.bnf
  $ diff calc.bnf calc.bnf.expected

Test the sorting logic
  $ menhir --cmly calc.mly
  $ menhir2bnfc --order calc_order.txt calc.cmly calc_ordered.cf
  $ diff calc_ordered.cf calc_ordered.cf.expected

Test that we can generate a complete parser if a simple lexer is provided
  $ ocamllex -ml calc_lexer.mll -o calc_lexer.ml
  $ menhir --cmly calc.mly
  $ menhir2bnfc --ml calc_lexer.ml calc.cmly calc_full.cf
  $ diff calc_full.cf calc_full.cf.expected
