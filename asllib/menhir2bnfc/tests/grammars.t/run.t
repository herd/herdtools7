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

Test that we can correctly identifiy nested expression precedence
  $ menhir --cmly split_expr.mly
  $ menhir2bnfc split_expr.cmly split_expr.cf
  $ diff split_expr.cf split_expr.cf.expected

Test that we can correctly identifiy singular nested expression precedence
  $ menhir --cmly split_expr_single.mly
  $ menhir2bnfc split_expr_single.cmly split_expr_single.cf
  $ diff split_expr_single.cf split_expr_single.cf.expected
