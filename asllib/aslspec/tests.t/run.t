# Basic tests
  $ aslspec hello.spec --render; diff -w generated_macros.tex hello.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec relations.spec --render; diff -w generated_macros.tex relations.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec rule.spec --render; diff -w generated_macros.tex rule.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec operators.spec --render; diff -w generated_macros.tex operators.expected; rm -f generated_macros.tex
  Generated LaTeX macros into generated_macros.tex
  $ aslspec layout_correct.spec
  $ aslspec expressions_correct.spec
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
  $ aslspec --render instantiation_expansion.spec
  Generated LaTeX macros into generated_macros.tex
  $ aslspec --render instantiation_expansion_depth.spec
  Generated LaTeX macros into generated_macros.tex
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
  $ aslspec parameterized_type_instantiation.bad
  Specification Error: parameterized_type_instantiation.bad:12:3: Unable to determine that `Leaf(Int)` is subsumed by `Leaf(value: T)`
  [1]
  $ aslspec relation_unnamed_arguments.bad
  Specification Error: relation_unnamed_arguments.bad:6:38: The term Num in relation unnamed_arg_has_rule does not provide a name for at least one of its sub-terms.
  [1]
  $ aslspec operator_arity.bad
  Specification Error: operator_arity.bad:7:10: The application of relation unary in expression unary(a, b) has an invalid number of arguments: expected 1 but found 2
  [1]
  $ aslspec variadic_operator_type.bad
  Specification Error: variadic_operator_type.bad:3:46: Could not unify types N and Bool for parameter T of relation make_set
  [1]
  $ aslspec record_extra_field.bad
  Specification Error: record_extra_field.bad:8:5: The record expression [rf : a, rg : a] has missing or invalid field names: expected rf but found rf, rg
  [1]
  $ aslspec field_access_unknown.bad
  Specification Error: field_access_unknown.bad:8:5: The non-field identifier rg is used as a field in r.rg
  [1]
  $ aslspec record_update_unknown_field.bad
  Specification Error: record_update_unknown_field.bad:8:5: The non-field identifier rg is used as a field in r[rg : a]
  [1]
  $ aslspec layout_type.bad
  Specification Error: layout_type.bad:1:1: layout (_,_) is inconsistent with BadAtomicType. Here's a consistent layout: _
  [1]
  $ aslspec layout_tuple.bad
  Specification Error: layout_tuple.bad:2:7: layout (_,_,_) is inconsistent with (N, Bool). Here's a consistent layout: (_,_)
  [1]
  $ aslspec layout_expr_atomic.bad
  Specification Error: layout_expr_atomic.bad:6:5: layout (_,_) is inconsistent with expression flag. Here's a consistent layout: _
  [1]
  $ aslspec layout_expr_nullary.bad
  Specification Error: layout_expr_nullary.bad:6:5: layout () is inconsistent with expression always_true(). Here's a consistent layout: _
  [1]
  $ aslspec layout_expr_list.bad
  Specification Error: layout_expr_list.bad:6:5: layout (_,_,_) is inconsistent with expression two_args(a, b). Here's a consistent layout: (_,_)
  [1]
  $ aslspec layout_record_update.bad
  Specification Error: layout_record_update.bad:7:5: layout (_) is inconsistent with expression r[rf : a, rg : b]. Here's a consistent layout: (_,(_,_))
  [1]
  $ aslspec rule_layout.bad
  Specification Error: rule_layout.bad:5:5: layout (_,_,_) is inconsistent with expression r(a) -> (a, a). Here's a consistent layout: ((_),(_,_))
  [1]
  $ aslspec --render constants.spec
  Generated LaTeX macros into generated_macros.tex
  $ aslspec --render parameterized_types.spec
  Generated LaTeX macros into generated_macros.tex
