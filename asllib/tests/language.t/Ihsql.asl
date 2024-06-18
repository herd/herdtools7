//I_HQSL: [The term Primitive Operations (c.f. D_KXWT)] includes binop, unop
//and if..then..else expressions.
//Expressions calculate values. All expressions have a unique type. The type
//of an expression can be a tuple. Expressions can have side effects and can
//raise exceptions and, therefore, there are constraints on the evaluation
//order and on the side-effects/exceptions to avoid surprising or
//unpredictable behavior (see 8.5.1 Evaluation order).
//
//Expressions contain several levels of precedence. This is expressed in the
//grammar by a set of mutually recursive definitions.
//expr ::= "if" cexpr "then" expr elsif_expr_list "else" expr | cexpr

//D_KXWT: The term Primitive Operations denotes the set of operations
//available in the expression syntax.

// RUN: interp %s | FileCheck %s

// ! TODO 

func main() => integer
begin
    return 0;
end
