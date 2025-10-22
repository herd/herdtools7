// This test shows an example of how testing for subsumption
// requires "type expansion". That is, inspecting the type
// variants of a type name. In this example, to prove that
// `NV_Literal(L_Int(v: Z))` is subsumed by `NV_Literal(l: literal)`
// we need to expand `literal` and show that `NV_Literal(l: literal)`
// is subsumed by either `L_Bool(Bool)` (it is not) or
// `L_Int(whole_number: Z)` (which is true).

typedef Z;
typedef Bool;

ast literal =
    | L_Bool(Bool)
    | L_Int(whole_number: Z)
;

typedef native_value = NV_Literal(l: literal);

typedef tint = (NV_Literal(L_Int(v: Z)));
