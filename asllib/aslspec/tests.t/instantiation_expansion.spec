typedef Z { "" };

ast literal { "literal" } =
    | L_Int(whole_number: Z) {""}
;

typedef native_value {""} =
    | NV_Literal(l: literal)
    { "" }
;

typedef tint {""} =
   (NV_Literal(L_Int(v: Z)))
   {
       "native integer for {v}",
   }
;
