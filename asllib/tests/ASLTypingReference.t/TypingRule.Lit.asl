type MyEnum of enumeration { LABEL_A, LABEL_B, LABEL_C };
func main () => integer
begin
  //                Literal                             Type of literal
  var n1          = 5;                                  // integer{5}
  var n2          = 1_000__000;                         // integer{1000000}
  var n4          = 0xa_b_c_d_e_f__A__B__C__D__E__F__0___12345567890;
                                    // integer{53170898287292728730499578000}
  var btrue       = TRUE;                               // boolean
  var bfalse      = FALSE;                              // boolean
  var rzero       = 1234567890.0123456789;              // real
  var s1          = "hello\\world \n\t \"here I am \""; // string
  var s2          = "";                                 // string
  var bv1         = '11 01';                            // bits(4)
  var bv2         = '';                                 // bits(0)
  var l1 : MyEnum = LABEL_B;                            // MyEnum
  return 0;
end;
