func main () => integer
begin
  var n1 = 5; // type: integer{5}
  var n2 = 1_000__000; // type integer{1000000}
  var n4 = 0xa_b_c_d_e_f__A__B__C__D__E__F__0___12345567890;
           // type integer{53170898287292728730499578000}
  var btrue = TRUE; // type: boolean
  var bfalse = FALSE; // type: boolean
  var rzero = 1234567890.0123456789; // type: real(1234567890.0123456789)
  var s1 = "hello\\world \n\t \"here I am \""; // type: string
  // var s1 = "hello\\world \n\t \"here I am \""; // type: string
  var s2 = ""; // type: string
  var bv1 = '11 01'; // type: bits(4)
  var bv2 = ''; // type: bits(0)
  return 0;
end