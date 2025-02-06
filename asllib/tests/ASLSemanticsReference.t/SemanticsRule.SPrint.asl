type MyEnum of enumeration { LABEL_A, LABEL_B, LABEL_C };
func main () => integer
begin
  print("string_");
  print("number_");
  println(1);
  println(1_000__000);
  println(0xa_b_c_d_e_f__A__B__C__D__E__F__0___12345567890);
  println(TRUE);
  println(FALSE);
  println(1234567890.0123456789);
  println("hello\\world\n\t \"here I am \"");
  print("");
  println('11 01');
  println('');
  println(LABEL_B);
  return 0;
end;
