  $ cat >comments1.asl <<EOF
  > // This is a line comment
  > integer this_is_not_a_comment = 1;
  > 
  > // This is another comment
  > integer main ()
  >   var a = b;
  >   return 0;
  > EOF

  $ aslref -0 comments1.asl
  File comments1.asl, line 6, characters 11 to 12:
    var a = b;
             ^
  ASL Static error: Undefined identifier: 'b'
  [1]

  $ cat >comments2.asl <<EOF
  > /*
  > 
  > 
  > 
  > */
  > 
  > var foo = "sigjrshgrsas
  > kgjrgsoirjggsr
  > fsoirjgrsig";
  > 
  > constant integer a = b;
  > EOF

  $ aslref -0 comments2.asl
  File comments2.asl, line 9, characters 22 to 23:
  constant integer a = b;
                        ^
  ASL Static error: Undefined identifier: 'b'
  [1]


