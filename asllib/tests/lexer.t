  $ cat >println1.asl <<EOF
  > constant msg = "old pond\\nfrog leaps in\\nwater's sound";
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println1.asl
  old pond
  frog leaps in
  water's sound
  $ cat >println2.asl <<EOF
  > constant msg = "old pond\\n\\tfrog\\tleaps in\\nwater's\\tsound";
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println2.asl
  old pond
  	frog	leaps in
  water's	sound
  $ cat >println3.asl <<EOF
  > constant msg = "Check out this haiku:\\n\\t\\"old pond\\n\\tfrog leaps in\\n\\twater's sound\\"";
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println3.asl
  Check out this haiku:
  	"old pond
  	frog leaps in
  	water's sound"
  $ cat >println4.asl <<EOF
  > constant msg = "Something with \\\\ backslashes.";
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println4.asl
  Something with \ backslashes.
  $ cat >println5.asl <<EOF
  > constant msg = "Something with \\p bad characters.";
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println5.asl
  File println5.asl, line 1, characters 32 to 33:
  constant msg = "Something with \p bad characters.";
                                  ^
  ASL Error: Unknown symbol.
  [1]
  $ cat >println6.asl <<EOF
  > constant msg = "Some unterminated string;
  > func main () => integer begin println(msg); return 0; end;
  > EOF
  $ aslref println6.asl
  Fatal error: exception End_of_file
  [2]

C-Style comments
  $ cat >comments1.asl <<EOF
  > func /* this is a /* test */ main () => integer
  > begin /*
  > let's try a multi-line comment /*
  > which finishes here */ constant msg = "/* a comment inside a string? */"; /* another comment
  > that finishes somewhere **/ println (msg); // but not here! */
  > return 0; /* oh a new one */
  > // /* when in a commented line, it doesn't count!
  > end;
  > EOF

  $ aslref comments1.asl
  /* a comment inside a string? */

  $ cat >comments2.asl <<EOF
  > /*
  > 
  > 
  > 
  > */
  > 
  > let foo = "sigjrshgrsas
  > kgjrgsoirjggsr
  > fsoirjgrsig";
  > 
  > let a = b;
  > EOF

  $ aslref comments2.asl
  File comments2.asl, line 11, characters 8 to 9:
  let a = b;
          ^
  ASL Error: Undefined identifier: 'b'
  [1]

Some problems with bitvectors and bitmasks:
  $ cat >masks0.asl <<EOF
  > func main() => integer
  > begin
  >     var b = '';
  >     let expr_a = '' IN {'1'};
  >     let expr_b = '1' IN {''};
  >     let expr_c = '' IN {'0'};
  >     let expr_d = '0' IN {''};
  >     return 0;
  > end;
  > EOF

  $ aslref masks0.asl
  File masks0.asl, line 4, characters 17 to 28:
      let expr_a = '' IN {'1'};
                   ^^^^^^^^^^^
  ASL Type error: cannot find a common ancestor to those two types bits(0) and
    bits(1).
  [1]

Check that variables starting with `__` are reserved:
  $ cat >reserved0.asl <<EOF
  > var pattern: bits(4) = '0001';
  > var _okay: integer = 1;
  > var __reserved: integer = 2;
  > func main() => integer
  > begin
  >   println(pattern);
  >   println(_okay);
  >   println(__reserved);
  >   return 0;
  > end;
  > EOF

  $ aslref reserved0.asl
  ASL Lexical error: "__reserved" is a reserved keyword.
  [1]
  $ aslref --allow-double-underscore reserved0.asl
  0x1
  1
  2
