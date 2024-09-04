  $ cat >print1.asl <<EOF
  > constant msg = "old pond\\nfrog leaps in\\nwater's sound";
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print1.asl
  old pond
  frog leaps in
  water's sound
  $ cat >print2.asl <<EOF
  > constant msg = "old pond\\n\\tfrog\\tleaps in\\nwater's\\tsound";
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print2.asl
  old pond
  	frog	leaps in
  water's	sound
  $ cat >print3.asl <<EOF
  > constant msg = "Check out this haiku:\\n\\t\\"old pond\\n\\tfrog leaps in\\n\\twater's sound\\"";
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print3.asl
  Check out this haiku:
  	"old pond
  	frog leaps in
  	water's sound"
  $ cat >print4.asl <<EOF
  > constant msg = "Something with \\\\ backslashes.";
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print4.asl
  Something with \ backslashes.
  $ cat >print5.asl <<EOF
  > constant msg = "Something with \\p bad characters.";
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print5.asl
  File print5.asl, line 1, characters 32 to 33:
  ASL Error: Unknown symbol.
  [1]
  $ cat >print6.asl <<EOF
  > constant msg = "Some unterminated string;
  > func main () => integer begin print(msg); return 0; end
  > EOF
  $ aslref print6.asl
  File print6.asl, line 3, character 0:
  ASL Error: Unknown symbol.
  [1]

C-Style comments
  $ cat >comments1.asl <<EOF
  > func /* this is a /* test */ main () => integer
  > begin /*
  > let's try a multi-line comment /*
  > which finishes here */ constant msg = "/* a comment inside a string? */"; /* another comment
  > that finishes somewhere **/ print (msg); // but not here! */
  > return 0; /* oh a new one */
  > // /* when in a commented line, it doesn't count!
  > end
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
  ASL Error: Undefined identifier: 'b'
  [1]

