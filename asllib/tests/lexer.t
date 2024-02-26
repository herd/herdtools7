  $ cat >print.asl <<EOF
  > constant msg = "old pond\\nfrog leaps in\\nwater's sound";
  > func main () => integer begin print(msg); return 0; end
  $ aslref print.asl
  old pond
  frog leaps in
  water's sound
  $ cat >print.asl <<EOF
  > constant msg = "old pond\\n\\tfrog\\tleaps in\\nwater's\\tsound";
  > func main () => integer begin print(msg); return 0; end
  $ aslref print.asl
  old pond
  	frog	leaps in
  water's	sound
  $ cat >print.asl <<EOF
  > constant msg = "Check out this haiku:\\n\\t\\"old pond\\n\\tfrog leaps in\\n\\twater's sound\\"";
  > func main () => integer begin print(msg); return 0; end
  $ aslref print.asl
  Check out this haiku:
  	"old pond
  	frog leaps in
  	water's sound"
  $ cat >print.asl <<EOF
  > constant msg = "Something with \\\\ backslashes.";
  > func main () => integer begin print(msg); return 0; end
  $ aslref print.asl
  Something with \ backslashes.
  $ cat >print.asl <<EOF
  > constant msg = "Something with \\p bad characters.";
  > func main () => integer begin print(msg); return 0; end
  $ aslref print.asl
  File print.asl, line 1, characters 32 to 33:
  ASL Error: Unknown symbol.
  [1]
