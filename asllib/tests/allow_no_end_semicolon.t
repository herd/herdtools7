We should be able to make semicolons after 'end' optional
  $ cat >no_semicolon.asl <<EOF
  > func main () => integer
  > begin
  >   if TRUE then
  >      print("test");
  >   end
  >   return 0;
  > end
  > EOF
  $ aslref no_semicolon.asl
  File no_semicolon.asl, line 5, characters 2 to 5:
    end
    ^^^
  ASL Grammar error: Obsolete syntax: Missing ';' after 'end' keyword.
  [1]
  $ aslref --allow-no-end-semicolon no_semicolon.asl
  test
