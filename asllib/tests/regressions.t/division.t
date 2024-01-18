Unsupported division:

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   assert 6 DIV -3 == 0;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
