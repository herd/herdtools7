  $ cat >printer1.asl <<EOF
  > func main () => integer begin
  >   print ("Wow", 2, 3.14, "some other string");
  >   print ("no type-checking");
  >   print (32);
  >   return 0;
  > end

  $ aslref printer1.asl
  Wow 2 3.14 some other string
  no type-checking
  32

  $ cat >printer2.asl <<EOF
  > func main () => integer begin
  >   print ("Wow", 2 + 3.14, "some other string");
  >   print ("no type-checking");
  >   print (32);
  >   return 0;
  > end

  $ aslref printer2.asl
  File printer2.asl, line 2, characters 16 to 24:
  ASL Typing error: Illegal application of operator + on types integer {2}
    and real
  [1]

