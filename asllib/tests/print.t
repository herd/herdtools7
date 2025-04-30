  $ cat >printer1.asl <<EOF
  > func main () => integer begin
  >   print ("Wow", 2, 3.14, "some other string");
  >   print ("no type-checking");
  >   print (32);
  >   return 0;
  > end;
  > EOF

  $ aslref printer1.asl
  Wow2157/50some other stringno type-checking32

  $ cat >printer2.asl <<EOF
  > func main () => integer begin
  >   print ("Wow", 2 + 3.14, "some other string");
  >   print ("no type-checking");
  >   print (32);
  >   return 0;
  > end;
  > EOF

  $ aslref printer2.asl
  File printer2.asl, line 2, characters 16 to 24:
    print ("Wow", 2 + 3.14, "some other string");
                  ^^^^^^^^
  ASL Typing error: Illegal application of operator + on types integer {2}
    and real.
  [1]

  $ cat >printer3.asl <<EOF
  > type MyEnum of enumeration { HELLOWORLD_ENUM };
  > type MyInteger of integer {0..100};
  > func main () => integer begin
  >   println ("helloworld");
  >   println (1234);
  >   println ('011');
  >   println (TRUE);
  >   println (0.5);
  >   println (1.0);
  >   println (-1.0);
  >   println (0.0);
  >   println (HELLOWORLD_ENUM);
  >   var abc: MyInteger = 20;
  >   println (abc);
  >   return 0;
  > end;
  > EOF

  $ aslref printer3.asl
  helloworld
  1234
  0x3
  TRUE
  1/2
  1
  -1
  0
  HELLOWORLD_ENUM
  20

  $ cat >print4.asl <<EOF
  > func main () => integer begin
  >   println ((1, 2));
  >   return 0;
  > end;
  > EOF

  $ aslref print4.asl
  File print4.asl, line 2, characters 11 to 17:
    println ((1, 2));
             ^^^^^^
  ASL Typing error: expected singular type, found (integer {1}, integer {2}).
  [1]

