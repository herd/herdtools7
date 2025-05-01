Deferred to execution ATCs
  $ cat >atcs1.asl <<EOF
  > func main () => integer begin
  >   let x = (3 as integer {42});
  >   return 0;
  > end;
  > EOF

  $ aslref atcs1.asl
  File atcs1.asl, line 2, characters 11 to 12:
    let x = (3 as integer {42});
             ^
  ASL Execution error: Mismatch type:
    value 3 does not belong to type integer {42}.
  [1]

Bad structure ATCs
  $ cat >atcs2.asl <<EOF
  > func main () => integer begin
  >   let x = (3 as boolean);
  >   return 0;
  > end;
  > EOF

  $ aslref atcs2.asl
  File atcs2.asl, line 2, characters 11 to 23:
    let x = (3 as boolean);
             ^^^^^^^^^^^^
  ASL Typing error: cannot perform Asserted Type Conversion on integer {3} by
    boolean.
  [1]

ATCs on other types
  $ cat >atcs3.asl <<EOF
  > func main () => integer begin
  >   let x = ("a string" as string);
  >   return 0;
  > end;
  > EOF

  $ aslref atcs3.asl

  $ cat >atcs4.asl <<EOF
  > type myty of record { a: integer, b: bits(4)};
  > func main () => integer begin
  >   let x = (myty { a = 4, b = Zeros{4} }) as myty;
  >   return 0;
  > end;
  > EOF

  $ aslref atcs4.asl

  $ cat >atcs5.asl <<EOF
  > type myty of record { a: integer, b: bits(4)};
  > type myty2 of record { a: integer { 0..10 }, b: bits(4)};
  > func main () => integer begin
  >   let x = (myty { a = 4, b = Zeros{4} }) as myty;
  >   let y = x as myty2;
  >   return 0;
  > end;
  > EOF

  $ aslref atcs5.asl
  File atcs5.asl, line 5, characters 10 to 20:
    let y = x as myty2;
            ^^^^^^^^^^
  ASL Typing error: cannot perform Asserted Type Conversion on myty by myty2.
  [1]

  $ cat > atcs6.asl <<EOF
  > type myty of (integer {0..10}, bits(4));
  > func main () => integer begin
  >   let x = ((42, Zeros{4}) as myty);
  >   return 0;
  > end;
  > EOF

  $ aslref atcs6.asl
  File atcs6.asl, line 3, characters 11 to 25:
    let x = ((42, Zeros{4}) as myty);
             ^^^^^^^^^^^^^^
  ASL Execution error: Mismatch type:
    value [42, 0x0] does not belong to type (integer {0..10}, bits(4)).
  [1]

  $ cat > atcs7.asl <<EOF
  > type myty of (integer {42}, bits(4));
  > func main () => integer begin
  >   let x = ((42, Zeros{4}) as myty);
  >   return 0;
  > end;
  > EOF

  $ aslref atcs7.asl

  $ cat > atcs8.asl <<EOF
  > type A of record{ a: integer};
  > type B subtypes A;
  > func main () => integer
  > begin
  >     let x: A = B { a = 0 };
  >     var a: array[[10]] of B;
  >     let b = a as array[[10]] of A;
  >     return 0;
  > end;
  > EOF

  $ aslref atcs8.asl

ATCs in types:
  $ cat > atcs9.asl <<EOF
  > let bv : bits(1 as integer{2}) = Ones{1};
  > EOF

  $ aslref atcs9.asl
  File atcs9.asl, line 1, characters 14 to 29:
  let bv : bits(1 as integer{2}) = Ones{1};
                ^^^^^^^^^^^^^^^
  ASL Typing error: a pure expression was expected, found 1 as integer {2},
    which produces the following side-effects: [PerformsAssertions].
  [1]
