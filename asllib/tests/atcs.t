Deferred to execution ATCs
  $ cat >atcs.asl <<EOF
  > func main () => integer begin
  >   let x = (3 as integer {42});
  >   return 0;
  > end

  $ aslref atcs.asl
  File atcs.asl, line 2, characters 11 to 12:
  ASL Execution error: Mismatch type:
    value 3 does not belong to type integer {42}.
  [1]

Bad structure ATCs
  $ cat >atcs.asl <<EOF
  > func main () => integer begin
  >   let x = (3 as boolean);
  >   return 0;
  > end

  $ aslref atcs.asl
  File atcs.asl, line 2, characters 11 to 23:
  ASL Typing error: cannot perform Asserted Type Conversion on integer {3} by
    boolean.
  [1]

ATCs on other types
  $ cat >atcs.asl <<EOF
  > func main () => integer begin
  >   let x = ("a string" as string);
  >   return 0;
  > end

  $ aslref atcs.asl

  $ cat >atcs.asl <<EOF
  > type myty of record { a: integer, b: bits(4)};
  > func main () => integer begin
  >   let x = (myty { a = 4, b = Zeros(4) }) as myty;
  >   return 0;
  > end

  $ aslref atcs.asl

  $ cat >atcs.asl <<EOF
  > type myty of record { a: integer, b: bits(4)};
  > type myty2 of record { a: integer { 0..10 }, b: bits(4)};
  > func main () => integer begin
  >   let x = (myty { a = 4, b = Zeros(4) }) as myty;
  >   let y = x as myty2;
  >   return 0;
  > end

  $ aslref atcs.asl
  File atcs.asl, line 5, characters 10 to 20:
  ASL Typing error: cannot perform Asserted Type Conversion on myty by myty2.
  [1]

  $ cat > atcs.asl <<EOF
  > type myty of (integer {0..10}, bits(4));
  > func main () => integer begin
  >   let x = ((42, Zeros(4)) as myty);
  >   return 0;
  > end

  $ aslref atcs.asl
  File atcs.asl, line 3, characters 11 to 33:
  ASL Typing error: cannot perform Asserted Type Conversion on
    (integer {42}, bits(4)) by myty.
  [1]

  $ cat > atcs.asl <<EOF
  > type myty of (integer {42}, bits(4));
  > func main () => integer begin
  >   let x = ((42, Zeros(4)) as myty);
  >   return 0;
  > end

  $ aslref atcs.asl

