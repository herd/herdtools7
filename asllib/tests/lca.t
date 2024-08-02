  $ cat >lca1.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then 2 else 3;
  >   let -: integer = x;
  >   let -: integer {2, 3} = x;
  >   let -: real = x;
  > end

  $ aslref lca1.asl
  File lca1.asl, line 6, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided integer {2, 3}.
  [1]

  $ cat >lca2.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then 2 as integer else 3;
  >   let -: integer = x;
  >   let -: integer {2, 3} = x;
  > end

  $ aslref lca2.asl
  File lca2.asl, line 5, characters 2 to 28:
  ASL Typing error: a subtype of integer {2, 3} was expected, provided integer.
  [1]

  $ cat >lca3.asl <<EOF
  > func main {N} (bv: bits(N)) => integer
  > begin
  >   let x = if UNKNOWN: boolean then N else 3;
  >   let -: integer = x;
  >   let -: integer {N} = x;
  > end

  $ aslref lca3.asl
  File lca3.asl, line 5, characters 2 to 25:
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {N, 3}.
  [1]

  $ cat >lca4.asl <<EOF
  > func main {N} (bv: bits(N)) => integer
  > begin
  >   let x = if UNKNOWN: boolean then 3 as integer {0..N} else 3;
  >   let -: real = x;
  > end

  $ aslref lca4.asl
  File lca4.asl, line 4, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided integer {0..N, 3}.
  [1]

  $ cat >lca5.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then TRUE else 3;
  > end

  $ aslref lca5.asl
  File lca5.asl, line 3, characters 10 to 46:
  ASL Typing error: cannot find a common ancestor to those two types boolean
    and integer {3}.
  [1]

  $ cat >lca6.asl <<EOF
  > type T1 of integer;
  > type T2 of T1;
  > type T3 of T1;
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then 3 as T3 else 2 as T2;
  >   let -: real = x;
  > end

  $ aslref lca6.asl
  File lca6.asl, line 7, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided integer.
  [1]

  $ cat >lca7.asl <<EOF
  > type T1 of integer;
  > type T2 of boolean;
  > func main () => integer
  > begin
  >   let - = if UNKNOWN: boolean then 3 as T1 else 2 as T2;
  > end

  $ aslref lca7.asl
  File lca7.asl, line 5, characters 48 to 55:
  ASL Typing error: cannot perform Asserted Type Conversion on integer {2} by
    T2.
  [1]

  $ cat >lca8.asl <<EOF
  > type T1 of bits (3) { [2] b1 };
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then '101' as T1 else '101' as bits(3);
  >   let -: real = x;
  > end

  $ aslref lca8.asl
  File lca8.asl, line 5, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided bits(3).
  [1]

  $ cat >lca9.asl <<EOF
  > type T1 of bits (3) { [2] b1 };
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then '101' as T1 else '101' as bits (3) { [2] b1 };
  >   let -: bits(3) { [2] b1 } = x;
  >   let -: real = x;
  > end

  $ aslref lca9.asl
  File lca9.asl, line 6, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided T1.
  [1]

  $ cat >lca10.asl <<EOF
  > type T1 of integer;
  > type T2 of integer;
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then 3 as T1 else 2 as T2;
  >   let -: integer = x;
  >   let -: real = x;
  > end

  $ aslref lca10.asl
  File lca10.asl, line 7, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided integer.
  [1]

  $ cat >lca11.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then 3 as T1 else 2 as integer;
  >   let -: T1 = x;
  >   return 0;
  > end

  $ aslref lca11.asl

  $ cat >lca12.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   let x = if UNKNOWN: boolean then (3 as integer, 2 as T1) else (3 as T1, 2 as integer);
  >   let -: (T1, T1) = x;
  >   return 0;
  > end

  $ aslref lca12.asl

  $ cat >lca13.asl <<EOF
  > func main () => integer
  > begin
  >   let v : (integer{3,1}, integer{2,4}) = if UNKNOWN: boolean then (3, 2) else (1, 4);
  >   return 0;
  > end

  $ aslref lca13.asl

  $ cat >lca14.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   var a: array[4] of integer;
  >   var b: array[4] of T1;
  >   let x = if UNKNOWN: boolean then a else b;
  >   let -: real = x;
  > end

  $ aslref lca14.asl
  File lca14.asl, line 7, characters 2 to 18:
  ASL Typing error: a subtype of real was expected, provided array [4] of T1.
  [1]
