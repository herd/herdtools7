  $ cat >lca1.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 2 else 3;
  >   let a: integer = x;
  >   let b: integer {2, 3} = x;
  >   let c: real = x;
  > end;
  > EOF

  $ aslref lca1.asl
  File lca1.asl, line 6, characters 2 to 18:
    let c: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided integer {2, 3}.
  [1]

  $ cat >lca2.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 2 as integer else 3;
  >   let a: integer = x;
  >   let b: integer {2, 3} = x;
  > end;
  > EOF

  $ aslref lca2.asl
  File lca2.asl, line 5, characters 2 to 28:
    let b: integer {2, 3} = x;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {2, 3} was expected, provided integer.
  [1]

  $ cat >lca3.asl <<EOF
  > func main {N} (bv: bits(N)) => integer
  > begin
  >   let x = if ARBITRARY: boolean then N else 3;
  >   let a: integer = x;
  >   let b: integer {N} = x;
  > end;
  > EOF

  $ aslref lca3.asl
  File lca3.asl, line 5, characters 2 to 25:
    let b: integer {N} = x;
    ^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of integer {N} was expected,
    provided integer {3, N}.
  [1]

  $ cat >lca4.asl <<EOF
  > func main {N} (bv: bits(N)) => integer
  > begin
  >   let x = if ARBITRARY: boolean then 3 as integer {0..N} else 3;
  >   let a: real = x;
  > end;
  > EOF

  $ aslref lca4.asl
  File lca4.asl, line 4, characters 2 to 18:
    let a: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided integer {0..N, 3}.
  [1]

  $ cat >lca5.asl <<EOF
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then TRUE else 3;
  > end;
  > EOF

  $ aslref lca5.asl
  File lca5.asl, line 3, characters 10 to 48:
    let x = if ARBITRARY: boolean then TRUE else 3;
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error: cannot find a common ancestor to those two types boolean and
    integer {3}.
  [1]

  $ cat >lca6.asl <<EOF
  > type T1 of integer;
  > type T2 of T1;
  > type T3 of T1;
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 3 as T3 else 2 as T2;
  >   let a: real = x;
  > end;
  > EOF

  $ aslref lca6.asl
  File lca6.asl, line 7, characters 2 to 18:
    let a: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided integer.
  [1]

  $ cat >lca7.asl <<EOF
  > type T1 of integer;
  > type T2 of boolean;
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 3 as T1 else 2 as T2;
  > end;
  > EOF

  $ aslref lca7.asl
  File lca7.asl, line 5, characters 50 to 57:
    let x = if ARBITRARY: boolean then 3 as T1 else 2 as T2;
                                                    ^^^^^^^
  ASL Type error: cannot perform Asserted Type Conversion on integer {2} by T2.
  [1]

  $ cat >lca8.asl <<EOF
  > type T1 of bits (3) { [2] b1 };
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then '101' as T1 else '101' as bits(3);
  >   let a: real = x;
  > end;
  > EOF

  $ aslref lca8.asl
  File lca8.asl, line 5, characters 2 to 18:
    let a: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided bits(3).
  [1]

  $ cat >lca9.asl <<EOF
  > type T1 of bits (3) { [2] b1 };
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then '101' as T1 else '101' as bits (3) { [2] b1 };
  >   let a: bits(3) { [2] b1 } = x;
  >   let b: real = x;
  > end;
  > EOF

  $ aslref lca9.asl
  File lca9.asl, line 6, characters 2 to 18:
    let b: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided T1.
  [1]

  $ cat >lca10.asl <<EOF
  > type T1 of integer;
  > type T2 of integer;
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 3 as T1 else 2 as T2;
  >   let a: integer = x;
  >   let b: real = x;
  > end;
  > EOF

  $ aslref lca10.asl
  File lca10.asl, line 7, characters 2 to 18:
    let b: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided integer.
  [1]

  $ cat >lca11.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then 3 as T1 else 2 as integer;
  >   let a: T1 = x;
  >   return 0;
  > end;
  > EOF

  $ aslref lca11.asl

  $ cat >lca12.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   let x = if ARBITRARY: boolean then (3 as integer, 2 as T1) else (3 as T1, 2 as integer);
  >   let a: (T1, T1) = x;
  >   return 0;
  > end;
  > EOF

  $ aslref lca12.asl

  $ cat >lca13.asl <<EOF
  > func main () => integer
  > begin
  >   let v : (integer{3,1}, integer{2,4}) = if ARBITRARY: boolean then (3, 2) else (1, 4);
  >   return 0;
  > end;
  > EOF

  $ aslref lca13.asl

  $ cat >lca14.asl <<EOF
  > type T1 of integer;
  > func main () => integer
  > begin
  >   var a: array[[4]] of integer;
  >   var b: array[[4]] of T1;
  >   let x = if ARBITRARY: boolean then a else b;
  >   let c: real = x;
  > end;
  > EOF

  $ aslref lca14.asl
  File lca14.asl, line 7, characters 2 to 18:
    let c: real = x;
    ^^^^^^^^^^^^^^^^
  ASL Type error: a subtype of real was expected, provided array [[4]] of T1.
  [1]
