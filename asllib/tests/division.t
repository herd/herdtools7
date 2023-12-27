Division by zero:

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 DIV 0;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator DIV on types integer {6}
    and integer {0}
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 DIVRM 0;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 28:
  ASL Typing error: Illegal application of operator DIVRM on types integer {6}
    and integer {0}
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 MOD 0;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator MOD on types integer {6}
    and integer {0}
  [1]

Unsupported divisions (caught at time-checking time):

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 DIV -3;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 27:
  ASL Typing error: Illegal application of operator DIV on types integer {6}
    and integer {(- 3)}
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 DIVRM -3;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 29:
  ASL Typing error: Illegal application of operator DIVRM on types integer {6}
    and integer {(- 3)}
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6 MOD -3;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 27:
  ASL Typing error: Illegal application of operator MOD on types integer {6}
    and integer {(- 3)}
  [1]

The following error is a runtime error because we don't know how to catch those at compile-time:

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 5 DIV 3;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 19 to 26:
  ASL Typing error: Illegal application of operator DIV on types integer {5}
    and integer {3}
  [1]

For completeness, those operations are runtime errors:

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6;
  >   let y: integer = -3;
  >   let z = x DIV y;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  ASL Execution error: Illegal application of operator DIV for values 6 and -3.
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6;
  >   let y: integer = -3;
  >   let z = x DIVRM y;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  ASL Execution error: Illegal application of operator DIVRM for values 6
    and -3.
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 6;
  >   let y: integer = -3;
  >   let z = x MOD y;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  ASL Execution error: Illegal application of operator MOD for values 6 and -3.
  [1]

  $ cat >div.asl <<EOF
  > func main () => integer
  > begin
  >   let x: integer = 5;
  >   let y: integer = 3;
  >   let z = x DIV y;
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  ASL Execution error: Illegal application of operator DIV for values 5 and 3.
  [1]

More complicated examples:

  $ cat >div.asl <<EOF
  > func foo {N} (x: bits(N))
  > begin
  >   let y = 5 DIV N;
  > end
  > func main () => integer
  > begin
  >   foo ('1');
  >   foo ('');
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 10 to 17:
  ASL Typing error: Illegal application of operator DIV on types integer {5}
    and an under-constrained integer
  [1]

  $ cat >div.asl <<EOF
  > func foo {N} (x: bits(N))
  > begin
  >   let y = 5 DIV N;
  > end
  > func main () => integer
  > begin
  >   foo ('1');
  >   foo ('11');
  >   return 0;
  > end
  > EOF

  $ aslref div.asl
  File div.asl, line 3, characters 10 to 17:
  ASL Typing error: Illegal application of operator DIV on types integer {5}
    and an under-constrained integer
  [1]

Example from asltools:

  $ cat >div.asl <<EOF
  > var glob_a: integer {2, 4, 8};
  > var glob_b: integer {0, 1, 2};
  > var glob_c: integer {0 .. 2};
  > func main() => integer
  > begin
  >   let x = glob_a DIV glob_b;
  > end

  $ aslref div.asl
  File div.asl, line 6, characters 10 to 27:
  ASL Typing error: Illegal application of operator DIV on types
    integer {2, 4, 8} and integer {0, 1, 2}
  [1]
