H Examples
  $ aslref --no-exec HExample1.asl
  $ aslref --no-exec HExample2.asl
  $ aslref --no-exec HExample3.asl
  $ aslref --no-exec HExample4.asl
  $ aslref --no-exec HExample5.asl
  $ aslref --no-exec HExample6.asl
  $ aslref --no-exec HExample7.asl
  $ aslref --no-exec HExample8.asl
  $ aslref --no-exec HExample9.asl
  $ aslref --no-exec HExample10.asl
  $ aslref --no-exec HExample11.asl
  $ aslref --no-exec HExample12.asl
  $ aslref --no-exec HExample13.asl
  $ aslref --no-exec HExample14.asl
  $ aslref --no-exec HExample15.asl
  $ aslref --no-exec HExample16.asl
  File HExample16.asl, line 10, characters 19 to 35:
    let x: bits(a) = Reverse{}(bv, b);
                     ^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {1..a} was expected,
    provided integer {8, 16, 32, 64}.
  [1]
  $ aslref --no-exec HExample17.asl
  File HExample17.asl, line 11, characters 19 to 35:
    let x: bits(a) = Reverse{}(bv, b);
                     ^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {1..a} was expected,
    provided integer {32}.
  [1]
  $ aslref --no-exec HExample18.asl
  File HExample18.asl, line 12, characters 20 to 37:
    let x: bits(a2) = Reverse{}(bv, a2);
                      ^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {1..a2} was expected,
    provided integer {8, 16, 32, 64}.
  [1]

T Tests
  $ aslref --no-exec TPositive1.asl

Assignments of constrained integers
  $ aslref --no-exec TPositive2.asl
  $ aslref --no-exec TNegative2-0.asl
  File TNegative2-0.asl, line 4, characters 4 to 41:
      let testA : integer {0..2}    = size;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..2} was expected,
    provided integer {0..3}.
  [1]
  $ aslref --no-exec TNegative2-1.asl
  File TNegative2-1.asl, line 4, characters 4 to 42:
      let testB : integer {8,16,32} = size2;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {8, 16, 32} was expected,
    provided integer {8, 16, 32, 64}.
  [1]
  $ aslref --no-exec TNegative2-2.asl
  File TNegative2-2.asl, line 4, characters 4 to 42:
      let testC : integer {8,16,32} = myInt; // assignment of unconstrained integers to constrained integers is also illegal without a ATC
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {8, 16, 32} was expected,
    provided integer.
  [1]

Propagation of constrained integers
  $ aslref --no-exec TPositive3.asl
  $ aslref --no-exec TPositive3-0.asl
  $ aslref --no-exec TPositive3-1.asl
  $ aslref --no-exec TNegative3.asl

Use of global vars in constraints
  $ aslref --no-exec TPositive4.asl
  $ aslref --no-exec TPositive4-1.asl
  File TPositive4-1.asl, line 5, characters 4 to 54:
      let testB : integer {LET_ALLOWED_NUMS_B}     = 16;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {8} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-2.asl
  File TPositive4-2.asl, line 8, characters 4 to 54:
      let testC : integer {LET_ALLOWED_NUMS_C}     = 16;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {LET_ALLOWED_NUMS_C} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-3.asl
  $ aslref --no-exec TPositive4-4.asl
  File TPositive4-4.asl, line 9, characters 4 to 54:
      let testF : integer {CONFIG_ALLOWED_NUMS}    = 16;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {CONFIG_ALLOWED_NUMS} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-5.asl
  $ aslref --no-exec TReconsider4-0.asl
  File TReconsider4-0.asl, line 13, characters 4 to 54:
      let testA : integer {CONST_ALLOWED_NUMS}     = 16;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {8} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TReconsider4-1.asl
  File TReconsider4-1.asl, line 14, characters 4 to 54:
      let testB : integer {0..CONST_ALLOWED_NUMS}  = 10;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..8} was expected,
    provided integer {10}.
  [1]
  $ aslref --no-exec TNegative4.asl
  File TNegative4.asl, line 5, characters 25 to 41:
      let testA : integer {VAR_ALLOWED_NUMS} = 8; // illegal var's aren't allowed in constraints
                           ^^^^^^^^^^^^^^^^
  ASL Typing error: a pure expression was expected, found VAR_ALLOWED_NUMS,
    which produces the following side-effects:
    [ReadsGlobal "VAR_ALLOWED_NUMS"].
  [1]
  $ aslref --no-exec TNegative4-bis.asl
  File TNegative4-bis.asl, line 5, characters 25 to 41:
      let testA : integer {VAR_ALLOWED_NUMS} = 8; // illegal var's aren't allowed in constraints
                           ^^^^^^^^^^^^^^^^
  ASL Typing error: a pure expression was expected, found VAR_ALLOWED_NUMS,
    which produces the following side-effects:
    [ReadsGlobal "VAR_ALLOWED_NUMS"].
  [1]

Asserted type conversions
  $ aslref --no-exec TPositive5.asl
  $ aslref --no-exec TNegative5-0.asl
  File TNegative5-0.asl, line 5, characters 4 to 38:
      let testA : integer {0..3} = temp; // illegal as value of type integer {8,16} can't be assigned to var of type integer {0..3}.
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..3} was expected,
    provided integer {8, 16}.
  [1]
  $ aslref --no-exec TNegative5-1.asl
  File TNegative5-1.asl, line 4, characters 26 to 41:
      let testB : integer = TRUE as integer;
                            ^^^^^^^^^^^^^^^
  ASL Typing error: cannot perform Asserted Type Conversion on boolean by
    integer.
  [1]

Comparisons
  $ aslref --no-exec TPositive6.asl
  $ aslref --no-exec TNegative6.asl

Named types
  $ aslref --no-exec TPositive7.asl
  $ aslref --no-exec TNegative7.asl
  File TNegative7.asl, line 7, characters 4 to 36:
      let testA : MyOtherSizes = size; // illegal as testA and size are different named types, even though they are the same structure and domain
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of MyOtherSizes was expected,
    provided MyBitsSizes.
  [1]
  $ aslref --no-exec KPositive01.asl

Loops
  $ aslref --no-exec TPositive8.asl
  $ aslref --no-exec TPositive8-1.asl
  File TPositive8-1.asl, line 5, characters 8 to 40:
          let testK : integer {8..31} = i;
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {8..31} was expected,
    provided integer {100..110}.
  [1]
  $ aslref --no-exec TNegative8-0.asl
  File TNegative8-0.asl, line 5, characters 8 to 40:
          let testA : integer {0..7}  = i; // N is an unconstrained integer, so i is also unconstrained
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..7} was expected, provided integer.
  [1]
  $ aslref --no-exec TNegative8-1.asl
  File TNegative8-1.asl, line 5, characters 8 to 40:
          let testB : integer {0..7}  = i; // N is an unconstrained integer, so i is also unconstrained
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..7} was expected, provided integer.
  [1]
  $ aslref --no-exec TNegative8-2.asl
  File TNegative8-2.asl, line 5, characters 8 to 40:
          let testC : integer {7..31} = i; // N is an unconstrained integer, so i is also unconstrained
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {7..31} was expected,
    provided integer.
  [1]
  $ aslref --no-exec TNegative8-3.asl
  File TNegative8-3.asl, line 5, characters 8 to 40:
          let testD : integer {7..31} = i; // N is an unconstrained integer, so i is also unconstrained
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {7..31} was expected,
    provided integer.
  [1]

Bit vector widths defined by constrained integers
  $ aslref --no-exec TPositive9.asl
  $ aslref --no-exec TPositive9-1.asl
  $ aslref --no-exec TNegative9-0.asl
  File TNegative9-0.asl, line 3, characters 4 to 36:
      let testA : bits(8) = Zeros{16};
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(8) was expected, provided bits(16).
  [1]
  $ aslref --no-exec TNegative9-1.asl
  File TNegative9-1.asl, line 3, characters 4 to 59:
      let testB : bits(N) = Zeros{N DIV 4} :: Zeros{N DIV 2}; // bits(3N/4) != bits(N)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(N) was expected,
    provided bits(((3 * N) DIV 4)).
  [1]
  $ aslref --no-exec TNegative9-2.asl
  File TNegative9-2.asl, line 3, characters 4 to 35:
      let testC : bits(M) = Zeros{N};
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(M) was expected, provided bits(N).
  [1]
  $ aslref --no-exec TNegative9-3.asl
  File TNegative9-3.asl, line 3, characters 26 to 34:
      let testD : bits(X) = Zeros{X}; // X isn't a constrained integer, so can't be used as bit width
                            ^^^^^^^^
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative9-4.asl
  File TNegative9-4.asl, line 3, characters 4 to 35:
      let testE : bits(N) = Zeros{8}; // N != 8, even though 8 is in the constraint set for N. N could be 16 after all.
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(N) was expected, provided bits(8).
  [1]

Symbolic execution of bit vector widths expressions
  $ aslref --no-exec TPositive10.asl
  $ aslref --no-exec TPositive10-0.asl
  $ aslref --no-exec TPositive10-1.asl
  $ aslref --no-exec TNegative10.asl
  File TNegative10.asl, line 8, characters 32 to 38:
      let testA : bits(N) = Zeros{widthN};
                                  ^^^^^^
  ASL Typing error: a pure expression was expected, found widthN, which
    produces the following side-effects: [ReadsLocal "widthN"].
  [1]
  $ aslref --no-exec TNegative10-0.asl
  File TNegative10-0.asl, line 16, characters 4 to 53:
      let testB : bits(letWidthN1) = Zeros{letWidthN2}; // illegal as type bits(letWidthN1) is different from bits(letWidthN2).
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(letWidthN1) was expected,
    provided bits(letWidthN2).
  [1]
  $ aslref --no-exec TNegative10-1.asl
  File TNegative10-1.asl, line 28, characters 4 to 49:
      let testC : bits(tempC3A)   = Zeros{tempC3B}; // illegal, type bits(tempC1) != bits(tempC3B)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(tempC3A) was expected,
    provided bits(tempC3B).
  [1]

Complex symbolic execution of bit vector widths expressions
  $ aslref --no-exec TPositive11.asl
  File TPositive11.asl, line 11, characters 4 to 64:
      let testA : bits(numBits)            = ZerosBytes{numBytes};
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(numBits) was expected,
    provided bits((8 * numBytes)).
  [1]
  $ aslref --no-exec TPositive11-0.asl
  $ aslref --no-exec TPositive11-1.asl

ATC's on bit vectors
  $ aslref --no-exec TPositive12.asl
  $ aslref --no-exec TNegative12.asl
  File TNegative12.asl, line 3, characters 16 to 32:
      let testA = N     as bits(8); // ATC's can't change structure.
                  ^^^^^^^^^^^^^^^^
  ASL Typing error: cannot perform Asserted Type Conversion on integer {8, 16}
    by bits(8).
  [1]

Large constraint sets
  $ aslref TPositive13.asl
  File TPositive13.asl, line 8, characters 17 to 34:
      let testA =  UInt(a) * UInt(b);
                   ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TPositive13.asl, line 8, characters 17 to 34:
      let testA =  UInt(a) * UInt(b);
                   ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TPositive13.asl, line 8, characters 4 to 35:
      let testA =  UInt(a) * UInt(b);
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref --no-exec TDegraded13.asl
  File TDegraded13.asl, line 7, characters 29 to 46:
      let temp               = UInt(a) * UInt(b);
                               ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13.asl, line 7, characters 29 to 46:
      let temp               = UInt(a) * UInt(b);
                               ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13.asl, line 7, characters 4 to 47:
      let temp               = UInt(a) * UInt(b);
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref --no-exec TDegraded13-sets1.asl
  File TDegraded13-sets1.asl, line 3, characters 10 to 27:
    var z = UInt(a) * UInt(b);
            ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets1.asl, line 3, characters 10 to 27:
    var z = UInt(a) * UInt(b);
            ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets1.asl, line 3, characters 2 to 28:
    var z = UInt(a) * UInt(b);
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref --no-exec TDegraded13-sets2.asl
  File TDegraded13-sets2.asl, line 3, characters 10 to 27:
    var z = UInt(a) * UInt(b);
            ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets2.asl, line 3, characters 10 to 27:
    var z = UInt(a) * UInt(b);
            ^^^^^^^^^^^^^^^^^
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets2.asl, line 3, characters 2 to 28:
    var z = UInt(a) * UInt(b);
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

Named types for bit vector widths
  $ aslref --no-exec TPositive14.asl
  $ aslref --no-exec TNegative14-0.asl
  File TNegative14-0.asl, line 6, characters 4 to 32:
      let tempA : NamedTypeB = w1;        // illegal, not the same type
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of NamedTypeB was expected, provided NamedTypeA.
  [1]
  $ aslref --no-exec TNegative14-1.asl
  File TNegative14-1.asl, line 7, characters 4 to 39:
      let testB : bits(w1)   = Zeros{w2}; // illegal, just because w1 and w2 are the same type doesn't mean they are the same value, so
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(w1) was expected, provided bits(w2).
  [1]

Bit slice expressions
  $ aslref --no-exec TPositive15.asl
  $ aslref --no-exec TReconsider15.asl
  $ aslref --no-exec TNegative15-0.asl
  File TNegative15-0.asl, line 6, characters 20 to 37:
      let testA     = 0xA55A1234[x+7:x];  // The RHS width express does not result in a constrained integer, so even though the width is
                      ^^^^^^^^^^^^^^^^^
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-1.asl
  File TNegative15-1.asl, line 6, characters 20 to 38:
      let testB     = 0xA55A1234[0 +: x]; // illegal, bit width isn't a constrained integer
                      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-2.asl
  File TNegative15-2.asl, line 6, characters 20 to 38:
      let testC     = 0xA55A1234[0 *: x]; // illegal, bit width isn't a constrained integer
                      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-3.asl
  File TNegative15-3.asl, line 7, characters 20 to 28:
      testD[0 *: x] = Zeros{x}; // Same rules apply to bit slices on LHS
                      ^^^^^^^^
  ASL Typing error: constrained integer expected, provided integer.
  [1]

C Tests
  $ aslref --no-exec CPositive1.asl
  $ aslref --no-exec CPositive1-1.asl
  File CPositive1-1.asl, line 5, characters 4 to 30:
      let z: integer {0..N} = y;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..N} was expected, provided integer.
  [1]
  $ aslref --no-exec CPositive2.asl
  $ aslref --no-exec CPositive3.asl
  File CPositive3.asl, line 5, characters 4 to 31:
      var a : integer {0..N} = b;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0}.
  [1]
  $ aslref --no-exec CPositive4.asl
  $ aslref --no-exec CPositive5.asl
  $ aslref --no-exec CPositive6.asl
  $ aslref --no-exec CPositive7.asl
  File CPositive7.asl, line 4, characters 4 to 31:
      var a: integer{0..2*N} = x;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..(2 * N)} was expected,
    provided integer {0..N}.
  [1]
  $ aslref --no-exec CPositive9.asl
  $ aslref --no-exec CPositive10.asl
  $ aslref --no-exec CPositive11a.asl
  $ aslref --no-exec CPositive11b.asl
  $ aslref --no-exec CPositive12.asl
  $ aslref --no-exec CNegative1.asl
  File CNegative1.asl, line 5, characters 4 to 31:
      var a : integer {0..N} = b; // illegal
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {-1}.
  [1]
  $ aslref --no-exec CNegative2.asl
  File CNegative2.asl, line 4, characters 2 to 11:
    return 3; // illegal
    ^^^^^^^^^
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {3}.
  [1]
  $ aslref --no-exec CNegative3.asl
  File CNegative3.asl, line 8, character 4 to line 13, character 8:
      while x*x + y*y <= 2.0*2.0 do
          let xtemp = (x*x - y*y) + x0;
          y = 2.0*x*y + y0;
          x = xtemp;
          z = z + 1; // should be illegal without ATC
      end;
  ASL Warning: Loop does not have a limit.
  File CNegative3.asl, line 12, characters 8 to 9:
          z = z + 1; // should be illegal without ATC
          ^
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref --no-exec CNegative4.asl
  File CNegative4.asl, line 5, character 4 to line 8, character 6:
      return (
          Ones{5} ::
          x
      );
  ASL Typing error: a subtype of bits(64) was expected, provided bits((N + 5)).
  [1]
  $ aslref --no-exec CNegative5.asl
  File CNegative5.asl, line 13, characters 2 to 33:
    printLengths{12}(3, Zeros{12}); // illegal
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {12} was expected,
    provided integer {3}.
  [1]
  $ aslref --no-exec CNegative6.asl
  File CNegative6.asl, line 4, characters 2 to 15:
    return N + 1; // illegal
    ^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref --no-exec CNegative7.asl
  File CNegative7.asl, line 8, characters 9 to 26:
    return GetBitAt{M}(x, M); // illegal
           ^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..(M - 1)} was expected,
    provided integer {M}.
  [1]
  $ aslref --no-exec CNegative8.asl
  File CNegative8.asl, line 7, characters 4 to 5:
      a = b; // illegal, would require ATC
      ^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0..M}.
  [1]
  $ aslref --no-exec CNegative10.asl
  File CNegative10.asl, line 7, characters 8 to 9:
          a = b; // illegal; only the static type is considered for type-checking
          ^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0..M}.
  [1]
  $ aslref --no-exec CNegative11.asl
  File CNegative11.asl, line 5, characters 4 to 5:
      z = M; // illegal
      ^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {M}.
  [1]
  $ aslref --no-exec CNegative12.asl
  File CNegative12.asl, line 2, characters 56 to 57:
  func negative12{N}(bv : bits(N), N: integer, bv2 : bits({0..N}))
                                                          ^
  ASL Error: Cannot parse.
  [1]

Extra tests by ASLRef team
  $ aslref NegParam.asl
  File NegParam.asl, line 3, characters 2 to 28:
    let x: integer {0..N} = 0;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0}.
  [1]

  $ aslref --no-exec order-insensitive.asl
