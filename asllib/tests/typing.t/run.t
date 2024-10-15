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
  $ aslref --no-exec HExample15.asl
  File HExample15.asl, line 3, characters 10 to 17:
  Warning: Removing some values that would fail with op DIV from constraint set
  {(4 DIV 4), (4 DIV 8), (8 DIV 4), (8 DIV 8)} gave
  {(4 DIV 4), (8 DIV 4), (8 DIV 8)}. Continuing with this constraint set.
  $ aslref --no-exec HExample16.asl
  File HExample16.asl, line 10, characters 19 to 33:
  ASL Typing error: a subtype of integer {1..a} was expected,
    provided integer {8, 16, 32, 64}.
  [1]
  $ aslref --no-exec HExample17.asl
  File HExample17.asl, line 11, characters 19 to 33:
  ASL Typing error: a subtype of integer {1..a} was expected,
    provided integer {32}.
  [1]
  $ aslref --no-exec HExample18.asl
  File HExample18.asl, line 12, characters 20 to 35:
  ASL Typing error: a subtype of integer {1..a2} was expected,
    provided integer {8, 16, 32, 64}.
  [1]

T Tests
  $ aslref --no-exec TPositive1.asl

Assignments of constrained integers
  $ aslref --no-exec TPositive2.asl
  $ aslref --no-exec TNegative2-0.asl
  File TNegative2-0.asl, line 4, characters 4 to 41:
  ASL Typing error: a subtype of integer {0..2} was expected,
    provided integer {0..3}.
  [1]
  $ aslref --no-exec TNegative2-1.asl
  File TNegative2-1.asl, line 4, characters 4 to 42:
  ASL Typing error: a subtype of integer {8, 16, 32} was expected,
    provided integer {8, 16, 32, 64}.
  [1]
  $ aslref --no-exec TNegative2-2.asl
  File TNegative2-2.asl, line 4, characters 4 to 42:
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
  ASL Typing error: a subtype of integer {8} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-2.asl
  File TPositive4-2.asl, line 8, characters 4 to 54:
  ASL Typing error: a subtype of integer {LET_ALLOWED_NUMS_C} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-3.asl
  $ aslref --no-exec TPositive4-4.asl
  File TPositive4-4.asl, line 9, characters 4 to 54:
  ASL Typing error: a subtype of integer {CONFIG_ALLOWED_NUMS} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TPositive4-5.asl
  $ aslref --no-exec TReconsider4-0.asl
  File TReconsider4-0.asl, line 13, characters 4 to 54:
  ASL Typing error: a subtype of integer {8} was expected,
    provided integer {16}.
  [1]
  $ aslref --no-exec TReconsider4-1.asl
  File TReconsider4-1.asl, line 14, characters 4 to 54:
  ASL Typing error: a subtype of integer {0..8} was expected,
    provided integer {10}.
  [1]
  $ aslref --no-exec TNegative4.asl
  File TNegative4.asl, line 5, characters 25 to 41:
  ASL Typing error: a pure expression was expected, found VAR_ALLOWED_NUMS.
  [1]
  $ aslref --no-exec TNegative4-bis.asl
  File TNegative4-bis.asl, line 5, characters 25 to 41:
  ASL Typing error: a pure expression was expected, found VAR_ALLOWED_NUMS.
  [1]

Asserted type conversions
  $ aslref --no-exec TPositive5.asl
  $ aslref --no-exec TNegative5-0.asl
  File TNegative5-0.asl, line 5, characters 4 to 38:
  ASL Typing error: a subtype of integer {0..3} was expected,
    provided integer {8, 16}.
  [1]
  $ aslref --no-exec TNegative5-1.asl
  File TNegative5-1.asl, line 4, characters 26 to 41:
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
  ASL Typing error: a subtype of MyOtherSizes was expected,
    provided MyBitsSizes.
  [1]
  $ aslref --no-exec KPositive01.asl

Loops
  $ aslref --no-exec TPositive8.asl
  $ aslref --no-exec TPositive8-1.asl
  File TPositive8-1.asl, line 5, characters 8 to 40:
  ASL Typing error: a subtype of integer {8..31} was expected,
    provided integer {100..110}.
  [1]
  $ aslref --no-exec TNegative8-0.asl
  File TNegative8-0.asl, line 5, characters 8 to 40:
  ASL Typing error: a subtype of integer {0..7} was expected, provided integer.
  [1]
  $ aslref --no-exec TNegative8-1.asl
  File TNegative8-1.asl, line 5, characters 8 to 40:
  ASL Typing error: a subtype of integer {0..7} was expected, provided integer.
  [1]
  $ aslref --no-exec TNegative8-2.asl
  File TNegative8-2.asl, line 5, characters 8 to 40:
  ASL Typing error: a subtype of integer {7..31} was expected,
    provided integer.
  [1]
  $ aslref --no-exec TNegative8-3.asl
  File TNegative8-3.asl, line 5, characters 8 to 40:
  ASL Typing error: a subtype of integer {7..31} was expected,
    provided integer.
  [1]

Bit vector widths defined by constrained integers
  $ aslref --no-exec TPositive9.asl
  $ aslref --no-exec TPositive9-1.asl
  $ aslref --no-exec TNegative9-0.asl
  File TNegative9-0.asl, line 3, characters 4 to 36:
  ASL Typing error: a subtype of bits(8) was expected, provided bits(16).
  [1]
  $ aslref --no-exec TNegative9-1.asl
  File TNegative9-1.asl, line 3, characters 4 to 59:
  ASL Typing error: a subtype of bits(N) was expected,
    provided bits(((3 * N) DIV 4)).
  [1]
  $ aslref --no-exec TNegative9-2.asl
  File TNegative9-2.asl, line 3, characters 4 to 35:
  ASL Typing error: a subtype of bits(M) was expected, provided bits(N).
  [1]
  $ aslref --no-exec TNegative9-3.asl
  File TNegative9-3.asl, line 3, characters 26 to 34:
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative9-4.asl
  File TNegative9-4.asl, line 3, characters 4 to 35:
  ASL Typing error: a subtype of bits(N) was expected, provided bits(8).
  [1]

Symbolic execution of bit vector widths expressions
  $ aslref --no-exec TPositive10.asl
  $ aslref --no-exec TPositive10-0.asl
  $ aslref --no-exec TPositive10-1.asl
  $ aslref --no-exec TNegative10.asl
  File TNegative10.asl, line 8, characters 32 to 38:
  ASL Typing error: a pure expression was expected, found widthN.
  [1]
  $ aslref --no-exec TNegative10-0.asl
  File TNegative10-0.asl, line 16, characters 4 to 53:
  ASL Typing error: a subtype of bits(letWidthN1) was expected,
    provided bits(letWidthN2).
  [1]
  $ aslref --no-exec TNegative10-1.asl
  File TNegative10-1.asl, line 28, characters 4 to 49:
  ASL Typing error: a subtype of bits(tempC1) was expected,
    provided bits(tempC3B).
  [1]

Complex symbolic execution of bit vector widths expressions
  $ aslref --no-exec TPositive11.asl
  File TPositive11.asl, line 11, characters 4 to 64:
  ASL Typing error: a subtype of bits(numBits) was expected,
    provided bits((8 * numBytes)).
  [1]
  $ aslref --no-exec TPositive11-0.asl
  $ aslref --no-exec TPositive11-1.asl

ATC's on bit vectors
  $ aslref --no-exec TPositive12.asl
  $ aslref --no-exec TNegative12.asl
  File TNegative12.asl, line 3, characters 16 to 32:
  ASL Typing error: cannot perform Asserted Type Conversion on integer {8, 16}
    by bits(8).
  [1]

Large constraint sets
  $ aslref TPositive13.asl
  File TPositive13.asl, line 8, characters 17 to 34:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TPositive13.asl, line 8, characters 17 to 34:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TPositive13.asl, line 10, characters 17 to 34:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  $ aslref --no-exec TDegraded13.asl
  File TDegraded13.asl, line 7, characters 29 to 46:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13.asl, line 7, characters 29 to 46:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  $ if [ $(ocaml -vnum | cut -b 1) = "5" ]; then aslref TDegraded13.asl 2>/dev/null; fi
  $ aslref --no-exec TDegraded13-sets1.asl
  File TDegraded13-sets1.asl, line 3, characters 10 to 27:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets1.asl, line 3, characters 10 to 27:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  $ aslref --no-exec TDegraded13-sets2.asl
  File TDegraded13-sets2.asl, line 3, characters 10 to 27:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.
  File TDegraded13-sets2.asl, line 3, characters 10 to 27:
  Interval too large: [ 0 .. 18446744073709551615 ]. Keeping it as an interval.

Named types for bit vector widths
  $ aslref --no-exec TPositive14.asl
  $ aslref --no-exec TNegative14-0.asl
  File TNegative14-0.asl, line 6, characters 4 to 32:
  ASL Typing error: a subtype of NamedTypeB was expected, provided NamedTypeA.
  [1]
  $ aslref --no-exec TNegative14-1.asl
  File TNegative14-1.asl, line 7, characters 4 to 39:
  ASL Typing error: a subtype of bits(w1) was expected, provided bits(w2).
  [1]

Bit slice expressions
  $ aslref --no-exec TPositive15.asl
  $ aslref --no-exec TReconsider15.asl
  $ aslref --no-exec TNegative15-0.asl
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-1.asl
  File TNegative15-1.asl, line 6, characters 36 to 37:
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-2.asl
  File TNegative15-2.asl, line 6, characters 36 to 37:
  ASL Typing error: constrained integer expected, provided integer.
  [1]
  $ aslref --no-exec TNegative15-3.asl
  File TNegative15-3.asl, line 7, characters 20 to 28:
  ASL Typing error: constrained integer expected, provided integer.
  [1]

C Tests
  $ aslref --no-exec CPositive1.asl
  $ aslref --no-exec CPositive1-1.asl
  File CPositive1-1.asl, line 5, characters 4 to 30:
  ASL Typing error: a subtype of integer {0..N} was expected, provided integer.
  [1]
  $ aslref --no-exec CPositive2.asl
  $ aslref --no-exec CPositive3.asl
  File CPositive3.asl, line 5, characters 4 to 31:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0}.
  [1]
  $ aslref --no-exec CPositive4.asl
  $ aslref --no-exec CPositive5.asl
  $ aslref --no-exec CPositive6.asl
  $ aslref --no-exec CPositive7.asl
  File CPositive7.asl, line 4, characters 4 to 31:
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
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {-1}.
  [1]
  $ aslref --no-exec CNegative2.asl
  File CNegative2.asl, line 4, characters 2 to 11:
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {3}.
  [1]
  $ aslref --no-exec CNegative3.asl
  File CNegative3.asl, line 12, characters 8 to 9:
  ASL Typing error: a subtype of integer {N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref --no-exec CNegative4.asl
  File CNegative4.asl, line 5, character 4 to line 8, character 6:
  ASL Typing error: a subtype of bits(64) was expected, provided bits((N + 5)).
  [1]
  $ aslref --no-exec CNegative5.asl
  File CNegative5.asl, line 13, characters 2 to 30:
  ASL Typing error: a subtype of integer {12} was expected,
    provided integer {3}.
  [1]
  $ aslref --no-exec CNegative6.asl
  File CNegative6.asl, line 4, characters 2 to 15:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {(N + 1)}.
  [1]
  $ aslref --no-exec CNegative7.asl
  File CNegative7.asl, line 8, characters 9 to 23:
  ASL Typing error: a subtype of integer {0..(M - 1)} was expected,
    provided integer {M}.
  [1]
  $ aslref --no-exec CNegative8.asl
  File CNegative8.asl, line 7, characters 4 to 5:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0..M}.
  [1]
  $ aslref --no-exec CNegative10.asl
  File CNegative10.asl, line 7, characters 8 to 9:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0..M}.
  [1]
  $ aslref --no-exec CNegative11.asl
  File CNegative11.asl, line 5, characters 4 to 5:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {M}.
  [1]
  $ aslref --no-exec CNegative12.asl
  File CNegative12.asl, line 2, characters 56 to 57:
  ASL Error: Cannot parse.
  [1]

Extra tests by ASLRef team
  $ aslref NegParam.asl
  File NegParam.asl, line 3, characters 2 to 28:
  ASL Typing error: a subtype of integer {0..N} was expected,
    provided integer {0}.
  [1]

