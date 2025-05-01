(* Constraint multiplication *)
  $ aslref constraint-mul-00.asl
  File constraint-mul-00.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of integer {0..4, 6, 8..9, 12, 16} was expected,
    provided integer {15}.
  [1]

  $ aslref constraint-mul-01.asl
  File constraint-mul-01.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of
    integer {0..16, 18, 20..22, 24..28, 30, 32..33, 35..36, 39..40, 42, 
             44..45, ...} was expected, provided integer {255}.
  [1]

  $ aslref constraint-mul-02.asl
  File constraint-mul-02.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of
    integer {0..36, 38..40, 42, 44..46, 48..52, 54..58, 60, 62..66, 68..70, 72,
             ...} was expected, provided integer {1023}.
  [1]

  $ aslref constraint-mul-03.asl
  File constraint-mul-03.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of
    integer {0..66, 68..70, 72, 74..78, 80..82, 84..88, 90..96, 98..100, 102,
             104..106, ...} was expected, provided integer {4095}.
  [1]

  $ aslref constraint-mul-04.asl
  File constraint-mul-04.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of
    integer {0..130, 132..136, 138, 140..148, 150, 152..156, 158..162,
             164..166, 168..172, 174..178, ...} was expected,
    provided integer {16383}.
  [1]

  $ aslref constraint-mul-05.asl
  File constraint-mul-05.asl, line 10, characters 4 to 6:
      b1 = (A * B) - 1; // Test if discrete or interval representation
      ^^
  ASL Typing error: a subtype of
    integer {0..256, 258..262, 264..268, 270, 272..276, 278..280, 282,
             284..292, 294..306, 308..310, ...} was expected,
    provided integer {65535}.
  [1]

  $ aslref constraint-mul-06.asl
  File constraint-mul-06.asl, line 6, characters 12 to 21:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
              ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..512 and 1..512. Continuing with the
  non-expanded constraints.
  File constraint-mul-06.asl, line 6, characters 4 to 22:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-07.asl
  File constraint-mul-07.asl, line 6, characters 12 to 21:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
              ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with
  the non-expanded constraints.
  File constraint-mul-07.asl, line 6, characters 4 to 22:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-08.asl
  File constraint-mul-08.asl, line 6, characters 12 to 21:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
              ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..2048 and 1..2048. Continuing with
  the non-expanded constraints.
  File constraint-mul-08.asl, line 6, characters 4 to 22:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-09.asl
  File constraint-mul-09.asl, line 6, characters 12 to 21:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
              ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..4096 and 1..4096. Continuing with
  the non-expanded constraints.
  File constraint-mul-09.asl, line 6, characters 4 to 22:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-10.asl
  File constraint-mul-10.asl, line 6, characters 12 to 21:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
              ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..8192 and 1..8192. Continuing with
  the non-expanded constraints.
  File constraint-mul-10.asl, line 6, characters 4 to 22:
      let n = a DIVRM b;     // 10 DIVRM 3 == 3
      ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

Other operations
  $ aslref constraint-div.asl
  File constraint-div.asl, line 7, characters 10 to 17:
    var z = a DIV b;
            ^^^^^^^
  Exploding sets for the binary operation DIV could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File constraint-div.asl, line 7, characters 2 to 18:
    var z = a DIV b;
    ^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref constraint-divrm.asl
  File constraint-divrm.asl, line 7, characters 10 to 19:
    var z = a DIVRM b;
            ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with
  the non-expanded constraints.
  File constraint-divrm.asl, line 7, characters 2 to 20:
    var z = a DIVRM b;
    ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref constraint-minus.asl
  $ aslref constraint-mod.asl
  File constraint-mod.asl, line 7, characters 10 to 17:
    var z = a MOD b;
            ^^^^^^^
  Interval too large: [ 1 .. 1048576 ]. Keeping it as an interval.
  File constraint-mod.asl, line 7, characters 2 to 18:
    var z = a MOD b;
    ^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref constraint-mod-half.asl
  $ aslref constraint-plus.asl
  $ aslref constraint-pow.asl
  File constraint-pow.asl, line 7, characters 10 to 15:
    var z = a ^ b;
            ^^^^^
  Exploding sets for the binary operation ^ could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File constraint-pow.asl, line 7, characters 2 to 16:
    var z = a ^ b;
    ^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref constraint-shl.asl
  File constraint-shl.asl, line 7, characters 10 to 16:
    var z = a << b;
            ^^^^^^
  Exploding sets for the binary operation << could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File constraint-shl.asl, line 7, characters 2 to 17:
    var z = a << b;
    ^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref constraint-shr.asl
  File constraint-shr.asl, line 7, characters 10 to 16:
    var z = a >> b;
            ^^^^^^
  Exploding sets for the binary operation >> could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File constraint-shr.asl, line 7, characters 2 to 17:
    var z = a >> b;
    ^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-div.asl
  File global-constraint-div.asl, line 8, characters 8 to 15:
  var z = a DIV b;
          ^^^^^^^
  Exploding sets for the binary operation DIV could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File global-constraint-div.asl, line 8, characters 0 to 16:
  var z = a DIV b;
  ^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-divrm.asl
  File global-constraint-divrm.asl, line 8, characters 8 to 17:
  var z = a DIVRM b;
          ^^^^^^^^^
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with
  the non-expanded constraints.
  File global-constraint-divrm.asl, line 8, characters 0 to 18:
  var z = a DIVRM b;
  ^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-minus.asl
  $ aslref global-constraint-mod.asl
  File global-constraint-mod.asl, line 8, characters 8 to 15:
  var z = a MOD b;
          ^^^^^^^
  Interval too large: [ 1 .. 1048576 ]. Keeping it as an interval.
  File global-constraint-mod.asl, line 8, characters 0 to 16:
  var z = a MOD b;
  ^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-plus.asl
  $ aslref global-constraint-pow.asl
  File global-constraint-pow.asl, line 8, characters 8 to 13:
  var z = a ^ b;
          ^^^^^
  Exploding sets for the binary operation ^ could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File global-constraint-pow.asl, line 8, characters 0 to 14:
  var z = a ^ b;
  ^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-shl.asl
  File global-constraint-shl.asl, line 8, characters 8 to 14:
  var z = a << b;
          ^^^^^^
  Exploding sets for the binary operation << could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File global-constraint-shl.asl, line 8, characters 0 to 15:
  var z = a << b;
  ^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-shr.asl
  File global-constraint-shr.asl, line 8, characters 8 to 14:
  var z = a >> b;
          ^^^^^^
  Exploding sets for the binary operation >> could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 0..1024. Continuing with the
  non-expanded constraints.
  File global-constraint-shr.asl, line 8, characters 0 to 15:
  var z = a >> b;
  ^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-constraint-mul.asl
  File global-constraint-mul.asl, line 7, characters 8 to 13:
  var z = a * b;
          ^^^^^
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File global-constraint-mul.asl, line 7, characters 0 to 14:
  var z = a * b;
  ^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

With pending constraints
  $ aslref pending.asl
  File pending.asl, line 6, characters 24 to 29:
    var z : integer {-} = a * b;
                          ^^^^^
  Interval too large: [ 1 .. 1048576 ]. Keeping it as an interval.
  File pending.asl, line 6, characters 2 to 30:
    var z : integer {-} = a * b;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
  $ aslref global-pending.asl
  File global-pending.asl, line 7, characters 21 to 26:
  var z: integer {-} = a * b;
                       ^^^^^
  Exploding sets for the binary operation * could result in a constraint set
  bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with the
  non-expanded constraints.
  File global-pending.asl, line 7, characters 0 to 27:
  var z: integer {-} = a * b;
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
