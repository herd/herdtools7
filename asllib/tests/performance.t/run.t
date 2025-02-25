(* Constraint multiplication *)
  $ aslref constraint-mul-00.asl
  File constraint-mul-00.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..4} gave {1..4}. Continuing with this constraint set.
  File constraint-mul-00.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of integer {0..4, 6, 8..9, 12, 16} was expected,
    provided integer {15}.
  [1]

  $ aslref constraint-mul-01.asl
  File constraint-mul-01.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..16} gave {1..16}. Continuing with this constraint set.
  File constraint-mul-01.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of
    integer {0..16, 18, 20..22, 24..28, 30, 32..33, 35..36, 39..40, 42, 
             44..45, ...} was expected, provided integer {255}.
  [1]

  $ aslref constraint-mul-02.asl
  File constraint-mul-02.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..32} gave {1..32}. Continuing with this constraint set.
  File constraint-mul-02.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of
    integer {0..36, 38..40, 42, 44..46, 48..52, 54..58, 60, 62..66, 68..70, 72,
             ...} was expected, provided integer {1023}.
  [1]

  $ aslref constraint-mul-03.asl
  File constraint-mul-03.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..64} gave {1..64}. Continuing with this constraint set.
  File constraint-mul-03.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of
    integer {0..66, 68..70, 72, 74..78, 80..82, 84..88, 90..96, 98..100, 102,
             104..106, ...} was expected, provided integer {4095}.
  [1]

  $ aslref constraint-mul-04.asl
  File constraint-mul-04.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..128} gave {1..128}. Continuing with this constraint set.
  File constraint-mul-04.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of
    integer {0..130, 132..136, 138, 140..148, 150, 152..156, 158..162,
             164..166, 168..172, 174..178, ...} was expected,
    provided integer {16383}.
  [1]

  $ aslref constraint-mul-05.asl
  File constraint-mul-05.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..256} gave {1..256}. Continuing with this constraint set.
  File constraint-mul-05.asl, line 10, characters 4 to 6:
  ASL Typing error: a subtype of
    integer {0..256, 258..262, 264..268, 270, 272..276, 278..280, 282,
             284..292, 294..306, 308..310, ...} was expected,
    provided integer {65535}.
  [1]

  $ aslref constraint-mul-06.asl
  File constraint-mul-06.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..512} gave {1..512}. Continuing with this constraint set.
  File constraint-mul-06.asl, line 6, characters 12 to 21:
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..512 and 1..512. Continuing with the
  non-expanded constraints.
  File constraint-mul-06.asl, line 6, characters 4 to 22:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-07.asl
  File constraint-mul-07.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..1024} gave {1..1024}. Continuing with this constraint set.
  File constraint-mul-07.asl, line 6, characters 12 to 21:
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..1024 and 1..1024. Continuing with
  the non-expanded constraints.
  File constraint-mul-07.asl, line 6, characters 4 to 22:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-08.asl
  File constraint-mul-08.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..2048} gave {1..2048}. Continuing with this constraint set.
  File constraint-mul-08.asl, line 6, characters 12 to 21:
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..2048 and 1..2048. Continuing with
  the non-expanded constraints.
  File constraint-mul-08.asl, line 6, characters 4 to 22:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-09.asl
  File constraint-mul-09.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..4096} gave {1..4096}. Continuing with this constraint set.
  File constraint-mul-09.asl, line 6, characters 12 to 21:
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..4096 and 1..4096. Continuing with
  the non-expanded constraints.
  File constraint-mul-09.asl, line 6, characters 4 to 22:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]

  $ aslref constraint-mul-10.asl
  File constraint-mul-10.asl, line 6, characters 12 to 21:
  Warning: Removing some values that would fail with op DIVRM from constraint
  set {0..8192} gave {1..8192}. Continuing with this constraint set.
  File constraint-mul-10.asl, line 6, characters 12 to 21:
  Exploding sets for the binary operation DIVRM could result in a constraint
  set bigger than 2^17 with constraints 0..8192 and 1..8192. Continuing with
  the non-expanded constraints.
  File constraint-mul-10.asl, line 6, characters 4 to 22:
  ASL Typing error: type used to define storage item is the result of precision
    loss.
  [1]
