  $ aslref --no-exec good-slice_equivalence.asl
  $ aslref --no-exec good-scope1.asl

  $ aslref --no-exec bad-scope1.asl
  File bad-scope1.asl, line 1, character 20 to line 5, character 1:
  ASL Typing error:
    bitfields `sub` and `sub.sub` are in the same scope but define different slices of the containing bitvector type: [1:0] and [0], respectively.
  [1]

  $ aslref --no-exec bad-scope2.asl
  File bad-scope2.asl, line 1, character 20 to line 7, character 1:
  ASL Typing error:
    bitfields `sub` and `sub.sub.sub` are in the same scope but define different slices of the containing bitvector type: [1:0] and [1], respectively.
  [1]

  $ aslref --no-exec bad-scope3.asl
  File bad-scope3.asl, line 1, character 20 to line 9, character 1:
  ASL Typing error:
    bitfields `sub.sub.lowest` and `lowest` are in the same scope but define different slices of the containing bitvector type: [0, 1] and [1:0], respectively.
  [1]
