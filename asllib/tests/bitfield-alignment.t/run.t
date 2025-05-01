  $ aslref --no-exec good-slice_equivalence.asl
  $ aslref --no-exec good-scope1.asl

  $ aslref --no-exec bad-scope1.asl
  File bad-scope1.asl, line 1, character 20 to line 5, character 1:
  type Nested_Type of bits(2) {
      [1:0] sub {
          [0] sub
      }
  };
  ASL Typing error:
    bitfields `sub` and `sub.sub` are in the same scope but define different slices of the containing bitvector type: [1:0] and [0], respectively.
  [1]

  $ aslref --no-exec bad-scope2.asl
  File bad-scope2.asl, line 1, character 20 to line 7, character 1:
  type Nested_Type of bits(2) {
      [1:0] sub {
          [1:0] sub {
              [1] sub
          }
      }
  };
  ASL Typing error:
    bitfields `sub` and `sub.sub.sub` are in the same scope but define different slices of the containing bitvector type: [1:0] and [1], respectively.
  [1]

  $ aslref --no-exec bad-scope3.asl
  File bad-scope3.asl, line 1, character 20 to line 9, character 1:
  type Nested_Type of bits(2) {
      [1:0] sub {
          [1:0] sub {
              [0,1] lowest
          }
      },
  
      [1,0] lowest
  };
  ASL Typing error:
    bitfields `sub.sub.lowest` and `lowest` are in the same scope but define different slices of the containing bitvector type: [0, 1] and [1:0], respectively.
  [1]

  $ aslref non-constant-width.asl
  File non-constant-width.asl, line 4, characters 10 to 47:
    let x = Zeros{64} as (bits(sub_k) {[0] flag});
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  ASL Typing error: expected constant-time expression, got sub_k, which
    produces the following side-effects: [ReadsLocal "sub_k"].
  [1]
  $ aslref non-constant-global-width.asl
  File non-constant-global-width.asl, line 3, character 0 to line 5,
    character 2:
  type my_type of bits(sub_k) {
    [0] flag
  };
  ASL Typing error: expected constant-time expression, got sub_k, which
    produces the following side-effects: [ReadsGlobal "sub_k"].
  [1]
  $ aslref config-global-width.asl
  File config-global-width.asl, line 3, character 0 to line 5, character 2:
  type my_type of bits(sub_k) {
    [0] flag
  };
  ASL Typing error: expected constant-time expression, got sub_k, which
    produces the following side-effects: [ReadsGlobal "sub_k"].
  [1]
