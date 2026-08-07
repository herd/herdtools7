Bad enumeration
  $ cat >bad-types1.asl <<EOF
  > type t of enumeration {};
  > EOF

  $ aslref bad-types1.asl
  File bad-types1.asl, line 1, characters 23 to 24:
  type t of enumeration {};
                         ^
  ASL Grammar error (BE_PE): Cannot parse.
  [1]

Invalid bitfields
==================

Bad fields
  $ cat >bad-types2.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [2+:1] a,
  > };
  > EOF

  $ aslref bad-types2.asl
  File bad-types2.asl, line 1, character 10 to line 4, character 1:
  type t of bits(12) {
    [10: 3] a,
    [2+:1] a,
  };
  ASL Type error (TE_IAD): Identifier "a" is already declared.
  [1]

Overlapping slices
  $ cat >bad-types3.asl <<EOF
  > type t of bits(64) {
  >   [23: 0] a,
  >   [10: 0, 3+: 2] b,
  > };
  > EOF

  $ aslref bad-types3.asl
  File bad-types3.asl, line 1, character 10 to line 4, character 1:
  type t of bits(64) {
    [23: 0] a,
    [10: 0, 3+: 2] b,
  };
  ASL Static error (TE_BS): Slices 0+:11, 3+:2 overlap.
  [1]

Bad slices
  $ cat >bad-types4.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [14:12] b,
  > };
  > EOF

  $ aslref bad-types4.asl
  File bad-types4.asl, line 1, character 10 to line 4, character 1:
  type t of bits(12) {
    [10: 3] a,
    [14:12] b,
  };
  ASL Static error (TE_BS):
    Slice 12+:3 is outside the bounds of a bitvector of length 12.
  [1]

  $ cat >bad-types5.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [-2+:1] b,
  > };
  > EOF

  $ aslref bad-types5.asl
  File bad-types5.asl, line 1, character 10 to line 4, character 1:
  type t of bits(12) {
    [10: 3] a,
    [-2+:1] b,
  };
  ASL Static error (TE_BS):
    Slice (- 2)+:1 is outside the bounds of a bitvector of length 12.
  [1]

  $ cat >bad-types6.asl <<EOF
  > type t of bits(12) {
  >   [10: 3] a,
  >   [7+:3] b {
  >     [1] d,
  >     [10:8] c,
  >   },
  > };
  > EOF

  $ aslref bad-types6.asl
  File bad-types6.asl, line 1, character 10 to line 7, character 1:
  type t of bits(12) {
    [10: 3] a,
    [7+:3] b {
      [1] d,
      [10:8] c,
    },
  };
  ASL Static error (TE_BS):
    Slice 8+:3 is outside the bounds of a bitvector of length 3.
  [1]

Empty types
===========

Arbitrary of empty type

  $ cat >bad-types7.asl <<EOF
  > func main () => integer
  > begin
  >    let b: integer {1..0} = ARBITRARY: integer {1..0};
  >    return 0;
  > end;
  > EOF

  $ aslref bad-types7.asl
  File bad-types7.asl, line 3, characters 38 to 52:
     let b: integer {1..0} = ARBITRARY: integer {1..0};
                                        ^^^^^^^^^^^^^^
  ASL Dynamic error (DE_AET): ARBITRARY cannot produce a value of type
    integer {1..0} because the type has an empty domain.
  [1]

Base value of empty type

  $ cat >bad-types8.asl <<EOF
  > func main () => integer
  > begin
  >   var b: integer {1..0};
  >   return 0;
  > end;
  > EOF

  $ aslref bad-types8.asl
  File bad-types8.asl, line 3, characters 2 to 24:
    var b: integer {1..0};
    ^^^^^^^^^^^^^^^^^^^^^^
  ASL Type error (TE_NBV):
    Cannot determine a base value for empty type integer {1..0}.
  [1]
