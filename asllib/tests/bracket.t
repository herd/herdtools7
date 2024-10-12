[] overloading revamp

1) Replace [] around nested bitvector's or record's field name list with () parentheses.

2) Replace [] around bitvectors to be concatenated with :: bit concatenation operator

3) Remove unadorned [lsb] for single-bit slice and add
.  a) [lsb:] as syntactic sugar for [lsb+:1] (single-bit slice)
.  b) [:wid] as syntactic sugar for [0+:wid] (least sig wid bits)

  $ cat >bracket1.asl <<EOF
  > type MyType of bits(4) { [3:2] A, [1:] B };
  > func main() => integer begin
  >   var x : MyType = '0000';
  >   print(x);
  >   print(x.(A));
  >   x.(A, B) = '110';
  >   print(x);
  >   print(x.(A, B));
  >   print(x[2:]);
  >   print(x[:2]);
  >   let z = x.A :: '1010';
  >   print(z);
  >   [x, x] = '11100111';
  >   print(x);
  >   return 0;
  > end
  > EOF

  $ aslref bracket1.asl
  '0000'
  '00'
  '1100'
  '110'
  '1'
  '00'
  '111010'
  '0111'
