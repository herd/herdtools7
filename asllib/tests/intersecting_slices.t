One slice cannot intersect itself:
  $ cat >intersecting_slices1.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros(4);
  >   var i: integer;
  >   x[i:] = '1';
  >   print (x);
  >   return 0;
  > end
  > EOF
  $ aslref intersecting_slices1.asl
  '0001'

Two intersecting slices...
  $ cat >intersecting_slices2.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros(4);
  >   let i = 0; let j = 0;
  >   x[i:, j:] = '10';
  >   print (x);
  >   return 0;
  > end
  > EOF

  $ aslref intersecting_slices2.asl
  File intersecting_slices2.asl, line 5, characters 2 to 11:
  ASL Typing error: overlapping slices i+:1, j+:1.
  [1]

Two maybe intersecting slices...
  $ cat >intersecting_slices3.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros(4);
  >   let i = 0;
  >   var j: integer;
  >   x[i:, j:] = '10';
  >   print (x);
  >   return 0;
  > end
  > EOF

  $ aslref intersecting_slices3.asl
  File intersecting_slices3.asl, line 6, characters 8 to 9:
  ASL Static Error: Unsupported expression j.
  [1]

Two intersecting bitfields
  $ cat >intersecting_slices4.asl <<EOF
  > type myty of bits (4) { [0:] f1, [0:] f2 };
  > func main () => integer
  > begin
  >   var x = Zeros(4) as myty;
  >   x.(f1, f2) = '10';
  >   print (x);
  >   return 0;
  > end
  > EOF

  $ aslref intersecting_slices4.asl
  File intersecting_slices4.asl, line 5, characters 2 to 12:
  ASL Typing error: overlapping slices 0+:1, 0+:1.
  [1]
