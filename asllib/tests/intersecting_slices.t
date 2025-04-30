One slice cannot intersect itself:
  $ cat >intersecting_slices1.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros{4};
  >   var i: integer;
  >   x[i] = '1';
  >   print (x);
  >   return 0;
  > end;
  > EOF
  $ aslref intersecting_slices1.asl
  0x1

Two intersecting slices...
  $ cat >intersecting_slices2.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros{4};
  >   let i = 0; let j = 0;
  >   x[i, j] = '10';
  >   print (x);
  >   return 0;
  > end;
  > EOF

  $ aslref intersecting_slices2.asl
  File intersecting_slices2.asl, line 5, characters 3 to 9:
    x[i, j] = '10';
     ^^^^^^
  ASL Static error: overlapping slices i+:1, j+:1.
  [1]

Two maybe intersecting slices...
  $ cat >intersecting_slices3.asl <<EOF
  > func main () => integer
  > begin
  >   var x = Zeros{4};
  >   let i = 0;
  >   var j: integer;
  >   x[i, j] = '10';
  >   print (x);
  >   return 0;
  > end;
  > EOF

  $ aslref intersecting_slices3.asl
  ASL Dynamic error: overlapping slices i+:1, j+:1.
  [1]

  $ cat>intersecting_slices3b.asl <<EOF
  > func set_unset{N}(bv: bits(N), x: integer, y: integer) => bits(N)
  >   begin
  >   var bv2 = bv;
  >   bv2[x,y] = '10'; // Should fail dynamically for x == y
  >   return bv2;
  > end;
  > func main () => integer
  > begin
  >   print (set_unset{4}('1111', 2, 3));
  >   print (set_unset{4}('1111', 2, 2));
  >   return 0;
  > end;
  > EOF

  $ aslref intersecting_slices3b.asl
  ASL Dynamic error: overlapping slices x+:1, y+:1.
  0x7
  [1]

Two intersecting bitfields
  $ cat >intersecting_slices4.asl <<EOF
  > type myty of bits (4) { [0] f1, [0] f2 };
  > func main () => integer
  > begin
  >   var x = Zeros{4} as myty;
  >   x.[f1, f2] = '10';
  >   print (x);
  >   return 0;
  > end;
  > EOF

  $ aslref intersecting_slices4.asl
  File intersecting_slices4.asl, line 5, characters 2 to 12:
    x.[f1, f2] = '10';
    ^^^^^^^^^^
  ASL Static error: overlapping slices 0+:1, 0+:1.
  [1]
