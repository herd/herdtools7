type my_enum of enumeration { MyEnum1, MyEnum2 };

func foo {N, M} (bv: bits(N), bv2: bits(M)) => integer {N..M, 42}
begin
  var x: integer {N..M, 42};
  return x;
end

func main () => integer
begin
  var x0: integer;
  assert x0 == 0;

  var x1: integer {3..5};
  assert x1 == 3;

  var x2: integer {-6, 16};
  assert x2 == -6;

  var x3: integer {2..5, -4..-3};
  assert x3 == 2;

  var x3_bis: integer {3..5, 2};
  assert x3_bis == 2;

  var x4: integer {2, -2};
  print ("base value of {2, -2} is", x4);

  var x4_bis: integer {-2, 2};
  print ("base value of {-2, 2} is", x4_bis);

  var x5: integer {-2..2};
  assert x5 == 0;

  var x6: integer {5..3, 42};
  assert x6 == 42;

  let x7 = foo ('00', '0000');
  assert x7 == 2;

  let x7_bis = foo ('00', '00');
  assert x7_bis == 2;

  let x7_ter = foo ('00', '0');
  assert x7_ter == 42;

  let x7_quatro = foo (Zeros(64), Zeros(128));
  assert x7_quatro == 42;

  var y: boolean;
  assert y == FALSE;

  var z: bits(3);
  assert z == '000';

  var a: string;
  assert a == "";

  var b: real;
  assert b == 0.0;

  var c: my_enum;
  assert c == MyEnum1;

  return 0;
end
