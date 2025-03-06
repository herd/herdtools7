func print_bv{N}(bv: bits(N))
begin
  print("'");
  for i = N-1 downto 0 do
    print(if bv[i] == '0' then "0" else "1");
  end;
  print("'");
end;

func main () => integer
begin
  assert ROL('', 42) == '';
  assert ROR('', 42) == '';

  let (res_l, carry_l) = ROL_C('', 42);
  assert (res_l == '' && carry_l == '0');
  let (res_r, carry_r) = ROR_C('', 42);
  assert (res_r == '' && carry_r == '0');

  let V : bits(3) = '100';
  print("V = ");
  print_bv{3}(V);
  println("\n");

  for i = 0 to 3 do
    print("ROR(V,", i, ") = ");
    print_bv{3}(ROR(V,i));
    println();
  end;
  println();

  for i = 1 to 4 do
    print("ROR_C(V,", i, ") = (");
    let (res,carry) = ROR_C(V,i);
    print_bv{3}(res);
    print(", ");
    print_bv{1}(carry);
    println(")");
  end;
  println();

  for i = 0 to 3 do
    print("ROL(V,", i, ") = ");
    print_bv{3}(ROL(V,i));
    println();
  end;
  println();

  for i = 1 to 4 do
    print("ROL_C(V,", i, ") = (");
    let (res,carry) = ROL_C(V,i);
    print_bv{3}(res);
    print(", ");
    print_bv{1}(carry);
    println(")");
  end;
  println();

  return 0;
end;
