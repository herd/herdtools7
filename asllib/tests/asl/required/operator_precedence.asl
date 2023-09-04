func operator_precedence( a: integer, b: integer, c: integer, d: bits(8), e: bits(8), f: bits(8), g: boolean)
begin
  let p_m_s = a * b - c;
  // '*' has higher precedence than '-' so interpreted as:
  let p_m_s_I = (a * b) - c;
  assert(p_m_s == p_m_s_I);

  let p_s_m = a - b * c;
  // '*' has higher precedence than '-' so interpreted as:
  let p_s_m_I = a - (b * c);
  assert(p_s_m == p_s_m_I);

  // let p_a_s = a + b - c;
  // '+' has equal precedence to '-' so causes a compile-time error.
  // Must be written as either:
  let p_a_s_A1 = (a + b) - c;
  let p_a_s_A2 = a + (b - c);

  // let p_s_a = a - b + c;
  // '-' has equal precedence to '+' so causes a compile-time error.
  // Must be written as either:
  let p_s_a_A1 = (a - b) + c;
  let p_s_a_A2 = a - (b + c);
  let p_a_e = a + b ^ c;

  // '^' has higher precedence than '+' so interpreted as:
  let p_a_e_I = a + (b ^ c);
  assert(p_a_e == p_a_e_I);

  let p_and_and = d AND e AND f;
  // 'AND' is associative so can be interpreted as either:
  let p_and_and_i1 = (d AND e) AND f;
  let p_and_and_i2 = d AND (e AND f);
  assert(p_and_and == p_and_and_i1);
  assert(p_and_and == p_and_and_i2);

  // let p_and_or = d AND e OR f;
  // 'AND' and 'OR' have no defined precedence so causes a compile-time error.
  // Must be written as either:
  let p_and_or_A1 = (d AND e) OR f;
  let p_and_or_A2 = d AND (e OR f);
  let p_band_eq = g && a == b;

  // '&&' is of precedence class 'Boolean'.
  // '==' is of precedence class 'Comparison'.
  // 'Comparison' has higher precedence than 'Boolean' so interpreted as:
  let p_band_eq_I = g && (a == b);
  assert(p_band_eq == p_band_eq_I);

  // let p_eq_eq = a == b == g;
  // '==' is not associative so causes a compile-time error.
  // Must be written as:
  let p_eq_eq_A1 = (a == b) == g;
  // Note: 'a == (b == g)' is not valid as it does not type satisfy.
end

func main() => integer
begin
  for i = 0 to 3 do
    for j = 0 to 3 do
      for k = 0 to 3 do
        for l = 0 to 3 do
          for m = 0 to 3 do
            for n = 0 to 3 do
              operator_precedence (i, j, k, l[7:0], m[7:0], n[7:0], TRUE);
              operator_precedence (i, j, k, l[7:0], m[7:0], n[7:0], FALSE);
            end
          end
        end
      end
    end
  end

  return 0;
end
