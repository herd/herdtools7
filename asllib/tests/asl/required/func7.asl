func f0 {N} (x: bits(N)) => bits(N)
begin
  return x;
end

func f1 {M} (x: bits(M)) => integer {0..M}
begin
  return Len(x) as integer {0..M};
end

func f2 (L: integer) => bits(L)
begin
  return Zeros(L);
end

func main() => integer
begin
  let x: bits(4) = f0 ('0000');
  let y: integer {0..5} = f1 ('11111');
  let z: bits(6) = f2 (6);

  return 0;
end

// RUN: archex.sh --eval=':set asl=1.0' --eval=':set +syntax:aslv1_colon_colon' --eval=':load %s' --eval='assert main() == 0;' | FileCheck %s

