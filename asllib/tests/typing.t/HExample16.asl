func Reverse{N}(word : bits(N), M : integer{1..N}) => bits(N)
begin
    return Zeros{N};
end;

func HExemple16 (a: integer {8, 16, 32, 64}, b: integer {8, 16, 32, 64})
begin
  if a < b then Unreachable(); end;
  let bv = Zeros{a};
  let -: bits(a) = Reverse{}(bv, b);
end;
