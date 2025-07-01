func Reverse{N}(word : bits(N), M : integer{1..N}) => bits(N)
begin
    return Zeros{N};
end;

func HExemple17 (a: integer {8, 16, 32, 64})
begin
  if a != 64 then unreachable; end;
  let b = 32;
  let bv = Zeros{a};
  let x: bits(a) = Reverse{}(bv, b);
end;
