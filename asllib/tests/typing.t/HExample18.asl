func Reverse{N}(word : bits(N), M : integer{1..N}) => bits(N)
begin
    return Zeros{N};
end;

func HExemple18 (a: bits(2))
begin
  if a IN {'0x'} then unreachable; end;
  let a2 = 8 << UInt(a);
  let bv = Zeros{a2};
  let b = 16;
  let x: bits(a2) = Reverse{}(bv, a2);
end;

