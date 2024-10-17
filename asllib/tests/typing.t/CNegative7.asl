// value passed for i must be within constraints
func GetBitAt{N}(x: bits(N), i: integer {0..N-1}) => bits(1)
begin
    return x[i:];
end
func negative7{M}(x: bits(M)) => bits(1)
begin
  return GetBitAt(x, M); // illegal
end
