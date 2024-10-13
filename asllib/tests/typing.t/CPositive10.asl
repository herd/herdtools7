// The value of a parameter may be negative.
func positive10(N : integer, x: bits(N+1), y: bits(N+2)) => bits(N*3+3)
begin
    return Zeros(N) :: x :: y;
end
