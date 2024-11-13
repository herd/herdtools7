// Different signature underconstrained to constrained
// for creating an array in a separate function
func ArrayExample{M,N}(vector1 : bits(M), vector0 : bits(N)) => bits(M)
begin
    let E = N DIVRM 8;
    return FunctWithConstraint(vector1, E as integer{1, 2, 3, 4});
end;

func FunctWithConstraint{M}(result : bits(M), x : integer{1,2,3,4}) => bits(M)
begin
    var y : array[x] of integer;
    return result;
end;