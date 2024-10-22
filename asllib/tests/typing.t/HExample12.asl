// Symbolic equivalence - mixture 'N' underconstrained 'E' well-constrained
func SignedZero(sign: bit, N : integer) => bits(N)
begin
    let E = 5;
    let F = N - (E + 1);
    return sign :: Zeros(E) :: Zeros(F);
end

// Mixture here is fine - constrained + underconstrained - requires type equivalence
func Extract(offset : integer{0..32}, size: integer, reg_value : bits(128)) => bits(size * 8)
begin
    return reg_value[(offset+size) * 8 - 1:(offset) * 8];
end

// Underconstrained to Named type constraint
type myconstraint of integer{1,2,3,4};
func UnderToNamedConstraint(N : integer)
begin
    var l : myconstraint;
    l = (N+5) as myconstraint;
    let x = l;
    var y : bits(x);
end
