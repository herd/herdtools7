//I_MPSW: In practice, [this, viz, R_DKGQ] forbids the assignment of
//anything other than an under-constrained width bitvector of matching
//width to an under-constrained width bitvector. However, values which are
//not under-constrained width bitvectors may be used as actual arguments
//for an under-constrained width bitvector formal argument in an
//invocation, since type satisfaction is performed against the invocation
//type which is always an under-constrained width bitvector of determined
//width.

//R_DKGQ: The domain of every bitvector type other than the
//under-constrained width bitvector, is not a subset of the domain of the
//under-constrained width bitvector.

// RUN : interp %s | FileCheck %s

func f(N: integer) => bits(N)
begin
    let x: bits(N+1) = Zeros(N);
    return x;
end

func main() => integer
begin
    - = f(32);
    return 0;
end
