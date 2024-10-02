//I_GQMV: If expr is a compile-time-constant expression whose type is a
//well-constrained integer whose domain contains multiple values, then even
//though the width of the bitvector is a known integer value, the type of
//the bitvector is still a constrained width bitvector and not a fixed-width
//bitvector. For example,bits(4 as integer {4,8}) is a constrained width
//bitvector whose determined width is 4. (It is not clear that there is any
//practical use for this, but it is a corollary of other rules.)

// RUN: interp %s | FileCheck %s

type a of bits(4 as integer {4,8});

func main() => integer
begin
    return 0;
end
