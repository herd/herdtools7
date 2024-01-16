// RUN: interp %s | FileCheck %s

type widTy of integer {4,8};

func callCB {N: widTy} (arg1: bits(N)) => bits(N)
begin
    // The formal argument is a constrained width bitvector
    // whose determined width N is IN {4,8}
    // For checking actual arguments, the domain of arg1 is
    // domain(bits(4)) union domain(bits(8))
    return arg1;
end

func bitvector(N: widTy)
begin
    var CB1: (bits(-: widTy), integer) = (Zeros(N), 0);
    // CB1 is a tuple whose first element is a constrained width bitvector
    // whose width is implicitly determined by the width of the bitvector
    // returned by the call to Zeros, i.e. N.
end

func main() => integer
begin
    return 0;
end
