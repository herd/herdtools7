//I_PDKT: ASL permits the declaration of subprograms with formals which are 
//parameterized such that where a formal or return type is a bitvector, its
//width may depend on the value of the subprogram parameters.

// RUN: interp %s | FileCheck %s

func test(N: integer{}) => bits(N)
begin
    return Zeros(N);
end

func main() => integer
begin
    return 0;
end
