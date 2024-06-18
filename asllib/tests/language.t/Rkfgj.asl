//R_KFGJ: The optional parameter_list declares the parameters of the
//subprogram. ASL permits the declaration of subprograms with formals which
//are parameterized such that where a formal or return type is a bitvector,
//its width may depend on the value of the subprogram parameters.

// RUN: interp %s | FileCheck %s

func test()
begin
    pass;
end

func test(a: bits(10))
begin
    pass;
end

func main() => integer
begin
    return 0;
end
