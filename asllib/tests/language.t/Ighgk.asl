//I_GHGK: The under-constrained integer is used as the implicit type of
//unconstrained integer subprogram parameters. Intuitively it indicates that
//the parameter has a single value provided by the subprogram’s invocation
//but the range of possible values is not known when the subprogram
//declaration is type checked. This allows parameters to be used as
//bitvector widths in a subprogram when their constraint is not given in the
//subprogram declaration, without causing type checking errors. 

// RUN: interp %s | FileCheck %s

func under_constrained(a: integer{})
begin
    pass;
end

func main() => integer
begin
    return 0;
end
