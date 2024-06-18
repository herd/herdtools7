//R_JBXS: If part of the type of any formal argument is a bitvector whose
//width is not a compile-time-constant, then any identifiers used in the
//bitvector’s width expression must be included in exactly one of the
//formal_list or the parameter_list and are declared as let identifiers in
//the scope of the subprogram body, denoting a local storage element which
//is all of the following: non-compile-time-constant, execution-time,
//immutable.

// RUN: interp %s | FileCheck %s

func testa{N: integer}(a: bits(N))
begin
    pass;
end

func testb(a: bits(N), N: integer)
begin
    pass;
end

func main() => integer
begin
    return 0;
end
