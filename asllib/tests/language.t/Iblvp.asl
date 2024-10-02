//I_BLVP: As required by rule JBXS, a parameter of a subprogram shall not be
//declared both in the parameter list and in the formal list.
//R_JBXS: If part of the type of any formal argument is a bitvector whose
//width is not a compile-time-constant, then any identifiers used in the
//bitvector’s width expression must be included in exactly one of the
//formal_list or the parameter_list and are declared as let identifiers in
//the scope of the subprogram body, denoting a local storage element which
//is all of the following: non-compile-time-constant, execution-time,
//immutable.

// RUN: not interp %s | FileCheck %s

func test{N:integer}(N:integer)
begin
    pass;
end

func main() => integer
begin
    return 0;
end
