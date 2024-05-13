//R_HWTV: When type-checking a subprogram declaration begins, the current
//type environment is initialized with the contents of the global type
//environment and also all identifiers introduced into the subprogram’s
//local scope by R_PTDD and R_JBXS.

//R_PTDD: The formal_list declares the formal arguments of the subprogram.
//Each element of the formal_list of a subprogram declaration declares a let
//identifier of the given name and type in the scope of the subprogram body,
//denoting a local storage element which is all of the following:
//non-compile-time-constant, execution-time, immutable.

//R_JBXS: If part of the type of any formal argument is a bitvector whose
//width is not a compile-time-constant, then any identifiers used in the
//bitvector’s width expression must be included in exactly one of the
//formal_list or the parameter_list and are declared as let identifiers in
//the scope of the subprogram body, denoting a local storage element which
//is all of the following: non-compile-time-constant, execution-time,
//immutable.

// RUN: interp %s | FileCheck %s

var a = 10;
var b = 10;

func c(d: integer, e: integer)
begin
    var f = a + b + d + e;
end

func main() => integer
begin
    return 0;
end
