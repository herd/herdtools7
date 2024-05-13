//R_PTDD: The formal_list declares the formal arguments of the subprogram.
//Each element of the formal_list of a subprogram declaration declares a let
//identifier of the given name and type in the scope of the subprogram body,
//denoting a local storage element which is all of the following:
//non-compile-time-constant, execution-time, immutable.


// RUN: interp %s | FileCheck %s

func test{N: integer}(a: bits(N)) => bits(N)
begin
    return a;
end

func main() => integer
begin
    return 0;
end
