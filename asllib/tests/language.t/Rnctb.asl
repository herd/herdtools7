//R_NCTB: The optional formal_list declares the formal arguments of the
//getter or setter. Each element of the formal_list of a subprogram
//declaration declares a let identifier of the given name and type in the
//scope of the subprogram body, denoting a local storage element which is
//all of the following: non-compile-time-constant, execution-time,
//immutable.

// RUN: interp %s | FileCheck %s


getter a{N:integer{1..100}}[t: bits(N)] => bits(N)
begin
    return Zeros(N);
end

setter a{N:integer}[t: bits(N)] = value: bits(N)
begin
    return;
end

func main() => integer
begin
    return 0;
end
