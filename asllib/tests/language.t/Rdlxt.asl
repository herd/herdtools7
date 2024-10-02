//R_DLXT: A global constant identifier is initialized before any
//non-compile-time-constant initialization expressions or subprograms are
//evaluated.

// RUN: interp %s | FileCheck %s

var a = b;
constant b = 10 + 10 + 10;

func main() => integer
begin
    return 0;
end
