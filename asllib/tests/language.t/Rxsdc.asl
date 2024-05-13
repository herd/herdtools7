//R_XSDC: The var keyword is used to declare local variable identifiers
//denoting local storage elements which are all of the following:
//non-compile-time-constant, execution-time, mutable.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var a = 10;

    return 0;
end
