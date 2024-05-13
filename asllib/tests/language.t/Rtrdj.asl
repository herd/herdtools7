//R_TRDJ: The var keyword is used to declare global variable identifiers
//denoting global storage elements which are all of the following:
//non-compile-time-constant, execution-time, mutable.

// RUN: interp %s | FileCheck %s

var a: integer;

func main() => integer
begin
    return 0;
end
