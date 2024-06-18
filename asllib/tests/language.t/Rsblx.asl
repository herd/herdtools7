//R_SBLX: The constant keyword is used to declare global constant
//identifiers denoting global storage elements which are all of the
//following: compile-time-constant, non-execution-time, immutable.

// RUN: interp %s | FileCheck %s

constant a: integer = 10;

func main() => integer
begin
    return 0;
end
