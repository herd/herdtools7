//R_TFJZ: The constant keyword is used to declare local constant identifiers
//denoting local storage elements which are all of the following:
//compile-time-constant, non-execution-time, immutable.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    constant a = 10;

    return 0;
end
