//R_DGBM: All value-returning standard functions behave as
//compile-time-constant unless otherwise stated.

// RUN: interp %s | FileCheck %s

constant a = Ones(10);

func main() => integer
begin
    return 0;
end
