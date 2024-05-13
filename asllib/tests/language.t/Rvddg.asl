//R_VDDG: A call to the standard library Unreachable() function is a dynamic
//error.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    Unreachable();
    return 0;
end
