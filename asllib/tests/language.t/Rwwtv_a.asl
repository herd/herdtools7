//R_WWTV: It is an error to divide by zero, or by a negative integer.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    print(div_int(6, -3));
    return 0;
end