//R_LLJZ: Real literals have type real.

// RUN: interp %s | FileCheck %s

var a: real = 10.0;

func main() => integer
begin
    return 0;
end
