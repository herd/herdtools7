//R_BSQR: Configuration variables are read only once the entry point is
//called. The runtime may enforce this.

// RUN: not interp %s | FileCheck %s

config a : integer = 10;

func main() => integer
begin
    a = 5;
    return 0;
end
