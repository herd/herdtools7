//R_WZCS: The width of a bitslice must be any non-negative, statically
//evaluable integer expression (including zero).

// RUN: not interp %s | FileCheck %s

var a = '1111 1111';

func main() => integer
begin
    var b = a[1:4];
    return 0;
end
