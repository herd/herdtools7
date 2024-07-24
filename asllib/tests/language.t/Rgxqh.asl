//R_GXQH: If a getter function does not contain an argument list in the
//declaration, the getter must not be invoked with square brackets.

// RUN: not interp %s | FileCheck %s

getter a => integer
begin
    return 10;
end

func main() => integer
begin
    var b = a[];
    return 0;
end
