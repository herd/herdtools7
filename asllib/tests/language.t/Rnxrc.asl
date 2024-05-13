//R_NXRC: The expression in a throw statement must have the structure of
//exception.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    throw 10;
    return 0;
end
