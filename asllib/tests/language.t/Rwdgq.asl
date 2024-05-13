//R_WDGQ: Immutable values may not be assigned to or otherwise have their
//values modified after initialization.

// RUN: not interp %s | FileCheck %s

let x: integer = 10;
func main() => integer
begin
    x = 5;
    return 0;
end
