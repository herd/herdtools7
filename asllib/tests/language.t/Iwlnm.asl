//I_WLNM: There is no type for bitmasks (bitmask_lit). Bitmasks cannot be
//assigned to variables or passed into or out of functions.

// RUN: not interp %s | FileCheck %s

var a = '1xx1';

func main() => integer
begin
    return 0;
end
