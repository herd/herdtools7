//R_ZTRR: It is an error to use an index that is out of range.

// RUN: not interp %s | FileCheck %s

func main() => integer
begin
    var a: array[10] of integer;
    var b = a[100];
    return 0;
end
